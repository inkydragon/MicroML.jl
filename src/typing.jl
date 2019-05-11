# import Base: string, show, isequal

"""
    AbstractMLType <: AbstractML

## attribute
+ name :: String

## method
+ string(t::AbstractMLType)
+ isequal
"""
abstract type AbstractMLType <: AbstractML end
show(io::IOBuffer, n::AbstractMLType) = print(io, string(n))
string(t::AbstractMLType) = t.name
isequal(x::T, y::T) where T <: AbstractMLType = typeof(x) == typeof(y)

struct TInt <: AbstractMLType
    # AbstractMLType
    name :: String
    
    TInt() = new("Int")
end

struct TBool <: AbstractMLType
    # AbstractMLType
    name :: String
    
    TBool() = new("Bool")
end

struct TFunc <: AbstractMLType
    # AbstractMLType
    name :: String
    # TFunc Only
    argtypes :: Vector
    rettype
    
    TFunc(argtypes, rettype) = new("Func", argtypes, rettype)
end
string(t::TFunc) = begin
    len = length(t.argtypes)
    if len == 0
        "(-> $(t.rettype))"
    elseif len == 1
        "($(t.argtypes[1]) -> $(t.rettype))"
    else
        "(" *
        join(map(string, t.argtypes), " -> ") *
        " -> $(t.rettype))"
    end
end
isequal(x::T, y::T) where T <: TFunc = 
    (TFunc(x) == TFunc(y)) && 
    (x.rettype == y.rettype) && 
    (length(x.argtypes) == length(y.argtypes)) &&
    all([x.argtypes[i]==y.argtypes[i] for i in 1:length(x.argtypes)])

"""A type variable."""
mutable struct MLTypeVar <: AbstractMLType
    # AbstractMLType
    name
end
isequal(x::T, y::T) where T <: MLTypeVar = 
    (typeof(x) == typeof(y)) && (x.name == y.name)


err(msg::String) = throw(MLTypingException(msg))

function _type_counter()
    count = -1

    function next() :: Int
        count += 1
        count
    end

    function reset() :: Nothing
        count = -1
        nothing
    end

    () -> (count, next, reset)
end
type_counter = _type_counter()

get_fresh_typename() :: String    = "t" * string(type_counter.next())
reset_type_counter() :: Nothing   = type_counter.reset()
make_type_var()      :: MLTypeVar = MLTypeVar(get_fresh_typename())


const SymTab = Dict #{String, AbstractMLType}


"""
    assign_typenames(node, symtab::SymTab=SymTab())

Assign typenames to the given AST subtree and all its children.

Symtab is the initial symbol table we can query for identifiers found
throughout the subtree. All identifiers in the subtree must be bound either in symtab or in lambdas contained in the subtree.
This function updates the `type` property on the AST nodes it visits.
"""
function assign_typenames(node::AbstractMLASTNode, symtab::SymTab=SymTab())
    if node isa MLId
        if node.name in keys(symtab)
            node.type = symtab[node.name]
        else
            err("unbound name `$(node.name)` :: MLId")
        end
    elseif node isa MLLambda
        node.type = make_type_var()
        local_symtab = SymTab()
        for argname in node.argnames
            local_symtab[argname] = make_type_var()
        end
        node.argtypes = local_symtab
        assign_typenames(node.expr, merge(symtab, local_symtab))
    elseif node isa Union{MLOp, MLIf, MLApp}
        node.type = make_type_var()
        visit_children(node, c -> assign_typenames(c, symtab))
    elseif node isa MLInt
        node.type = TInt()
    elseif node isa MLBool
        node.type = TBool()
    else
        err("unknown node $(node) :: $(typeof(node))")
    end
    symtab
end

"""
    show_type_assignment(node)

Show a type assignment for the given subtree, as a table.
Returns a string that shows the assigmnent.
"""
function show_type_assignment(node::AbstractMLASTNode)
    lines = []

    function show_rec(node)
        s = string(node)
        push!(lines, "$s\t| $(node.type)") # make table
        visit_children(node, show_rec)
    end
    show_rec(node)
    
    join(lines, "\n")
end

"""
    TypeEquation <: AbstractML

A type equation between two types: left and right.
`original` is the original AST node from which this equation was derived, for debugging.
"""
struct TypeEquation <: AbstractML
    left
    right
    original
end
string(t::TypeEquation) = "$(t.left) :: $(t.right) [from $(t.original)]"

"""
    generate_equations(node, type_equations::Vector=[])

Generate type equations from node and place them in type_equations.
Prior to calling this functions, node and its children already have to
be annotated with _type, by a prior call to assign_typenames.
"""
function generate_equations(node::AbstractMLASTNode, type_equations::Vector=[])
    BOOL_OPS = [NEQ, EQEQ, GEQ, LEQ, GT, LT]
    
    if node isa MLInt
        push!(type_equations, TypeEquation(node.type, TInt(), node))
    elseif node isa MLBool
        push!(type_equations, TypeEquation(node.type, TBool(), node))
    elseif node isa MLId
        nothing
    elseif node isa MLOp
        visit_children(node, c -> generate_equations(c, type_equations))
        push!(type_equations, TypeEquation(node.left.type, TInt(), node))
        push!(type_equations, TypeEquation(node.right.type, TInt(), node))
        typ = (node.op in BOOL_OPS) ? TBool : TInt
        push!(type_equations, TypeEquation(node.type, typ(), node))
    elseif node isa MLApp
        visit_children(node, c -> generate_equations(c, type_equations))
        argtypes = [arg.type for arg in node.args]
        push!(type_equations, 
            TypeEquation(node.f.type, TFunc(argtypes, node.type), node)
        )
    elseif node isa MLIf
        visit_children(node, c -> generate_equations(c, type_equations))
        push!(type_equations, TypeEquation(node.ifx.type, TBool(), node))
        push!(type_equations, TypeEquation(node.type, node.thenx.type, node))
        push!(type_equations, TypeEquation(node.type, node.elsex.type, node))
    elseif node isa MLLambda
        visit_children(node, c -> generate_equations(c, type_equations))
        argtypes = [node.argtypes[name] for name in node.argnames]
        push!(type_equations, 
            TypeEquation(node.type, TFunc(argtypes, node.expr.type), node)
        )
    else
        err("unknown node $(typeof(node))")
    end
    
    type_equations
end

const Subst = Dict #{String, AbstractMLType}
Maybe(T::Type) = Union{T, Nothing}

"""
    unify(typ_x, typ_y, subst)

Unify two types typ_x and typ_y, with initial subst.
Returns a subst (map of name->Type) that unifies typ_x and typ_y, or None if they can't be unified. Pass subst={} if no subst are initially
known. Note that {} means valid (but empty) subst.
"""
function unify(
        typ_x::AbstractMLType, 
        typ_y::AbstractMLType, 
        subst::Subst
    ) :: Maybe(Subst)
    if typ_x == typ_y
        subst
    elseif typ_x isa MLTypeVar
        unify_variable(typ_x, typ_y, subst)
    elseif typ_y isa MLTypeVar
        unify_variable(typ_y, typ_x, subst)
    elseif (typ_x isa TFunc) && (typ_y isa TFunc)
        if length(typ_x.argtypes) != length(typ_y.argtypes)
            nothing
        else
            subst = unify(typ_x.rettype, typ_y.rettype, subst)
            for i in eachindex(typ_x.argtypes)
                subst = unify(typ_x.argtypes[i], typ_y.argtypes[i], subst)
            end
            subst
        end
    else
        nothing
    end
end

"""
    occurs_check(v::AbstractMLType, typ::AbstractMLType, subst::Subst) :: Bool

Does the variable v occur anywhere inside typ?
Variables in typ are looked up in subst and the check is applied recursively.
"""
function occurs_check(
        v::AbstractMLType, 
        typ::AbstractMLType, 
        subst::Subst
    ) :: Bool
    # @assert v isa MLTypeVar
    if v == typ
        true
    elseif (typ isa MLTypeVar) && (typ.name in keys(subst))
        occurs_check(v, subst[typ.name], subst)
    elseif typ isa TFunc
        (occurs_check(v, typ.rettype, subst) ||
        any([occurs_check(v, arg, subst) for arg in typ.argtypes]))
    else 
        false
    end
end

"""
    unify_variable(v::AbstractMLType, typ::AbstractMLType, subst::Subst) :: Subst

Unifies variable v with type typ, using subst.
Returns updated subst or None on failure.
"""
function unify_variable(
        v::AbstractMLType, 
        typ::AbstractMLType, 
        subst::Subst
    ) :: Maybe(Subst)
    # @assert v isa MLTypeVar
    if v.name in keys(subst)
        unify(subst[v.name], typ, subst)
    elseif (typ isa MLTypeVar) && (typ.name in keys(subst))
        unify(v, subst[typ.name], subst)
    elseif occurs_check(v, typ, subst)
        nothing
    else
        # v is not yet in subst and can't simplify x. Extend subst.
        merge(subst, SymTab(v.name => typ))
    end
end

"""
    unify_equations(eqs::Vector) :: Subst

Unifies all type equations in the sequence eqs.
Returns a substitution (most general unifier).
"""
function unify_equations(eqs::Vector) :: Subst
    subst = SymTab()
    for eq in eqs
        subst = unify(eq.left, eq.right, subst)
        if subst == nothing
            break
        end
    end
    subst
end

"""
    apply_unifier(typ::AbstractMLType, subst::Subst) :: Subst

Applies the unifier subst to typ.
Returns a type where all occurrences of variables bound in subst
were replaced (recursively); on failure returns None.
"""
function apply_unifier(
        typ::AbstractMLType, 
        subst::Subst
    ) :: Maybe(AbstractMLType)
    if length(subst) == 0
        typ
    elseif typ isa Union{TBool, TInt}
        typ
    elseif typ isa MLTypeVar
        if typ.name in keys(subst)
            apply_unifier(subst[typ.name], subst)
        else
            typ
        end
    elseif typ isa TFunc
        newargtypes = [apply_unifier(arg, subst) for arg in typ.argtypes]
        TFunc(newargtypes, apply_unifier(typ.rettype, subst))
    else
        nothing
    end
end

"""
    get_expression_type(
        expr, 
        subst::Subst,
        rename_types::Bool=true
    ) :: AbstractMLType

Finds the type of the expression given a substitution.

If rename_types is True, renames all the type vars to be sequential
characters starting from 'a', so that 't5 -> t3' will be renamed to
'a -> b'. These names are less cluttery and also facilitate testing.

Note: expr should already be annotated with assign_typenames.
"""
function get_expression_type(
        expr, 
        subst::Subst,
        rename_types::Bool=true
    ) :: AbstractMLType
    typ = apply_unifier(expr.type, subst)
    namecounter = _type_counter()
    get_counter() = namecounter.next()
    namemap = Dict()
    
    function rename_type(typ)
        if typ isa MLTypeVar
            if typ.name in keys(namemap)
                typ.name = namemap[typ.name]
            else
                name = Char(Int('a') + get_counter())
                namemap[typ.name] = name
                namemap[name] = name
                typ.name = namemap[typ.name]
            end
        elseif typ isa TFunc
            rename_type(typ.rettype)
            for argtyp in typ.argtypes
                rename_type(argtyp)
            end
        end
    end
    if rename_types
        rename_type(typ)
    end
    typ
end
