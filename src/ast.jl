import Base: string, show, eval, parse 

abstract type AbstractML end
abstract type AbstractMLNode <: AbstractML end
abstract type AbstractMLVal  <: AbstractMLNode end

"""
    Compile ML code to c-lang.
"""
function compile end

"""
    Eval ML code.
"""
# function eval end


struct MLNode <: AbstractMLNode
    # AbstractMLNode
    type
    children :: Vector
end
visit_children(n::AbstractMLNode, f::Function) = map(f, n.children)
show(io::IOBuffer, n::AbstractMLNode) = print(io, string(n))

struct MLVal <: AbstractMLVal
    # AbstractMLNode
    type
    children :: Vector
    
    # AbstractMLVal
    value
    
    MLVal(val) = new("Val", [], val)
end
string(n::AbstractMLVal) = string(n.value)
compile(n::AbstractMLVal, unifier=nothing) = string(parse(Int, n.value))
parse(::Type{Int64}, i::Integer) = i
parse(::Type{Int64}, b::Bool) = b ? 1 : 0
parse(::Type{Bool}, b::Bool) = b

struct MLInt <: AbstractMLVal
    # AbstractMLNode
    type
    children :: Vector
    
    # AbstractMLVal
    value
        
    MLInt(val) = new("MLInt", [], val)
end
eval(n::MLInt, env::Dict=Dict()) =  parse(Int, n.value)


struct MLBool <: AbstractMLVal
    # AbstractMLNode
    type
    children :: Vector
    
    # AbstractMLVal
    value
    
    MLBool(val) = new("MLBool", [], val)
end
eval(n::MLBool, env::Dict=Dict()) = parse(Bool, n.value)


struct MLId <: AbstractMLNode
    # AbstractMLNode
    type
    children :: Vector
    
    # Id only
    name
    
    MLId(name::String) = new("ID", [], name)
end
string(n::MLId) = string(n.name)
compile(n::MLId, unifier=nothing) = n.name
eval(n::MLId, env::Dict) = env[n.name]


const OPERATORS = Dict(
    "+" => +,
    "-" => -,
    "*" => *,
    "/" => /,
    "<"  => <,
    "<=" => <=,
    ">"  => >,
    ">=" => >=,
    "==" => ==,
)

struct MLOp <: AbstractMLNode
    # AbstractMLNode
    type
    children :: Vector
    
    # Op only
    op      :: String
    left    :: AbstractMLNode
    right   :: AbstractMLNode
    MLOp(op, left, right) = new("Op", [left, right], op, left, right)
end
string(n::MLOp) = "($(n.left) $(n.op) $(n.right))"
compile(n::MLOp, unifier) = 
    "(" * 
    compile(n.left, unifier) * 
    " $(n.op) " *
    compile(n.right, unifier) * 
    ")"
find_op(n::MLOp) :: Function = OPERATORS[n.op]
eval(n::MLOp, env::Dict) = 
    find_op(n)(eval(n.left, env), eval(n.right, env))


struct MLApp <: AbstractMLNode
    # AbstractMLNode
    type
    children :: Vector
    
    # App only
    f
    args    :: Tuple
    
    MLApp(f, args::Tuple=()) = new("App", [f, args], f, args)
    MLApp(f, args::Vector) = MLApp(f, Tuple(args))
end
string(n::MLApp) = 
    "$(n.f)(" * join([string(a) for a in n.args], ", ") * ")"
compile(n::MLApp, unifier) = 
    "$(n.f)(" * 
    join([compile(a, unifier) for a in n.args], ", ") * 
    ")"
eval(n::MLApp, env::Dict) = begin
    f = eval(n.f, env)
    eval(f, env, [eval(arg, env) for arg in n.args])
end


struct MLIf <: AbstractMLNode
    # AbstractMLNode
    type
    children :: Vector
    
    # App only
    ifx
    thenx
    elsex
    
    MLIf(ifx, thenx, elsex) = 
        new("If", [ifx, thenx, elsex], ifx, thenx, elsex)
end
string(n::MLIf) = 
    "(if $(n.ifx) then $(n.thenx) else $(n.elsex))"
compile(n::MLIf, unifier) = 
    compile(n.ifx, unifier) *
    " ? " *
    compile(n.thenx, unifier) *
    " : " *
    compile(n.elsex, unifier)
eval(n::MLIf, env::Dict) = 
    eval(n.ifx, env) ? eval(n.thenx, env) : n


struct MLLambda <: AbstractMLNode
    # AbstractMLNode
    type
    children :: Vector
    
    # Lambda only
    argnames
    argtypes
    expr # :: MLToken ???
    
    MLLambda(argnames, expr) = new("lambda", [expr], argnames, Dict(), expr)
end
string(n::MLLambda) = 
    "(lambda " * 
    join(n.argnames, ", ") * 
    " -> $(n.expr))"
compile(n::MLLambda, unifier) = begin
    typ = n.expr.type |> unifier |> to_c
    compiled = compile(n.expr, unifier)
    body = "return $compiled;"
    
    "(" *
    join(
        [
            unifier(n.argtypes[name]) |> to_c * 
            " $name" for name in n.argnames
        ], 
        ", "
    ) *
    ") {\n" *
    join(["  $l" for l in split(body, "\n")], "\n") *
    "\n}"
end
eval(n::MLLambda, env::Dict, args) = begin
    new_env = Dict(env)
    l_args = length(args)
    l_argnames = length(n.argnames)
    if l_args != l_argnames
        throw(MLEvalException(
            "lambda was called with $l_args arguments, but expected $l_argnames."
        ))
    end
    for i in eachindex(args)
         new_env[n.argnames[i]] = args[i]
    end
    eval(n.expr, new_env)
end


struct MLDecl <: AbstractMLNode
    # AbstractMLNode
    type
    children :: Vector
    
    # Lambda only
    name :: String
    expr
    
    MLDecl(name, expr) = new("Decl", [expr], name, expr)
end
string(n::MLDecl) = "$(n.name) = $(n.expr)"
compile(n::MLDecl, unifier) = begin
    typ = unifier(n.expr.type) |> to_c
    if n.expr <: MLLambda
        "$typ $(n.name)$(compile(n.expr, unifier))"
    else
        "$typ $(n.name) = $(compile(n.expr, unifier));"
    end
end
eval(n::MLDecl, env::Dict) = env[n.name] = n.expr
