# AST nodes for micro-ML.
# import Base: string, show, parse 

"""
    AbstractML

Top type of MicroML
"""
abstract type AbstractML end

"""
    AbstractMLASTNode <: AbstractML

## attribute
+ type      :: String
    Used by the type inference algorithm.
+ children  :: Vector
    Used by passes that traverse the AST. Each concrete node class lists the sub-nodes it has as children.

## method
+ visit_children(n::AbstractMLASTNode, f::Function)
+ show(io::IOBuffer, n::AbstractMLASTNode)
"""
abstract type AbstractMLASTNode <: AbstractML end
"Visit all children with a function that takes a child node."
visit_children(n::AbstractMLASTNode, f::Function) = map(f, n.children)
show(io::IOBuffer, n::AbstractMLASTNode) = print(io, string(n))

"""
    AbstractMLVal <: AbstractMLASTNode

## attribute
+ type      :: String
+ children  :: Vector
+ value     :: String

## method
+ string(n::AbstractMLVal)
+ compile(n::AbstractMLVal, unifier=nothing)
"""
abstract type AbstractMLVal <: AbstractMLASTNode end
string(n::AbstractMLVal) = string(n.value)
compile(n::AbstractMLVal, unifier=nothing) = string(parse(Int, n.value))
# setproperty!(n::AbstractMLVal, name::Symbol, x) = 

parse(::Type{Int64}, i::Integer) = i
parse(::Type{Int64}, b::Bool) = b ? 1 : 0
parse(::Type{Bool}, b::Bool) = b


#=
    concrete node type
=#

"Compile ML code to c-lang."
function compile end
"Eval ML code."
function eval end

"""Int Constant"""
mutable struct MLInt <: AbstractMLVal
    # AbstractMLASTNode
    type        # :: AbstractMLType # depend on typing
    children    :: Vector
    # AbstractMLVal
    value       :: String
        
    MLInt(val::String) = new("tInt", [], val)
end
eval(n::MLInt, env::Dict=Dict()) = parse(Int, n.value)

"""Bool Constant"""
mutable struct MLBool <: AbstractMLVal
    # AbstractMLASTNode
    type        # :: AbstractMLType
    children    :: Vector
    # AbstractMLVal
    value       :: String
    
    MLBool(val::String) = new("tBool", [], val)
end
eval(n::MLBool, env::Dict=Dict()) = parse(Bool, n.value)

"""Identifier"""
mutable struct MLId <: AbstractMLASTNode
    # AbstractMLASTNode
    type        # :: AbstractMLType
    children    :: Vector  
    # Id only
    name        :: String
    
    MLId(name::String) = new("tID", [], name)
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

"""Binary operation between expressions."""
mutable struct MLOp <: AbstractMLASTNode
    # AbstractMLASTNode
    type        # :: AbstractMLType
    children    :: Vector
    # Op only
    op      :: String
    left    :: AbstractMLASTNode
    right   :: AbstractMLASTNode
    MLOp(op, left, right) = new("tOp", [left, right], op, left, right)
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

"""
Application of a function to a sequence of arguments.
func is a node, args is a sequence of nodes.
"""
mutable struct MLApp <: AbstractMLASTNode
    # AbstractMLASTNode
    type        # :: AbstractMLType
    children    :: Vector
    # App only
    f
    args
    
    MLApp(f, args=()) = new("tApp", [f, args...], f, args)
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

"""`if ... then ... else ...` expression."""
mutable struct MLIf <: AbstractMLASTNode
    # AbstractMLASTNode
    type        # :: AbstractMLType
    children    :: Vector
    # App only
    ifx
    thenx
    elsex
    
    MLIf(ifx, thenx, elsex) = 
        new("tIf", [ifx, thenx, elsex], ifx, thenx, elsex)
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

"""lambda [args] -> expr"""
mutable struct MLLambda <: AbstractMLASTNode
    # AbstractMLASTNode
    type        # :: AbstractMLType
    children    :: Vector
    # Lambda only
    argnames
    argtypes    :: Dict
    expr # :: MLToken ???
    
    MLLambda(argnames, expr) = new("tLambda", [expr], argnames, Dict(), expr)
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
    new_env = copy(env)
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

mutable struct MLDecl <: AbstractMLASTNode
    # AbstractMLASTNode
    type        # :: AbstractMLType
    children    :: Vector
    # Lambda only
    name :: String
    expr
    
    MLDecl(name, expr) = new("tDecl", [expr], name, expr)
end
string(n::MLDecl) = "$(n.name) = $(n.expr)"
compile(n::MLDecl, unifier) = begin
    @show (n.expr,) typeof(n.expr)
    typ = unifier(n.expr) |> to_c
    if n.expr isa MLLambda
        "$typ $(n.name)$(compile(n.expr, unifier))"
    else
        "$typ $(n.name) = $(compile(n.expr, unifier));"
    end
end
eval(n::MLDecl, env::Dict) = env[n.name] = n.expr
