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
"""
abstract type AbstractMLVal <: AbstractMLASTNode end
string(n::AbstractMLVal) = string(n.value)

parse(::Type{Int64}, i::Integer) = i
parse(::Type{Int64}, b::Bool) = b ? 1 : 0
parse(::Type{Bool}, b::Bool) = b


#=
    concrete node type
=#

"Eval ML code."
function evaluate end

"""Int Constant"""
mutable struct MLInt <: AbstractMLVal
    # AbstractMLASTNode
    type        # :: AbstractMLType # depend on typing
    children    :: Vector
    # AbstractMLVal
    value       :: String
        
    MLInt(val::String) = new("tInt", [], val)
end
evaluate(n::MLInt, env::Dict=Dict()) = parse(Int, n.value)

"""Bool Constant"""
mutable struct MLBool <: AbstractMLVal
    # AbstractMLASTNode
    type        # :: AbstractMLType
    children    :: Vector
    # AbstractMLVal
    value       :: String
    
    MLBool(val::String) = new("tBool", [], val)
end
evaluate(n::MLBool, env::Dict=Dict()) = parse(Bool, n.value)

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
evaluate(n::MLId, env::Dict) = env[n.name]


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
find_op(n::MLOp) :: Function = OPERATORS[n.op]
evaluate(n::MLOp, env::Dict) = 
    find_op(n)(evaluate(n.left, env), evaluate(n.right, env))

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
evaluate(n::MLApp, env::Dict) = begin
    f = evaluate(n.f, env)
    evaluate(f, env, [evaluate(arg, env) for arg in n.args])
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
evaluate(n::MLIf, env::Dict) = 
    evaluate(n.ifx, env) ? evaluate(n.thenx, env) : n

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
evaluate(n::MLLambda, env::Dict, args) = begin
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
    evaluate(n.expr, new_env)
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
evaluate(n::MLDecl, env::Dict) = env[n.name] = n.expr
