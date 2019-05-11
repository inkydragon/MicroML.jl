# commandline sctipt
include("MicroML.jl")
using .MicroML: Compiler, MLException
include("repl.jl")


const CMD_HELP = """
Usage: 
    julia mml.jl            # will open REPL

In REPL you may use those keywords.

    [:q|quit]        quit REPL
    [:i|interpret]   interpret code
"""

rerr(msg) = println("[runtime error] $msg")

function main()
    if length(ARGS) == 0     # no args
        repl()
    else
        rerr("Too many args! $ARGS")
        println(CMD_HELP)
    end
end

try
    main()
catch e
    rerr(e)
end
