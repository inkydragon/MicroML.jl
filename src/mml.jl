# commandline sctipt
include("MicroML.jl")
using .MicroML: Compiler, MLException
include("repl.jl")


const CMD_HELP = """
Usage: 
    julia mml.jl            # will open REPL
    julia mml.jl [FileName] # exec code in a file

In REPL you may use those keywords.

    [:q|quit]        quit REPL
    [:i|interpret]   interpret code
    [:e|execute]     execute code
"""

rerr(msg) = println("[runtime error] $msg")

function main()
    len = length(ARGS)
    if len == 0     # no args
        repl()
    elseif len == 1 # one args
        @warn "Not test, may have many bugs."
        c = Compiler(interactive=False)
        contents = ""
        
        try # tring to read file
            open(ARGS[1], "w") do f
                 contents = read(f, String)
            end;  
        catch SystemError
            println(CMD_HELP)
        end

        c.compile(contents)
        c.execute()
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
