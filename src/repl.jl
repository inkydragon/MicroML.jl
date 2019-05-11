
# # copy from util.jl:503~513
# function prompt(input::IO, output::IO, message::AbstractString; default::AbstractString="")
#     msg = isempty(default) ? "$message" : "$message[$default]" # changed here
#     print(output, msg)
#     uinput = readline(input, keep=true)
#     isempty(uinput) && return nothing
#     uinput = chomp(uinput)
#     isempty(uinput) ? default : uinput
# end
# prompt(message::AbstractString; default::AbstractString="") = prompt(stdin, stdout, message, default=default)

using Juno: input # TODO: remove depend on Juno

"""
    repl()

A `repl()` for MicroML.
"""
function repl()
    c = Compiler()
    
    err(e::MLException) = println("[ML error] $(e.mod): $(e.msg)")
    err(e::Exception) = println(e)
    
    while true
        line = nothing
        try
            # line = prompt("μML> ") # using prompt may caused MethodError
            line = input("μML> ") 
        catch e # TODO: catch only EOFError & KeyboardInterrupt
            err(e)
            print("\nMoriturus te saluto!")
            break
        end

        if isempty(line)
            println()
            continue
        end

        if line in [":q", "quit"]
            println("Moriturus te saluto!")
            break
        end

        if line in [":i", "interpret"]
            try
                c.interpret()
            catch e
                err(e)
            finally
                continue
            end
        end

        if line in [":e", "execute"]
            @warn "Not test, may have many bugs."
            try
                c.execute()
            catch e
                err(e)
            finally
                continue
            end
        end
        
        try
            c.compile(line)
        catch e
            err(e)
        end
    end
end
