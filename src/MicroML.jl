module MicroML
import Base: string, show, parse, match, isequal

include("exceptions.jl") # all others depends on `exceptions`
include("ast.jl")
include("lexer.jl")     # depends on `ast`
include("parser.jl")    # depends on `ast, lexer`
include("typing.jl")    # depends on `ast, lexer`
# include("compiler.jl")  # depends on `parser, typing`
# include("repl.jl")      # depends on `compiler`

export Compiler

end  # module MicroML