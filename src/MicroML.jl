module MicroML
import Base: string, show, parse, match, isequal

include("exceptions.jl") # all others depends on `exceptions`
include("ast.jl")
include("lexer.jl")     # depends on `ast`
include("parser.jl")    # depends on `ast, lexer`
include("typing.jl")    # depends on `ast, lexer`
include("compiler.jl")

export MLToken, Lexer, # lexer
    Parser, # parser
    # typing
    assign_typenames, show_type_assignment, generate_equations,
    unify_equations, get_expression_type,
    type_counter, reset_type_counter,
    Compiler # Compiler


end  # module MicroML