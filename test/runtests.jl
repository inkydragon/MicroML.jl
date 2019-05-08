# using .MicroML
using Test

include("../src/exceptions.jl") # all others depends on `exceptions`
include("../src/lexer.jl")
include("../src/ast.jl")
include("../src/parser.jl")    # depends on `ast, lexer`
# include("../src/typing.jl")    # depends on `ast, lexer`
# include("../src/compiler.jl")  # depends on `parser, typing`
# include("../src/repl.jl")      # depends on `compiler`

@testset "MicroML" begin

@testset "Lexer" begin
    function ltest(code, result)
        lexer = Lexer()
        lexer.start(code)
        lexer.tokens() == result
    end
    
    code1 = """
    (* simple add *)
    x y z = y + z
        
    (*
      we need a main function
    *)
    main = lambda -> print(x(1, 2))
    """
    tokens1 = [
        MLToken("ID", "x", 18), 
        MLToken("ID", "y", 20), 
        MLToken("ID", "z", 22), 
        MLToken("=", "=", 24), 
        MLToken("ID", "y", 26), 
        MLToken("+", "+", 28), 
        MLToken("ID", "z", 30), 
        MLToken("ID", "main", 69), 
        MLToken("=", "=", 74), 
        MLToken("LAMBDA", "lambda", 76), 
        MLToken("ARROW", "->", 83), 
        MLToken("ID", "print", 86), 
        MLToken("(", "(", 91), 
        MLToken("ID", "x", 92), 
        MLToken("(", "(", 93), 
        MLToken("INT", "1", 94), 
        MLToken(",", ",", 95), 
        MLToken("INT", "2", 97), 
        MLToken(")", ")", 98), 
        MLToken(")", ")", 99)
    ]
    
    @test ltest(code1, tokens1)
end

@testset "Parser" begin
    function ptest(code, result)
        parser = Parser()
        parsed, _ = parser.parse(code, true)
        parsed |> string == result
    end
    
    code1   = "x           y  z =   if  y < z  then  y * z  else  y / z"
    result1 = "x = (lambda y, z -> (if (y < z) then (y * z) else (y / z)))"
    code2   = "main =  lambda  -> print(x(1, 2))"
    result2 = "main = (lambda  -> print(x(1, 2)))"
    code3   = "x           y  z =   y + z"
    result3 = "x = (lambda y, z -> (y + z))"
   
    @test ptest(code1, result1)
    @test ptest(code2, result2)
    @test ptest(code3, result3)
end

end