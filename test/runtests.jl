# using .MicroML
using Test
include("../src/MicroML.jl")
using .MicroML: Lexer, MLToken, Parser


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
    code2 = "erw = _abc + 12*(R4==623902)  "
    tokens2 = [
        MLToken("ID", "erw", 1)
        MLToken("=", "=", 5)
        MLToken("ID", "_abc", 7)
        MLToken("+", "+", 12)
        MLToken("INT", "12", 14)
        MLToken("*", "*", 16)
        MLToken("(", "(", 17)
        MLToken("ID", "R4", 18)
        MLToken("==", "==", 20)
        MLToken("INT", "623902", 22)
        MLToken(")", ")", 28)
    ]
    
    @test ltest(code1, tokens1)
    @test ltest(code2, tokens2)
end

@testset "Parser" begin
    function ptest(code, result)
        parser = Parser()
        parsed, _ = parser.parse(code, true)
        parsed |> string == result
    end
    
    @testset "basic decls" begin
        testset = [
            ("foo x = 2", ),
            ("foo x = false", ),
            ("foo x = joe",),
            ("foo x = (joe)",),
        ]
    end
    
    @testset "parse multiple" begin
        parser = Parser()
        code1 = "foo x = 10"
        rse1  = "foo = (lambda x -> 10)"
        parsed1, _ = parser.parse(code1, true)
        @test parsed1 |> string == rse1
        
        code2 = "foo y = true"
        rse2  = "foo = (lambda y -> true)"
        parsed2, _ = parser.parse(code2, true)
        @test parsed2 |> string == rse2
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