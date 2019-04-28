using Test

include("../src/lexer.jl")

const code1 = """
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

@testset "Test Lexer" begin
    lexer = Lexer()
    lexer.start(code1)
    
    @test lexer.peek() == MLToken("ID", "x", 18)

    tks = lexer.tokens()
    @test tks == tokens1
end

lexer = Lexer()
lexer.start(code1)

