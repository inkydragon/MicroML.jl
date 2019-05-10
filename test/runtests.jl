# using .MicroML
using Test
include("../src/MicroML.jl")
using .MicroML: Lexer, MLToken, Parser,
    assign_typenames, show_type_assignment, 
    generate_equations, unify_equations, get_expression_type,
    type_counter, reset_type_counter


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

function parse_code(code::String)
    parsed_expr, _ = Parser().parse(code, true)
    parsed_expr
end

@testset "Parser" begin
    function ptest(code, result)
        parsed = parse_code(code)
        parsed |> string == result
    end
    function rtestset(testset)
        for (code, result) in testset
            @test ptest(code, result)
        end
    end
    function ralltest(set)
        for (name, testset) in set
            @testset "$name" begin
                rtestset(testset)
            end
        end
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
    
    ALL_TEST_SET = [
        ("basic decls",
            [
                ("foo           x =  2", 
                 "foo = (lambda x -> 2)"),
                ("foo           x =  false", 
                 "foo = (lambda x -> false)"),
                ("foo           x =  joe",
                 "foo = (lambda x -> joe)"),
                ("foo           x =  (joe)",
                 "foo = (lambda x -> joe)"),
            ]
        ),
        ("Parser functions",
            [
                ("(* basic ifexpr *) foo x = if y then z else q", 
                 "foo = (lambda x -> (if y then z else q))"),
                ("(* basic op *)    bar z y = z + y", 
                 "bar = (lambda z, y -> (z + y))"),
                ("(* basic proc *)  bar z y = lambda f -> z + (y * f)",
                 "bar = (lambda z, y -> (lambda f -> (z + (y * f))))"),
            ]
        ),
        ("(* basic app *)",
            [
                ("foo           x =  gob(x)",
                 "foo = (lambda x -> gob(x))"),
                ("foo           x =  bob(x, true)",
                 "foo = (lambda x -> bob(x, true))"),
                ("foo           x =  bob(x, max(10), true)",
                 "foo = (lambda x -> bob(x, max(10), true))"),
            ]
        ),
        ("full exprs", 
            [
                ("bar =  if  ((t + p) * v) > 0  then x else f(y)", 
                 "bar = (if (((t + p) * v) > 0) then x else f(y))"),
                ("bar = joe(moe(doe(false)))", 
                 "bar = joe(moe(doe(false)))"), 
                ("cake =  lambda f ->  lambda x ->  f(3) - f(x)",
                 "cake = (lambda f -> (lambda x -> (f(3) - f(x))))"),
                ("cake =  lambda f  x ->  f(3) - f(x)",
                 "cake = (lambda f, x -> (f(3) - f(x)))"),
                ("x           y  z =   if  y < z  then  y * z  else  y / z",
                 "x = (lambda y, z -> (if (y < z) then (y * z) else (y / z)))"),
                ("main =  lambda  -> print(x(1, 2))",
                 "main = (lambda  -> print(x(1, 2)))"),
                ("x           y  z =   y + z",
                 "x = (lambda y, z -> (y + z))"),
            ]
        ),
    ]
    ralltest(ALL_TEST_SET)
end

@testset "Typing" begin
    # code = "foo f g x = if f(x == 1) then g(x) else 20"
    # parser = Parser()
    # parsed, _ = parser.parse(code, true)
    # println("Code\n------")
    # parsed |> string |> println
    # 
    # assign_typenames(parsed.expr)
    # println("\nParsed AST\n------")
    # show_type_assignment(parsed.expr) |> println
    # 
    # equations = []
    # generate_equations(parsed.expr, equations)
    # println("\nTypename assignment\n------")
    # for eq in equations
    #     println("$eq")
    # end
    # 
    # unifier = unify_equations(equations)
    # println("\nInferred type\n------")
    # get_expression_type(parsed.expr, unifier) |> string |> println
    
    @testset "Assign Typenames" begin
        reset_type_counter()
        e = parse_code("foo f x = f(3) - f(x)")
        assign_typenames(e.expr)

        @test e.expr.expr.left.f.type.name == "t1"
        @test e.expr.expr.right.f.type.name == "t1"
        @test e.expr.expr.right.args[1].type.name == "t2"
        @test e.expr.expr.type.name == "t3"
        @test e.expr.expr.left.type.name == "t4"
        @test e.expr.expr.right.type.name == "t5"
    end
    
    @testset "Generate Equations" begin
        reset_type_counter()
        """
        Assert that the list of equations eqs has a left=right equation.
        left and right are given in string representations.
        """
        has_equation(eqs, left, right) =
            any([string(e.left)==left && string(e.right)==right for e in eqs])
        
        e = parse_code("foo f x = f(3) - f(x)")
        equations = []
        assign_typenames(e.expr)
        generate_equations(e.expr, equations)
        
        @test has_equation(equations, "t1", "(Int -> t4)")
        @test has_equation(equations, "t1", "(t2 -> t5)")
    end
    
    @testset "Full Inference" begin
        reset_type_counter()
        
        """Assert that the type of the declaration is inferred to ty."""
        function assertInferredType(decl, ty)
            e = parse_code(decl)
            assign_typenames(e.expr)
            equations = []
            generate_equations(e.expr, equations)
            unifier = unify_equations(equations)
            inferred = get_expression_type(e.expr, unifier)
            
            @test string(inferred) == ty
        end
        
        @testset "simple" begin
            assertInferredType("foo = 9", "Int")
            assertInferredType("foo = false", "Bool")
            assertInferredType("foo x = 9", "(a -> Int)")
            assertInferredType("foo x = true", "(a -> Bool)")
            assertInferredType("foo x y = x + y", "(Int -> Int -> Int)")
            assertInferredType("foo x y = x == y", "(Int -> Int -> Bool)")
        end
        
        @testset "if" begin
            assertInferredType(
              "foo x y = if x > y then 10 else 20",
              "(Int -> Int -> Int)")
            assertInferredType(
              "foo x = if x then 10 else 20",
              "(Bool -> Int)")
            assertInferredType(
              "foo x = if x then x else x",
              "(Bool -> Bool)")
        end
        
        @testset "full" begin
            assertInferredType(
              "foo f = f(11)",
              "((Int -> a) -> a)")
            assertInferredType(
              "foo g h = g(h(0))",
              "((b -> a) -> (Int -> b) -> a)")
            assertInferredType(
              "foo f g x = f(g(3 + x))",
              "((b -> a) -> (Int -> b) -> Int -> a)")
            assertInferredType(
              "foo f x = f(3) - f(x)",
              "((Int -> Int) -> Int -> Int)")
            assertInferredType(
              "foo f g x = if f(x) then g(x) else 20",
              "((a -> Bool) -> (a -> Int) -> a -> Int)")
            assertInferredType(
              "foo f g x = if f(x == 1) then g(x) else 20",
              "((Bool -> Bool) -> (Int -> Int) -> Int -> Int)")
            assertInferredType(
              "foo f = lambda t -> f(t)",
              "((b -> a) -> (b -> a))")
            assertInferredType(
              "foo f x = if x then lambda t -> f(t) else lambda j -> f(x)",
              "((Bool -> a) -> Bool -> (Bool -> a))")
        end
    end
end

end