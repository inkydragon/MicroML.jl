
# for `interpret()` to print things
struct Printer end
eval(::Printer, ::Dict, args) = println(args...)

"""
    Compiler()

A `Compiler()` object.

# Property

## attribute
+ p :: Parser
+ equations
+ symtab
+ code

## method
+ compile(source::String)
+ interpret()
"""
function Compiler(; interactive=true)
    p = Parser()
    equations::Vector = []
    symtab = Dict(
        "print" => TFunc([TInt()], TInt())
    )
    code = []

    err(msg::String) = throw(MLCompilerException(msg))

    function compile(source::String)
        parsed, _ = p.parse(source, interactive)
        if parsed.name in keys(symtab)
            println("[Warning] Redefining $(parsed.name)!")
            for i in eachindex(code)
                if code[i][2].name == "main"
                    continue
                elseif code[i][2].name == parsed.name
                    pop!(code, i)
                    break
                end
            end
        end

        symtab = merge(symtab, assign_typenames(parsed.expr, symtab))
        append!(equations, generate_equations(parsed.expr))
        unifier = unify_equations(equations)
        t = get_expression_type(parsed.expr, unifier)
        symtab[parsed.name] = t
        if interactive
            println("$parsed :: $t")
        end
        
        push!(code, (unifier, parsed))

        nothing
    end

    function interpret() :: Nothing
        env = Dict{String, Any}(
            "print" => Printer()
        )
        
        for (_, node) in code
            eval(node, env)
        end
        if "main" in keys(env)
            try
                eval(env["main"], env, [])
            catch e
                throw(MLEvalException(string(e)))
            end
        end
        nothing
    end
    
    
    () -> (
        # attribute
        p, equations, symtab, code,
        # method
        compile, interpret
    )
end