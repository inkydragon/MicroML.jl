
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
+ main

## method
+ compile(source::String)
+ interpret()
+ execute()
+ genc()
"""
function Compiler(; interactive=true)
    p = Parser()
    equations::Vector = []
    symtab = Dict(
        "print" => TFunc([TInt()], TInt())
    )
    code = []
    main::Int = 0

    err(msg::String) = throw(MLCompilerException(msg))

    function compile(source::String)
        parsed, _ = p.parse(source, interactive)

        if parsed.name in keys(symtab)
            println("[Warning] Redefining $(parsed.name)!")
            main_found = false
            for i in eachindex(code)
                if code[i][2].name == "main"
                    main_found = true
                elseif code[i][2].name == parsed.name
                    pop!(code, i)
                    if !main_found
                        main -= 1
                    end
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
        
        if parsed.name == "main"
            main = length(code)
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
    
    "Generate c code"
    function genc() :: String
        @warn "Not test, may have many bugs."
        if code == []
            err("Nothing to execute!")
        elseif main == 0
            err("No `main` function specified!")
        end
        
        PRELUDE = """
        #include <stdio.h>

        int print(int in) {
            printf("%d\\n", in);
            return 0;
        }
        """
        get_type(unifier) = x -> get_expression_type(x, unifier)
        
        main_unifier, main_node = code[main]
        
        # compiled = "{}\n{}\n{}".format(
        #     PRELUDE,
        #     "\n".join(
        #         node.compile(get_type(unifier))
        #         for (unifier, node) in code
        #         if node.name != "main"
        #     ),
        #     main_node.compile(get_type(main_unifier))
        # )
        PRELUDE * "\n" *
        join(
            [
                (node.name=="main") ? 
                    "" : 
                    compile(node, get_type(unifier)) for 
                    (unifier, node) in code
            ], 
            "\n"
        ) * "\n" *
        compile(main_node, get_type(main_unifier))
    end
    
    """
        execute()
    
    Generate c code, compile with `CC` and print output.
    """
    function execute()
        compiled = genc()
        
        i = tempname() * ".c"
        o = tempname() * ".o"
        open("$i", "w+") do f
            write(f, compiled)
        end
        
        env = copy(ENV)
        cc = getkey(env, "CC", "gcc")
        
        # compile c code using `CC`
        try
            cmd1 = Cmd(`$cc $i -o $o`; windows_hide=false)
            r = run(cmd1)
        catch e
            err(string(e))
        end
        
        # execute binary file
        try
            out = IOBuffer()
            err = IOBuffer()
            cmd2 = pipeline(`$o`; stdout=out, stderr=err)
            r = run(cmd2)
            out = String(take!(out))
            println(out)
        catch e
            code = r.exitcode
            err = String(take!(err))
            println("[program output][stderr] $err")
            if code < 0
                err("Running the executable failed with signal $(r.termsignal)!")
            end
            err("Running the executable failed with exit code: $code!")
        end
    end

    
    () -> (
        # attribute
        p, equations, symtab, code, main,
        # method
        compile, interpret, execute, genc
    )
end