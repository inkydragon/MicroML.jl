import Base: parse, match

"""
Parser()

A `Parser()` object.

# Property

## attribute
+ lexer
+ token :: MLToken

## method
+ parse(source::String, should_terminate::Bool=True)
"""
function Parser()
    lexer = Lexer()
    token :: MLToken = MLTokenNone
    OPERATORS = [
        NEQ,    EQEQ,   GEQ,    LEQ,    LT,     GT,
        PLUS,   MINUS,  TIMES,  DIV
    ]
    should_terminate = true
    
    function parse(source::String, should_terminate::Bool)
        lexer.start(source)
        next()
        
        decl = decl()
        if token != MLTokenNone && should_terminate
            println(source)
            println(" "^(token.pos-1) * "^~~~")
            err("Unexpected token `$(token.val)` at $(token.pos)") 
        end
        
        decl, token.pos
    end
    
    err(msg::String) = throw(MLParserException(msg))
    next() = begin token = lexer.token(); end
    
    function match(typ::String) :: String
        tok_type = token.type
        if tok_type == typ
            val = token.val
            next()
    
            val
        else
            err("Expected $(typ), but found $(tok_type) at $(token.pos)")
        end
    end
    
    function decl()
        name = match(ID)
        argnames = []
    
        while token.type == ID
            push!(argnames, token.val)
            next()
        end
    
        match(EQ)
        expr = expr()
    
        if isempty(argnames)
            MLDecl(name, expr)
        else
            MLDecl(name, MLLambda(argnames, expr))
        end
    end
    
    function expr()
        node = expr_component()
        if token.type in OPERATORS
            op = token.type
            next()
            rhs = expr_component()
    
            MLOp(op, node, rhs)
        else
            node 
        end
    end
    
    function expr_component()
        _token = token
        _typ = _token.type
    
        if _typ == INT
            next()
            MLInt(_token.val)
        elseif _typ in [FALSE, TRUE]
            next()
            MLBool(_typ == TRUE)
        elseif _typ == ID
            next()
            if token.type == LPAREN
                app(_token.val)
            else
                MLId(_token.val)
            end
        elseif _typ == LPAREN
            next()
            expr = expr()
            match(RPAREN)
            expr
        elseif _typ == IF
            ifexpr()
        elseif _typ == LAMBDA
            lambdaexpr()
        else
            err("We don’t support $(_typ) yet!")
        end
    end
    
    function ifexpr()
        match(IF)
        _ifexpr = expr()
        match(THEN)
        _thenexpr = expr()
        match(ELSE)
        _elseexpr = expr()
    
        MLIf(_ifexpr, _thenexpr, _elseexpr)
    end
    
    function lambdaexpr()
        match(LAMBDA)
        argnames = []
    
        while token.type == ID
            push!(argnames, token.val)
            next()
        end
    
        match(ARROW)
        expr = expr()
    
        MLLambda(argnames, expr)
    end
    
    function app(name::String)
        match(LPAREN)
        args = []
        while token.type != RPAREN
            push!(args, expr())
            if token.type == COMMA
                next()
            elseif token.type == RPAREN
                break
            else
                err("Unexpected $(token.val) in application at $(token.pos)")
            end
        end
        match(RPAREN)
    
        MLApp(MLId(name), args)
    end
    
    () -> (
        # attribute
        lexer, token,
        # method
        parse
    )
end
