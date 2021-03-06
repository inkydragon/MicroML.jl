# import Base: parse, match

#= EBNF specification for micro-ML. 
{ x } means zero or more repetitions of x.

The top-level is decl.

decl:   ID { ID } '=' expr
expr:   INT
        | bool
        | ID
        | ID '(' { expr ',' } ')'
        | '(' expr ')'
        | expr op expr
        | 'if' expr 'then' expr 'else' expr
        | 'lambda' { ID } '->' expr
op:     + | * | - | /
        | == | !=
        | > | >= | <= | < 
bool:   'true' | 'false'
ID:     identifier
INT:    an integer
=#

"""
    Parser()

Parser for micro-ML.
    
Usage:
```julia
p = Parser()
decl = p.parse(<some micro-ML code>)
# decl is now an ast.Decl node
```

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
    OPERATORS = Set([
        NEQ,    EQEQ,   GEQ,    LEQ,    LT,     GT,
        PLUS,   MINUS,  TIMES,  DIV
    ])
    should_terminate = true
    
    function parse(source::String, should_terminate::Bool)
        lexer.start(source)
        next()
        _decl = decl()
        if token != MLTokenNone && should_terminate
            ppos(lexer)
            err("Unexpected `token `$(token.val)` at [$(token.pos)]") 
        end
        
        _decl, token.pos
    end
    
    err(msg::String) = throw(MLParserException(msg))
    next() = begin 
        try
            token = lexer.token()
        catch e
            throw(MLParserException("Lexer error: $e"))
        end
    end
    
    function match(typ::String) :: String
        if token.type == typ
            _val = token.val
            next()
            _val
        else
            ppos(lexer)
            err("Expected `$(typ)`, but found `$(token.type)` at [$(token.pos)]")
        end
    end
    
    function decl()
        _name = match(ID)
        _argnames = []
    
        while token.type == ID
            push!(_argnames, token.val)
            next()
        end
    
        match(EQ)
        _expr = expr()
    
        if isempty(_argnames)
            MLDecl(_name, _expr)
        else
            MLDecl(_name, MLLambda(_argnames, _expr))
        end
    end
    
    """
    Parse an expr of the form:
    
        expr op expr
    
    We only allow a single operator between expressions. Additional
    operators should be nested using parens, e.g. x + (y * z)
    """
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
    
    """
    Parse an expr component (components can be separated by an operator).
    """
    function expr_component()
        _token = token
        _typ = _token.type
        _val = _token.val
    
        if _typ == INT
            next()
            MLInt(_val)
        elseif _typ in [FALSE, TRUE]
            next()
            MLBool(_val)
        elseif _typ == ID
            next()
            if token.type == LPAREN
                # ID followed by '(' is function application
                app(_val)
            else
                MLId(_val)
            end
        elseif _typ == LPAREN
            next()
            _expr = expr()
            match(RPAREN)
            _expr
        elseif _typ == IF
            ifexpr()
        elseif _typ == LAMBDA
            lambdaexpr()
        else
            ppos(lexer)
            err("We don’t support `$_typ` yet!")
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
        _argnames = []
    
        while token.type == ID
            push!(_argnames, token.val)
            next()
        end
        match(ARROW)
        _expr = expr()
    
        MLLambda(_argnames, _expr)
    end
    
    function app(_name::String)
        match(LPAREN)
        _args = []
        while token.type != RPAREN
            push!(_args, expr())
            if token.type == COMMA
                next()
            elseif token.type == RPAREN
                break
            else
                ppos(lexer)
                err("Unexpected `$(token.val)` in application at [$(token.pos)]")
            end
        end
        match(RPAREN)
    
        MLApp(MLId(_name), _args)
    end
    
    () -> (
        # attribute
        lexer, token,
        # method
        parse
    )
end
