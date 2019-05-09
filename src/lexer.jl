import Base.string

""" 
A simple Token structure.
Contains the token type, value and position.
"""
struct MLToken <: AbstractML
    type :: String
    val  :: String
    pos  :: Int
end
string(t::MLToken) = "$(t.type)($(t.val)) at $(t.pos)"
const MLTokenNone = MLToken("Nothing", "nothing", -1)

# regex names
const IF     = "IF"
const THEN   = "THEN"
const ELSE   = "ELSE"
const TRUE   = "TRUE"
const FALSE  = "FALSE"
const LAMBDA = "LAMBDA"
const INT    = "INT"
const ARROW  = "ARROW"
const NEQ    = "!="
const EQEQ   = "=="
const GEQ    = ">="
const LEQ    = "<="
const LT     = "<"
const GT     = ">"
const PLUS   = "+"
const MINUS  = "-"
const TIMES  = "*"
const DIV    = "/"
const LPAREN = "("
const RPAREN = ")"
const EQ     = "="
const COMMA  = ","
const ID     = "ID"

# regex rules and its names
const RULES = [
    ("if",             IF),
    ("then",           THEN),
    ("else",           ELSE),
    ("true",           TRUE),
    ("false",          FALSE),
    ("lambda",         LAMBDA),
    ("\\d+",           INT),
    ("->",             ARROW),
    ("!=",             NEQ),
    ("==",             EQEQ),
    (">=",             GEQ),
    ("<=",             LEQ),
    ("<",              LT),
    (">",              GT),
    ("\\+",            PLUS),
    ("-",              MINUS),
    ("\\*",            TIMES),
    ("/",              DIV),
    ("\\(",            LPAREN),
    ("\\)",            RPAREN),
    ("=",              EQ),
    (",",              COMMA),
    ("[a-zA-Z_]\\w*",  ID),
]

"""
    Lexer(rules=RULES; skip_whitespace=rrue)

A simple regex-based lexer/tokenizer.

# Property

## attribute
+ buf :: String
+ pos :: Int

## method
+ start(buf::String)
+ token()
+ tokens()
+ peek()
"""
function Lexer(rules=RULES; skip_whitespace::Bool=true)
    #=
        init
    =#
    # prepare regex
    idx = 1
    regex_parts = Vector{String}()
    group_type = Dict{String, String}()
    for (re, typ) in RULES
         groupname = "GROUP$idx"
         idx += 1
         push!(regex_parts, "(?P<$groupname>$re)")
         group_type[groupname] = typ
    end
    regex = Regex(join(regex_parts, "|"))
    re_ws_skip = r"\S"
    
    # attribute
    buf = ""
    pos = -1

    #=
        methods
    =#
    
    """
        start(code::String) :: Nothing
    
    Initialize a `Lexer()`'s buffer with source code string. 
    And it will remove all comments.
    """
    function start(code::String)
        # remove comments: ``(* comments *)``
        buf = replace(
            code, 
            r"\(\*[^(\*\))]+\*\)"ms => m -> " " ^ length(m)
        )
        pos = 1
        nothing
    end
    
    """
        token() :: MLToken
    
    Match and return one `token::MLToken`.
    """
    function token() :: MLToken
        if pos > length(buf)
            return MLTokenNone
        end
        
        # update pos
        if skip_whitespace
            m = match(re_ws_skip, buf, pos)
            if m == nothing 
                return MLTokenNone
            else
                pos = m.offset
            end
        end
        
        # help function
        function get_group_id(a::Array{T}, val::T) where T
            for i in eachindex(a)
               if a[i] == val
                   continue
               else
                   return i
               end
            end
            0
        end
        get_group_id(a::Array{Int}) = get_group_id(a, 0)
        # `m.end()` in python
        get_match_end(m::RegexMatch) = m.offset + length(m.match)
        
        m = match(regex, buf, pos)
        if m == nothing
            throw(MLLexerException("Nothing match at [$pos]"))
        else
            # @show m m.offset m.offsets
            id = get_group_id(m.offsets)
            groupname = "GROUP$id"
            tok_type = group_type[groupname]
            tok = MLToken(tok_type, m[groupname], pos)
            pos = get_match_end(m)
            
            tok
        end
    end
    
    """
        tokens() :: Vector{MLToken}
    
    Get all tokens.
    """
    function tokens() :: Vector{MLToken}
        res = []
        while (tk = token()) != MLTokenNone
            push!(res, tk)
        end 
        res
    end
    
    """
        peek() :: MLToken
    
    Return current token and doesn't increase position.
    """
    function peek() :: MLToken
        old_pos = pos
        tk = token()
        pos = old_pos
        return tk
    end
    
    () -> (
        # attribute
        buf, pos,
        # methods
        start, token, tokens, peek
    )
end
