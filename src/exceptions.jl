
abstract type MLException <: Exception end

struct MLLexerException <: MLException
    msg :: String
    mod :: String
    MLLexerException(msg) = new(msg, "lexer")
end
struct MLParserException <: MLException
    msg :: String
    mod :: String
    MLParserException(msg) = new(msg, "parser")
end
struct MLTypingException <: MLException
    msg :: String
    mod :: String
    MLTypingException(msg) = new(msg, "types")
end
struct MLEvalException <: MLException
    msg :: String
    mod :: String
    MLEvalException(msg) = new(msg, "interpretation")
end
struct MLCompilerException <: MLException
    msg :: String
    mod :: String
    MLCompilerException(msg) = new(msg, "compiler")
end
