
Symbol = String

#
# Defines the lexical pieces of the langauge
#
openParenRx = r"[\({\[]"
closeParenRx = r"[\)}\]]"
numericLiteralRx = r"(?:-?(?:(?:(?:(?:[0-9])(?:(?:(?:[0-9]))|_)*)(?:\.(?:(?:[0-9])(?:(?:(?:[0-9]))|_)*))?(?:(?:[eE])(?:[-+])?(?:(?:[0-9])(?:(?:(?:[0-9]))|_)*))?)|(?:0x(?:[0-9A-Fa-f])(?:(?:(?:[0-9A-Fa-f]))|_)*)(?:\.(?:[0-9A-Fa-f])(?:(?:(?:[0-9A-Fa-f]))|_)*)?(?:(?:[pP])(?:[-+])?(?:(?:[0-9])(?:(?:(?:[0-9]))|_)*))))|(?:-?(?:(?:(?:(?:[0-9])(?:(?:(?:[0-9]))|_)*)|(?:0x(?:[0-9A-Fa-f])(?:(?:(?:[0-9A-Fa-f]))|_)*)|(?:0o(?:[0-7])(?:(?:(?:[0-7]))|_)*)|(?:0b(?:[0-1])(?:(?:(?:[0-1]))|_)*))))"
stringLiteralRx = r"(?:(?:\")(?:(?:(?:(?:(?:(?:\\)0)|(?:(?:\\)\\)|(?:(?:\\)t)|(?:(?:\\)n)|(?:(?:\\)r)|(?:(?:\\)\")|(?:(?:\\)')))|[^\"\\\u000A\u000D])+)?(?:\"))"
idRx = r"(?:[A-Za-z_\u00A8\u00AA\u00AD\u00AF\u00B2-\u00B5\u00B7-\u00BA\u00BC-\u00BE\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u00FF\u0100-\u02FF\u0370-\u167F\u1681-\u180D\u180F-\u1DBF\u1E00-\u1FFF\u200B-\u200D\u202A-\u202E\u203F-\u2040\u2054\u2060-\u206F\u2070-\u20CF\u2100-\u218F\u2460-\u24FF\u2776-\u2793\u2C00-\u2DFF\u2E80-\u2FFF\u3004-\u3007\u3021-\u302F\u3031-\u303F\u3040-\uD7FF\uF900-\uFD3D\uFD40-\uFDCF\uFDF0-\uFE1F\uFE30-\uFE44\uFE47-\uFFFD\U00010000-\U0001FFFD\U00020000-\U0002FFFD\U00030000-\U0003FFFD\U00040000-\U0004FFFD\U00050000-\U0005FFFD\U00060000-\U0006FFFD\U00070000-\U0007FFFD\U00080000-\U0008FFFD\U00090000-\U0009FFFD\U000A0000-\U000AFFFD\U000B0000-\U000BFFFD\U000C0000-\U000CFFFD\U000D0000-\U000DFFFD\U000E0000-\U000EFFFD])(?:(?:(?:[A-Za-z_\u00A8\u00AA\u00AD\u00AF\u00B2-\u00B5\u00B7-\u00BA\u00BC-\u00BE\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u00FF\u0100-\u02FF\u0370-\u167F\u1681-\u180D\u180F-\u1DBF\u1E00-\u1FFF\u200B-\u200D\u202A-\u202E\u203F-\u2040\u2054\u2060-\u206F\u2070-\u20CF\u2100-\u218F\u2460-\u24FF\u2776-\u2793\u2C00-\u2DFF\u2E80-\u2FFF\u3004-\u3007\u3021-\u302F\u3031-\u303F\u3040-\uD7FF\uF900-\uFD3D\uFD40-\uFDCF\uFDF0-\uFE1F\uFE30-\uFE44\uFE47-\uFFFD\U00010000-\U0001FFFD\U00020000-\U0002FFFD\U00030000-\U0003FFFD\U00040000-\U0004FFFD\U00050000-\U0005FFFD\U00060000-\U0006FFFD\U00070000-\U0007FFFD\U00080000-\U0008FFFD\U00090000-\U0009FFFD\U000A0000-\U000AFFFD\U000B0000-\U000BFFFD\U000C0000-\U000CFFFD\U000D0000-\U000DFFFD\U000E0000-\U000EFFFD]))|(?:[0-9\u002D\u003F\u0021\u0300–\u036F\u1DC0–\u1DFF\u20D0–\u20FF\uFE20–\uFE2F]))*"
operatorRx = r"(?:[:\/=\-\+!\*%<>&\|\^~\?\u00A1-\u00A7\u00A9-\u00AB\u00AC-\u00AE\u00B0-\u00B1\u00B6\u00BB\u00BF\u00D7\u00F7\u2016-\u2017\u2020-\u2027\u2030-\u203E\u2041-\u2053\u2055-\u205E\u2119-\u23FF\u2500-\u2775\u2794-\u2BFF\u2E00-\u2E7F\u3001-\u3003\u3008-\u3020\u3030])(?:(?:(?:[:\/=\-\+!\*%<>&\|\^~\?\u00A1-\u00A7\u00A9-\u00AB\u00AC-\u00AE\u00B0-\u00B1\u00B6\u00BB\u00BF\u00D7\u00F7\u2016-\u2017\u2020-\u2027\u2030-\u203E\u2041-\u2053\u2055-\u205E\u2119-\u23FF\u2500-\u2775\u2794-\u2BFF\u2E00-\u2E7F\u3001-\u3003\u3008-\u3020\u3030]))|[\u0300-\u036F\u1DC0-\u1DFF\u20D0-\u20FF\uFE00-\uFE0F\uFE20-\uFE2F\u00E0100-\u00E01EF])*"

# Combine all of the above into one mother-of-all regular expression. We can then enumerate a string with the
# eachmatch() call, which will indicate to us which subexpression we matched against. This can then be translated
# to the token type we found.
fullRx = Regex(join(["(", openParenRx.pattern, ")|",
                     "(", closeParenRx.pattern, ")|",
                     "(", numericLiteralRx.pattern, ")|",
                     "(", stringLiteralRx.pattern, ")|",
                     "(", idRx.pattern, ")|",
                     "(", operatorRx.pattern, ")"]))

# Define the structures used by the lexer

# The "SExpression" typed or the supertype of all of our expressions.
abstract type SExpression end

# Represents an array expression.
struct ArraySExp <: SExpression
    values::Array{SExpression}
    openParan::String
    closeParen::String
    ArraySExp(openP) = (
        closeP = "";
        if openP == "("
            closeP = ")"
        elseif openP == "{"
            closeP = "}"
        elseif openP == "["
            closeP = "]"
        end;
        new(Vector{SExpression}(), openP, closeP)
    )
end

# Represents a symbol
struct SymbolSExp <: SExpression
    name::Symbol
end

# Represents a number
struct NumberSExp <: SExpression
    value::Number
end

# Represents a string
struct StringSExp <: SExpression
    value::String
end

# Lexically parses input input an SExpression. This can then be parsed.
function lex(input::String)::SExpression
    # Stack to track the nested expressions
    stack = Vector{SExpression}()

    # Add an initial ArraySExp
    push!(stack, ArraySExp(""))

    # Add the expression to our stack. The expression is added to the last ArraySExp on the stack. If the expression is
    # also an ArraySExp, then it's pushed on to the stack. If the stack is empty, throws an error.
    function addExpression(expression::SExpression)
        if length(stack) >= 1
            last = stack[length(stack)]
            if isa(last, ArraySExp)
                push!(last.values, expression)
                if isa(expression, ArraySExp)
                    push!(stack, expression)
                end
            end
        else
            error("DXUQ: Error: Unmatched parentheses")
        end
    end

    # Now enumerate the input with our monster regular expression.
    for submatch in eachmatch(fullRx, input)
        #println("match: ", submatch)
        caps = submatch.captures
        if caps[1] != nothing
            println("open")
            addExpression(ArraySExp(submatch.match))
        elseif caps[2] != nothing
            println("close")
            if length(stack) == 1
                error("DXUQ: Error: Unmatch parentheses")
            end
            pop!(stack)
        elseif caps[3] != nothing
            println("number")
            # TODO: Actually look for a "." and create an Integer if one doesn't exist.
            addExpression(NumberSExp(parse(Float64, submatch.match)))
        elseif caps[4] != nothing
            println("string")
            addExpression(StringSExp(submatch.match))
        elseif caps[5] != nothing
            println("id")
            addExpression(SymbolSExp(submatch.match))
        elseif caps[6] != nothing
            # DXUQ doesn't distinguish operators from symbols.
            addExpression(SymbolSExp(submatch.match))
            println("operator")
        end
    end
    if length(stack) != 1
        error("DXUQ: Error: Unmatched parentheses.")
    end
    return stack[1]
end

println(lex("{+ 1 1}"))

# ================================
# Expressions and Values
# ================================

abstract type ExprC end
abstract type Value end

struct Binding
    name::Symbol
    value::Value
end
Env = Array{Binding}

struct closureV <: Value
    args::Array{String}
    body::ExprC
    env::Env
end

struct realV <: Value
    value::Real
end

struct boolV <: Value
    value::Bool
end

struct stringV <: Value
    value::String
end

struct primitiveV <: Value
    name::Symbol
end

struct valueC <: ExprC
    value::Value
end

struct conditionalC <: ExprC
    condition::ExprC
    ifblock::ExprC
    elseblock::ExprC
end

struct idC <: ExprC
    name::Symbol
end

struct appC <: ExprC
    func::ExprC
    arguments::Array{ExprC}
end

struct lambdaC <: ExprC
    args::Array{Symbol}
    body::ExprC
end

# ==============================
# Primitives
# ==============================

struct Primitive
    name
    body
end

primitives = [
    Primitive("+", (a, b) -> realV(a.value + b.value)),
    Primitive("-", (a, b) -> realV(a.value - b.value))
]
primitiveEnv = map(p -> Binding(p.name, primitiveV(p.name)), primitives)

function primitiveGet(primitives, name)
    (first, rest) = Iterators.peel(primitives)
    if first.name == name
        return first.body
    end
    return primitiveGet(rest, name)
end

# ==============================
# Environment functions
# ==============================


function envGet(env, symbol::Symbol)::Value # Yes, the first argument is missing a type...
    (first, rest) = Iterators.peel(env)
    if first.name == symbol
        return first.value 
    end
    return envGet(rest, symbol)
end

# ==============================
# Parsing
# ==============================

function dxuqParse(expression::SExpression)::ExprC
end

# ==============================
# Interpretation
# ==============================

function interp(expression::ExprC, env::Env)::Value
    if isa(expression, valueC)
        return expression.value
    elseif isa(expression, conditionalC)
        if interp(expression.condition, env).value
            return interp(expression.ifblock, env)
        else
            return interp(expression.elseblock, env)
        end
    elseif isa(expression, lambdaC)
        return closureV(expression.args, expression.body, env)
    elseif isa(expression, appC)
        return interpFunction(expression.func, expression.arguments, env)
    elseif isa(expression, idC)
        return envGet(env, expression.name)
    end
end

function interpFunction(func::ExprC, arguments::Array{ExprC}, env::Env)::Value
    closure = interp(func, env)
    if isa(closure, closureV)
        argValues = map(a -> interp(a, env), arguments)
        newEnv = vcat(closure.env, map(t -> Binding(t[1], t[2]), zip(closure.args, argValues)))
        return interp(closure.body, newEnv)
    elseif isa(closure, primitiveV)
        argValues = map(a -> interp(a, env), arguments)
        body = primitiveGet(primitives, closure.name)
        return body(argValues...)
    end
end

rootEnv = vcat(
    [
        Binding("true", boolV(true)),
        Binding("false", boolV(false)),
    ]
    , primitiveEnv)

@assert interp(
    valueC(realV(1)),
    rootEnv
) == realV(1)

@assert interp(
    conditionalC(idC("true"), valueC(realV(1)), valueC(realV(0))),
    rootEnv
) == realV(1)

@assert interp(
    conditionalC(idC("false"), valueC(realV(1)), valueC(realV(0))),
    rootEnv
) == realV(0)

@assert interp(
    appC(lambdaC(["x", "y"], appC(idC("+"), [idC("x"), idC("y")])), [valueC(realV(4)), valueC(realV(5))]),
    rootEnv
) == realV(9)

@assert interp(
    appC(lambdaC(["x", "y"], appC(idC("-"), [idC("x"), idC("y")])), [valueC(realV(4)), valueC(realV(5))]),
    rootEnv
) == realV(-1)
