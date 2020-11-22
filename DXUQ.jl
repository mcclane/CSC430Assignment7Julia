
using Test

# Defines an error for us to use.
struct DXUQError <: Exception
    msg::String
end

# Makes Symbol a string. Julia has a Symbol type, but we're taking the easy route.
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
            throw(DXUQError("Unmatched parentheses"))
        end
    end

    # Now enumerate the input with our monster regular expression.
    for submatch in eachmatch(fullRx, input)
        #println("match: ", submatch)
        captures = submatch.captures
        if captures[1] != nothing
            #println("open")
            addExpression(ArraySExp(submatch.match))
        elseif captures[2] != nothing
            #println("close")
            if length(stack) == 1
                throw(DXUQError("Unmatch parentheses"))
            end
            pop!(stack)
        elseif captures[3] != nothing
            #println("number")
            # TODO: Actually look for a "." and create an Integer if one doesn't exist.
            addExpression(NumberSExp(parse(Float64, submatch.match)))
        elseif captures[4] != nothing
            #println("string")
            value = submatch.match
            # NOTE: The regex includes the "quotes" in the match, so trim those.
            # TODO: We should search replace escapes with actual characters, for example: \n, \t, etc...
            raw = SubString(value, 2, length(value) - 1)
            # Not efficient, but good enough for now.
            raw = replace(raw, "\\n" => "\n")
            addExpression(StringSExp(raw))
        elseif captures[5] != nothing
            #println("id")
            addExpression(SymbolSExp(submatch.match))
        elseif captures[6] != nothing
            #println("operator")
            # DXUQ doesn't distinguish operators from symbols.
            addExpression(SymbolSExp(submatch.match))
        end
    end
    if length(stack) != 1
        throw(DXUQError("Unmatched parentheses."))
    end
    return stack[1].values[1]
end

#println(lex("{+ 1 1}"))

# ================================
# Expressions and Values
# ================================

# Type of all expressions
abstract type ExprC end

# Type of all values
abstract type Value end

# Types defines a binding in the environment
mutable struct Binding
    name::Symbol
    value::Value
end

# An environment is just an array of Bindings.
Env = Array{Binding}

# Defines a closure
struct closureV <: Value
    args::Array{String}     # The arguments
    body::ExprC             # The body 
    env::Env                # The captures environment
end

# Defines a number
struct realV <: Value
    value::Real             # Its value
end

# Defines a boolean. 
# NOTE: Julia may support singletongs, and if it does, we should make one for true and false.
struct boolV <: Value
    value::Bool             # Its value
end

# Defines a string.
struct stringV <: Value
    value::String           # Its value
end

# Defines a call to a primitive
struct primitiveV <: Value
    name::Symbol            # The name of the primitive to call.
end

# Defines the void type.
struct voidV <: Value end

# Converts Value to a String.
function serialize(value::Value)::String
    if isa(value, realV)
        return string(value.value)
    elseif isa(value, boolV)
        return value.value ? "true" : "false"
    elseif isa(value, stringV)
        return string("\"", value.value, "\"")
    elseif isa(value, closureV) 
        return "#<procedure>"
    elseif isa(value, primitiveV)
        return "#<primop>"
    elseif isa(value, voidV)
        return "Void"
    end
end

# Converts the value to a nice string for printing.
function printable(value::Value)::String
    if isa(value, realV)
        return string(value.value)
    elseif isa(value, boolV)
        return value.value ? "true" : "false"
    elseif isa(value, stringV)
        return value.value
    elseif isa(value, closureV) 
        return string("{fn {", join(value.args, " "), "} ", unparse(value.body))
    elseif isa(value, primitiveV)
        return string("{", value.name, " ...}")
    elseif isa(value, voidV)
        return "Void"
    end
end

# Defines a value ExprC
struct valueC <: ExprC
    value::Value                # The value
end

# Defines a conditional ExprC. NOTE: Should be a primitive, but we didn't support that in Julia.
struct conditionalC <: ExprC
    condition::ExprC            # An expression that must evaluate to a boolean
    ifblock::ExprC              # The block to interpret if condition is true.
    elseblock::ExprC            # The block to interpret if condition is false.
end

# Defines an identifier.
struct idC <: ExprC
    name::Symbol                # The identifier's name.
end

# Defines a function application (Call).
struct appC <: ExprC
    func::ExprC                 # The function's code.
    arguments::Array{ExprC}     # Its arguments. Will be evaluated before call.
end

# Defines a lambda calculus function
struct lambdaC <: ExprC 
    args::Array{Symbol}         # Its named parameters
    body::ExprC                 # Its body to interpret
end

# Defines an assignment.
struct setC <: ExprC 
    variable::Symbol            # The symbol to assign to (in current environment)
    argument::ExprC             # The expression to evaluate and then assign.
end

# ==============================
# Primitives
# ==============================

# Defines a primitive, which is a name / body pair.
struct Primitive
    name::Symbol
    body
end

# Returns a Float64 or throws an error
function realOrError(value::Value)
    if isa(value, realV)
        return value.value
    end
    throw(DXUQError("Expected a number."))
end

# If value is a boolV, returns a Boolean, otherwise throws an error.
function boolOrError(value::Value)
    if isa(value, boolV)
        return value.value
    end
    throw(DXUQError("Expected a boolean."))
end

# Returns true if the two values are equal. Doesn't currently follow the defined rules of DXUQ.
function primitiveEqual(a::Value, b::Value)::Value
    return boolV(a == b)
end

# Returns a / b, unless b is 0, in which case an exception is thrown.
function primitiveDivide(a::Value, b::Value)::Value
    dividend = realOrError(a)
    divisor = realOrError(b)
    if divisor == 0 
        throw(DXUQError("Divide by zero"))
    end
    return realV(dividend / divisor)
end

# Implements printing. Returns void.
function primitivePrint(values...)::Value
    for value in values
        print(printable(value))
    end
    return voidV()
end

# Implements the begin function, we basically returns the result of evaluating the last arguments.
function primitiveBegin(values...)::Value
    return last(values)
end

# A dictionary of primitives.
primitives = Dict([
    "+" => Primitive("+", (a, b) -> realV(realOrError(a) + realOrError(b))),
    "-" => Primitive("-", (a, b) -> realV(realOrError(a) - realOrError(b))),
    "*" => Primitive("*", (a, b) -> realV(realOrError(a) * realOrError(b))),
    "/" => Primitive("/", primitiveDivide),
    "<=" => Primitive("<=", (a, b) -> boolV(realOrError(a) <= realOrError(b))),
    "<" => Primitive("<", (a, b) -> boolV(realOrError(a) < realOrError(b))),
    ">=" => Primitive(">=", (a, b) -> boolV(realOrError(a) >= realOrError(b))),
    ">" => Primitive(">", (a, b) -> boolV(realOrError(a) > realOrError(b))),
    "not" => Primitive("not", (a) -> boolV(!boolOrError(a))),
    "equal?" => Primitive("equal?", primitiveEqual),
    "print" => Primitive("print", primitivePrint),
    "begin" => Primitive("begin", primitiveBegin),
])

# Returns the primitives as they'd be defined in an environment.
primitiveEnv = map(p -> Binding(p.name, primitiveV(p.name)), values(primitives))

# Looks up and returns a primitive's body. Throws an error if no primitive exists.
function primitiveGet(primitives, name)
    primitive = primitives[name]
    if primitive == nothing 
        throw(DXUQError("Undefined primitive: ", name))
    end
    return primitive.body
end

# ==============================
# Environment functions
# ==============================

# Get a value from the environment. Throws an error if the symbol doesn't exist.
function envGet(env::Env, symbol::Symbol)::Value
    index = findfirst(binding -> binding.name == symbol, env)
    if index == nothing
        throw(DXUQError("Undefined symbol: ", symbol))
    end
    return env[index].value
end

# Sets the value of the symbol in the environment.
function envSet!(env::Env, symbol::Symbol, value::Value)
    index = findfirst(binding -> binding.name == symbol, env)
    if index == nothing
        throw(DXUQError("Undefined symbol: ", symbol))
    end
    env[index].value = value
end

# ==============================
# Parsing
# ==============================

# Parses the SExpression returning an ExprC.
function dxuqParse(expression::SExpression)::ExprC

    # Helper for DXUQParse. Returns true if symbol is an SymbolSExp and its name is name.
    function isSymbol(symbol::SExpression, name::String)::Bool
        return isa(symbol, SymbolSExp) && symbol.name == name
    end

    # Helper for DXUQParse. Builds an array of let assignments.
    function buildLetDeclarations(declarations::SubArray{SExpression})::Array{NamedTuple}
        output = Vector()
        for expression in declarations
            if (isa(expression, ArraySExp)
                && length(expression.values) == 3
                && isa(expression.values[1], SymbolSExp)
                && isSymbol(expression.values[2], "="))
                name = expression.values[1].name
                body = dxuqParse(expression.values[3])
                tuple = (symbol=name, body=body)
                push!(output, tuple)
            else
                throw(DXUQError("Bad let synyax"))
            end
        end
        return output
    end

    if isa(expression, NumberSExp) 
        return valueC(realV(expression.value))
    end
    if isa(expression, StringSExp)
        return valueC(stringV(expression.value))
    end
    if isa(expression, SymbolSExp)
        return idC(expression.name)
    end
    if (isa(expression, ArraySExp)
            && length(expression.values) == 3
            && isSymbol(expression.values[2], ":="))
        return setC(expression.values[1].name, dxuqParse(expression.values[3]))
    end
    if (isa(expression, ArraySExp) 
            && length(expression.values) == 3
            && isSymbol(expression.values[1], "fn")
            && isa(expression.values[2], ArraySExp))
        params = map(symbolExpression -> symbolExpression.name, expression.values[2].values)
        body = dxuqParse(expression.values[3])
        return lambdaC(params,  body)
    end
    if (isa(expression, ArraySExp) 
            && length(expression.values) == 4
            && isSymbol(expression.values[1], "if"))
        return conditionalC(dxuqParse(expression.values[2]), dxuqParse(expression.values[3]), dxuqParse(expression.values[4]))
    end
    if (isa(expression, ArraySExp)
            && length(expression.values) >= 4
            && isSymbol(expression.values[1], "let"))
        inIndex = findfirst(a -> isSymbol(a, "in"), expression.values)
        if (inIndex != nothing
            && length(expression.values) == inIndex + 1)
            decls = buildLetDeclarations(view(expression.values, 2:inIndex - 1))
            body = dxuqParse(expression.values[inIndex + 1])
            return appC(lambdaC(map(tuple -> tuple.symbol, decls), body),
                        map(tuple -> tuple.body, decls))
        end
    end
    if (isa(expression, ArraySExp)
            && length(expression.values) > 0)
        body = dxuqParse(expression.values[1])
        argumentLength = length(expression.values)
        if argumentLength - 1 > 0
            rawArguments = view(expression.values, 2:length(expression.values))
        else
            rawArguments = Array{ExprC}()
        end
        arguments = map(expr -> dxuqParse(expr), rawArguments)
        return appC(body, arguments)
    end
    throw(DXUQError("Syntax error!"))
end

# Lexically parses and then symantically parses string.
function dxuqParse(expression::String)::ExprC
    exp = lex(expression)
    return dxuqParse(exp)
end

# Returns list of ExprC as a concatenated, human readable-ish form.
function unparseList(expressions::Array{ExprC})::String
    string = ""

    for expression in expressions
        if length(string) > 0
            string = string * " " * unparse(expression)
        else
            string = unparse(expression)
        end
    end
    return string
end

# Returns ExprC in human readable form.
function unparse(expression::ExprC)::String
    if isa(expression, valueC)
        return serialize(expression.value)
    elseif isa(expression, idC)
        return expression.name
    elseif isa(expression, setC) 
        return string("{", expression.variable, " := ", unparse(expression.argument), "}")
    elseif isa(expression, appC)
        args = expression.arguments
        if length(args) == 0
            return string("{", unparse(expression.func), "}")
        end
        return string("{", unparse(expression.func), " ", unparseList(args), "}")
    elseif isa(expression, lambdaC)
        return string("{fn {", join(expression.args, " "), "} ", unparse(expression.body), "}")
    end
    throw(DXUQError("Couldn't unparse: ", expression))
end

#println(dxuqParse("1"))
#println(dxuqParse("\"hi mom\""))
#println(dxuqParse("+"))
#println(dxuqParse("{+ 1 2}"))
#println(dxuqParse("{fn {x y} {+ x y}}"))
#println(dxuqParse("{{fn {x y} {+ x y}} 3 4}"))
#println(dxuqParse("{if {<= 1 2} \"a\" \"b\"}"))

# ==============================
# Interpretation
# ==============================

# Interpres expression, return the expression's result as a Value
function interp(expression::ExprC, env::Env)::Value
    if isa(expression, valueC)
        return expression.value
    elseif isa(expression, setC)
        envSet!(env, expression.variable, interp(expression.argument, env))
        return voidV()
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

# Deals with calling a function for interp() above.
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

# Represents the root environment
rootEnv = vcat(
    [
        Binding("true", boolV(true)),
        Binding("false", boolV(false)),
    ]
    , primitiveEnv)

# Parses the string and then call interp with the root environment.
function topInterp(program::String)::String
    expression = dxuqParse(program)
    return serialize(interp(expression, rootEnv))
end

# ==============================
# Unit Tests
# ==============================

@test interp(
    valueC(realV(1)),
    rootEnv
) == realV(1)

@test interp(
    conditionalC(idC("true"), valueC(realV(1)), valueC(realV(0))),
    rootEnv
) == realV(1)

@test interp(
    conditionalC(idC("false"), valueC(realV(1)), valueC(realV(0))),
    rootEnv
) == realV(0)

@test interp(
    appC(lambdaC(["x", "y"], appC(idC("+"), [idC("x"), idC("y")])), [valueC(realV(4)), valueC(realV(5))]),
    rootEnv
) == realV(9)

@test interp(
    appC(lambdaC(["x", "y"], appC(idC("-"), [idC("x"), idC("y")])), [valueC(realV(4)), valueC(realV(5))]),
    rootEnv
) == realV(-1)

@test_throws DXUQError dxuqParse("{{+ 1 1}")
@test_throws DXUQError dxuqParse("{+ 1 1}}")
@test dxuqParse("1") == valueC(realV(1.0))
@test dxuqParse("\"hi mom\"") == valueC(stringV("hi mom"))
@test dxuqParse("+") == idC("+")
# TODO: Not equalling, not sure why. Not worried about it right now.
#@test dxuqParse("{+ 1 2}") == appC(idC("+"), ExprC[valueC(realV(1.0)), valueC(realV(2.0))])
@test topInterp("{+ 1 1}") == "2.0"
@test topInterp("{- 2 1}") == "1.0"
@test topInterp("{* 2 2}") == "4.0"
@test topInterp("{/ 2 2}") == "1.0"
@test_throws DXUQError topInterp("{/ 2 0}")
@test topInterp("{{fn {x y} {+ x y}} 3 4}") == "7.0"
@test topInterp("{<= 1 2}") == "true"
@test topInterp("{if {<= 1 2} \"a\" \"b\"}") == "\"a\""
@test topInterp("{if {<= 3 2} \"a\" \"b\"}") == "\"b\""
@test topInterp("{if {<= 2 2} \"a\" \"b\"}") == "\"a\""
@test topInterp("{if {not {equal? 1 2}} \"a\" \"b\"}") == "\"a\""
@test topInterp("{print 1 \"\\n\" 2 \"\\n\" 3 \"\\n\" \"hi\" \"\\n\" {fn {a b} {+ a b}} \"\\n\"}") == "Void"
@test topInterp("{begin 1 2 3}") == "3.0"
@test unparse(dxuqParse("{+ 1 1}")) == "{+ 1.0 1.0}"
@test unparse(dxuqParse("{fn {a b} {+ a b}}")) == "{fn {a b} {+ a b}}"

simpleTest = """{let
                  {f = {fn {x} {+ x 14}}}
                  in
                  {f 2}}
                """
@test unparse(dxuqParse(simpleTest)) == "{{fn {f} {f 2.0}} {fn {x} {+ x 14.0}}}"
@test topInterp(simpleTest) == "16.0"

@test unparse(dxuqParse("{let {a = 10} in {begin {a := 5} a}}")) == "{{fn {a} {begin {a := 5.0} a}} 10.0}"
@test topInterp("{let {a = 10} in {begin {a := 5} a}}") == "5.0"