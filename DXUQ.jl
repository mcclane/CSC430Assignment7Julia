abstract type ExprC end
abstract type Value end

Symbol = String

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

function envGet(env, symbol::Symbol)::Value # Yes, the first argument is missing a type...
    (first, rest) = Iterators.peel(env)
    if first.name == symbol
        return first.value 
    end
    return envGet(rest, symbol)
end

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