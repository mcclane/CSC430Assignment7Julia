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

vc = valueC(realV(1))
print(vc)

clo = closureV(["Hello"], valueC(realV(1)), Env([Binding("hello", realV(1))]))
print(clo)

i = idC("asdf")
print(i)

function interpFunction(func::ExprC, arguments::Array{ExprC}, env::Env)::Value
    return interp(func, env)
end

function envGet(env::Env, symbol::Symbol)::Value
    return realV(1)
end


function interp(expression::ExprC, env::Env)::Value
    if isa(expression, valueC)
        return expression.value
    elseif isa(expression, conditionalC)
        if expression.condition
            return interp(expression.ifblock, env)
        else
            return interp(expression.elseblock, env)
        end
    elseif isa(expression, lambdaC)
        return closureV(expression.arguments, expression.body, env)
    elseif isa(expression, appC)
        return interpFunction(expression.func, expression.arguments, env)
    elseif isa(expression, idC)
        return envGet(env, expression.name)
    end
end

interp(valueC(realV(1)), Env([Binding("hello", realV(1))]))