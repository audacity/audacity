/*
* Audacity: A Digital Audio Editor
*/
#include "macroserialization.h"

#include <cmath>
#include <limits>

using namespace au::macromanager;
using namespace muse;
using namespace muse::rcommand;

namespace {
const std::string NAME_KEY("name");
const std::string STEPS_KEY("steps");
const std::string COMMAND_KEY("command");
const std::string PARAMS_KEY("params");

JsonObject paramsToJson(const Params& params)
{
    JsonObject object;

    for (const auto& [key, val] : params) {
        switch (val.type()) {
        case Val::Type::Bool: object.set(key, val.toBool());
            break;
        case Val::Type::Int: object.set(key, val.toInt());
            break;
        case Val::Type::Int64:
        case Val::Type::Double: object.set(key, val.toDouble());
            break;
        default: object.set(key, val.toString());
            break;
        }
    }

    return object;
}

Val numberToVal(double number)
{
    const bool isIntegral = std::floor(number) == number
                            && number >= std::numeric_limits<int>::min()
                            && number <= std::numeric_limits<int>::max();

    return isIntegral ? Val(static_cast<int>(number)) : Val(number);
}

Params paramsFromJson(const JsonObject& object)
{
    Params params;

    for (const std::string& key : object.keys()) {
        const JsonValue value = object.value(key);

        if (value.isBool()) {
            params[key] = Val(value.toBool());
        } else if (value.isNumber()) {
            params[key] = numberToVal(value.toDouble());
        } else if (value.isString()) {
            params[key] = Val(value.toStdString());
        }
    }

    return params;
}
}

JsonObject au::macromanager::toJson(const Macro& macro)
{
    JsonArray steps;
    for (const MacroStep& step : macro.steps) {
        JsonObject stepObject;
        stepObject.set(COMMAND_KEY, step.command.toString());
        stepObject.set(PARAMS_KEY, paramsToJson(step.params));
        steps << stepObject;
    }

    JsonObject object;
    object.set(NAME_KEY, macro.name);
    object.set(STEPS_KEY, steps);

    return object;
}

RetVal<Macro> au::macromanager::macroFromJson(const JsonObject& object)
{
    const JsonValue name = object.value(NAME_KEY);
    if (!name.isString() || name.toStdString().empty()) {
        return RetVal<Macro>(make_ret(Ret::Code::UnknownError, "macro has no name"));
    }

    Macro macro;
    macro.name = name.toStdString();

    const JsonArray steps = object.value(STEPS_KEY).toArray();
    for (size_t i = 0; i < steps.size(); ++i) {
        const JsonObject stepObject = steps.at(i).toObject();

        const Command command(stepObject.value(COMMAND_KEY).toStdString());
        if (!command.isValid()) {
            return RetVal<Macro>(make_ret(Ret::Code::UnknownError, "macro step has an invalid command"));
        }

        macro.steps.push_back({ command, paramsFromJson(stepObject.value(PARAMS_KEY).toObject()) });
    }

    return RetVal<Macro>::make_ok(macro);
}
