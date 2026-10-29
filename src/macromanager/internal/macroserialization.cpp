/*
* Audacity: A Digital Audio Editor
*/
#include "macroserialization.h"

#include <charconv>
#include <cmath>
#include <limits>
#include <optional>

using namespace au::macromanager;
using namespace muse;
using namespace muse::rcommand;

namespace {
const std::string NAME_KEY("name");
const std::string STEPS_KEY("steps");
const std::string COMMAND_KEY("command");
const std::string PARAMS_KEY("params");
const std::string MACRO_KEY("macro");
const std::string INT64_KEY("int64");

JsonObject int64ToJson(int64_t number)
{
    JsonObject object;
    object.set(INT64_KEY, std::to_string(number));

    return object;
}

std::optional<int64_t> int64FromJson(const JsonObject& object)
{
    if (object.size() != 1 || !object.contains(INT64_KEY)) {
        return std::nullopt;
    }

    const JsonValue value = object.value(INT64_KEY);
    if (!value.isString()) {
        return std::nullopt;
    }

    const std::string& text = value.toStdString();
    int64_t number = 0;
    const auto [end, error] = std::from_chars(text.data(), text.data() + text.size(), number);
    if (error != std::errc() || end != text.data() + text.size()) {
        return std::nullopt;
    }

    return number;
}

JsonObject valMapToJson(const ValMap& map);

JsonArray valListToJson(const ValList& list);

JsonValue valToJson(const Val& val)
{
    JsonValue value;

    switch (val.type()) {
    case Val::Type::Bool: value = val.toBool();
        break;
    case Val::Type::Int: value = val.toInt();
        break;
    case Val::Type::Int64: value = int64ToJson(val.toInt64());
        break;
    case Val::Type::Double: value = val.toDouble();
        break;
    case Val::Type::List: value = valListToJson(val.toList());
        break;
    case Val::Type::Map: value = valMapToJson(val.toMap());
        break;
    default: value = val.toString();
        break;
    }

    return value;
}

JsonArray valListToJson(const ValList& list)
{
    JsonArray array;
    for (const Val& val : list) {
        array << valToJson(val);
    }

    return array;
}

JsonObject valMapToJson(const ValMap& map)
{
    JsonObject object;
    for (const auto& [key, val] : map) {
        object.set(key, valToJson(val));
    }

    return object;
}

Val numberToVal(double number)
{
    if (std::trunc(number) == number
        && number >= std::numeric_limits<int>::min() && number <= std::numeric_limits<int>::max()) {
        return Val(static_cast<int>(number));
    }

    return Val(number);
}

ValMap valMapFromJson(const JsonObject& object);

std::optional<Val> valFromJson(const JsonValue& value)
{
    if (value.isBool()) {
        return Val(value.toBool());
    }

    if (value.isNumber()) {
        return numberToVal(value.toDouble());
    }

    if (value.isString()) {
        return Val(value.toStdString());
    }

    if (value.isArray()) {
        const JsonArray array = value.toArray();

        ValList list;
        for (size_t i = 0; i < array.size(); ++i) {
            if (const std::optional<Val> val = valFromJson(array.at(i))) {
                list.push_back(*val);
            }
        }

        return Val(list);
    }

    if (value.isObject()) {
        const JsonObject object = value.toObject();
        if (const std::optional<int64_t> number = int64FromJson(object)) {
            return Val(*number);
        }

        return Val(valMapFromJson(object));
    }

    return std::nullopt;
}

ValMap valMapFromJson(const JsonObject& object)
{
    ValMap map;
    for (const std::string& key : object.keys()) {
        if (const std::optional<Val> val = valFromJson(object.value(key))) {
            map[key] = *val;
        }
    }

    return map;
}
}

JsonObject au::macromanager::toJson(const Macro& macro)
{
    JsonArray steps;
    for (const MacroItem& item : macro.steps) {
        JsonObject stepObject;

        if (const MacroStep* step = std::get_if<MacroStep>(&item)) {
            stepObject.set(COMMAND_KEY, step->command.toString());
            stepObject.set(PARAMS_KEY, valMapToJson(step->params));
        } else {
            stepObject.set(MACRO_KEY, std::get<MacroReference>(item).name);
        }

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

        if (stepObject.contains(MACRO_KEY)) {
            const std::string macroName = stepObject.value(MACRO_KEY).toStdString();
            if (macroName.empty()) {
                return RetVal<Macro>(make_ret(Ret::Code::UnknownError, "macro step has no macro name"));
            }

            macro.steps.push_back(MacroReference { macroName });
            continue;
        }

        const Command command(stepObject.value(COMMAND_KEY).toStdString());
        if (!command.isValid()) {
            return RetVal<Macro>(make_ret(Ret::Code::UnknownError, "macro step has an invalid command"));
        }

        macro.steps.push_back(MacroStep { command, valMapFromJson(stepObject.value(PARAMS_KEY).toObject()) });
    }

    return RetVal<Macro>::make_ok(macro);
}
