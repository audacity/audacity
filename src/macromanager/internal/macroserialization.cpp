/*
* Audacity: A Digital Audio Editor
*/
#include "macroserialization.h"

#include "framework/global/translation.h"

using namespace au::macromanager;
using namespace muse;
using namespace muse::rcommand;

namespace {
const std::string METADATA_KEY("metadata");
const std::string FILE_VERSION_KEY("fileVersion");
const std::string CREATED_KEY("created");
const std::string EDITED_KEY("edited");
const std::string AUDACITY_VERSION_KEY("audacityVersion");
const std::string NAME_KEY("name");
const std::string STEPS_KEY("steps");
const std::string COMMAND_KEY("command");
const std::string PARAMS_KEY("params");
const std::string MACRO_KEY("macro");

const std::string FILE_VERSION("1.0");

JsonObject metadataToJson(const std::optional<MacroMetadata>& metadata)
{
    JsonObject object;
    object.set(FILE_VERSION_KEY, FILE_VERSION);

    if (metadata) {
        object.set(CREATED_KEY, metadata->created.toString());
        object.set(EDITED_KEY, metadata->edited.toString());
        object.set(AUDACITY_VERSION_KEY, metadata->audacityVersion);
    }

    return object;
}

std::optional<MacroMetadata> metadataFromJson(const JsonObject& object)
{
    const JsonValue created = object.value(CREATED_KEY);
    const JsonValue edited = object.value(EDITED_KEY);
    const JsonValue audacityVersion = object.value(AUDACITY_VERSION_KEY);
    if (!created.isString() || !edited.isString() || !audacityVersion.isString()) {
        return std::nullopt;
    }

    return MacroMetadata {
        DateTime::fromStringISOFormat(created.toString()),
        DateTime::fromStringISOFormat(edited.toString()),
        audacityVersion.toStdString()
    };
}
}

JsonObject au::macromanager::toJson(const Macro& macro)
{
    JsonArray steps;
    for (const MacroItem& item : macro.steps) {
        JsonObject stepObject;

        if (const MacroStep* step = std::get_if<MacroStep>(&item)) {
            stepObject.set(COMMAND_KEY, step->command.toString());
            stepObject.set(PARAMS_KEY, JsonValue::fromVal(Val(ValMap(step->params))));
        } else {
            stepObject.set(MACRO_KEY, std::get<MacroReference>(item).name);
        }

        steps << stepObject;
    }

    JsonObject object;
    object.set(METADATA_KEY, metadataToJson(macro.metadata));
    object.set(NAME_KEY, macro.name);
    object.set(STEPS_KEY, steps);

    return object;
}

RetVal<Macro> au::macromanager::macroFromJson(const JsonObject& object)
{
    const JsonValue name = object.value(NAME_KEY);
    if (!name.isString() || name.toStdString().empty()) {
        return RetVal<Macro>(make_ret(Ret::Code::UnknownError, trc("macromanager", "macro has no name")));
    }

    Macro macro;
    macro.name = name.toStdString();
    macro.metadata = metadataFromJson(object.value(METADATA_KEY).toObject());

    const JsonArray steps = object.value(STEPS_KEY).toArray();
    for (size_t i = 0; i < steps.size(); ++i) {
        const JsonObject stepObject = steps.at(i).toObject();

        if (stepObject.contains(MACRO_KEY)) {
            const std::string macroName = stepObject.value(MACRO_KEY).toStdString();
            if (macroName.empty()) {
                return RetVal<Macro>(make_ret(Ret::Code::UnknownError, trc("macromanager", "macro step has no macro name")));
            }

            macro.steps.push_back(MacroReference { macroName });
            continue;
        }

        const Command command(stepObject.value(COMMAND_KEY).toStdString());
        if (!command.isValid()) {
            return RetVal<Macro>(make_ret(Ret::Code::UnknownError, trc("macromanager", "macro step has an invalid command")));
        }

        macro.steps.push_back(MacroStep { command, Params(stepObject.value(PARAMS_KEY).toVal().toMap()) });
    }

    return RetVal<Macro>::make_ok(macro);
}
