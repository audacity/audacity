/*
* Audacity: A Digital Audio Editor
*/
#include "exportsteptypes.h"

#include <algorithm>

#include "framework/global/translation.h"


using namespace au::importexport;
using namespace muse;
using namespace muse::rcommand;

namespace {
async::Promise<Ret> resolved(const Ret& ret)
{
    return async::make_promise<Ret>([ret](auto resolve) {
        return resolve(ret);
    });
}
}

async::Promise<Ret> ExportStepType::runExport(const ExportFormatCommand& command) const
{
    return std::visit([this](const auto& c) { return commandDispatcher()->dispatch(c); }, command)
           .then<Ret>(this, [](const Response& response, auto resolve) {
        return resolve(response.ret);
    });
}

std::string ExportAudioStepType::id() const
{
    return EXPORT_AUDIO_STEP_TYPE;
}

TranslatableString ExportAudioStepType::title() const
{
    return EXPORT_AUDIO_STEP_TITLE;
}

String ExportAudioStepType::summary(const Val& data) const
{
    const RetVal<ExportFormatCommand> command = formatCommandFromVal(data);
    if (!command.ret) {
        return String::fromStdString(command.ret.text());
    }

    return String::fromStdString(formatSummary(command.val));
}

Val ExportAudioStepType::defaultData() const
{
    return toVal(ExportFormatCommand(ExportWavCommand {}));
}

std::optional<UriQuery> ExportAudioStepType::editUri() const
{
    return UriQuery(EXPORT_AUDIO_STEP_DIALOG_URI);
}

async::Promise<Ret> ExportAudioStepType::execute(const Val& data) const
{
    const RetVal<ExportFormatCommand> command = formatCommandFromVal(data);
    if (!command.ret) {
        return resolved(command.ret);
    }

    return runExport(command.val);
}

std::string ExportSimpleStepType::id() const
{
    return EXPORT_SIMPLE_STEP_TYPE;
}

TranslatableString ExportSimpleStepType::title() const
{
    return EXPORT_SIMPLE_STEP_TITLE;
}

String ExportSimpleStepType::summary(const Val& data) const
{
    const RetVal<ExportSimpleStep> step = simpleStepFromVal(data);
    if (!step.ret) {
        return String::fromStdString(step.ret.text());
    }

    const auto& presets = ExportPresetContract::table;
    const auto it = std::ranges::find(presets, step.val.preset, [](const auto& entry) { return entry.eval; });
    return String::fromStdString(std::string(it->description));
}

Val ExportSimpleStepType::defaultData() const
{
    return toVal(ExportSimpleStep {});
}

std::optional<UriQuery> ExportSimpleStepType::editUri() const
{
    return UriQuery(EXPORT_SIMPLE_STEP_DIALOG_URI);
}

async::Promise<Ret> ExportSimpleStepType::execute(const Val& data) const
{
    const RetVal<ExportSimpleStep> step = simpleStepFromVal(data);
    if (!step.ret) {
        return resolved(step.ret);
    }

    const ExportPresetSettings preset = presetSettings(step.val.preset);
    const std::string projectRate = EnumContract<ExportSampleRate>::str_val(ExportSampleRate::ProjectRate);

    return runExport(formatCommand(preset.settings, projectRate, preset.channels, step.val.folder, step.val.fileName));
}
