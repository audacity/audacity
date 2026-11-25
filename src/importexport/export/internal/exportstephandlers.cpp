/*
* Audacity: A Digital Audio Editor
*/
#include "exportstephandlers.h"

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

async::Promise<Ret> ExportStepHandler::runExport(const ExportFormatCommand& command) const
{
    return std::visit([this](const auto& c) { return commandDispatcher()->dispatch(c); }, command)
           .then<Ret>(this, [](const Response& response, auto resolve) {
        return resolve(response.ret);
    });
}

std::string ExportAudioStepHandler::id() const
{
    return EXPORT_AUDIO_STEP_HANDLER_ID;
}

TranslatableString ExportAudioStepHandler::title() const
{
    return EXPORT_AUDIO_STEP_TITLE;
}

String ExportAudioStepHandler::summary(const Val& data) const
{
    const RetVal<ExportFormatCommand> command = formatCommandFromVal(data);
    if (!command.ret) {
        return String::fromStdString(command.ret.text());
    }

    return String::fromStdString(formatSummary(command.val, *exporter()));
}

Val ExportAudioStepHandler::defaultData() const
{
    return toVal(ExportFormatCommand(ExportWavCommand {}));
}

std::optional<UriQuery> ExportAudioStepHandler::editUri() const
{
    return UriQuery(EXPORT_AUDIO_STEP_DIALOG_URI);
}

async::Promise<Ret> ExportAudioStepHandler::execute(const Val& data) const
{
    const RetVal<ExportFormatCommand> command = formatCommandFromVal(data);
    if (!command.ret) {
        return resolved(command.ret);
    }

    return runExport(command.val);
}
