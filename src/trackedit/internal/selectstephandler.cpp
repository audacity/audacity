/*
* Audacity: A Digital Audio Editor
*/
#include "selectstephandler.h"

#include "selectstep.h"

using namespace au::trackedit;
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

std::string SelectEnhancedStepHandler::id() const
{
    return SELECT_ENHANCED_STEP_HANDLER_ID;
}

TranslatableString SelectEnhancedStepHandler::title() const
{
    return SELECT_ENHANCED_STEP_TITLE;
}

String SelectEnhancedStepHandler::summary(const Val& data) const
{
    const RetVal<SelectEnhancedStep> step = selectStepFromVal(data);
    if (!step.ret) {
        return String::fromStdString(step.ret.text());
    }

    return selectStepSummary(step.val);
}

Val SelectEnhancedStepHandler::defaultData() const
{
    return toVal(SelectEnhancedStep {});
}

std::optional<UriQuery> SelectEnhancedStepHandler::editUri() const
{
    return UriQuery(SELECT_STEP_DIALOG_URI);
}

async::Promise<Ret> SelectEnhancedStepHandler::execute(const Val& data) const
{
    const RetVal<SelectEnhancedStep> step = selectStepFromVal(data);
    if (!step.ret) {
        return resolved(step.ret);
    }

    const ITrackeditProjectPtr project = globalContext()->currentTrackeditProject();
    const int trackCount = project ? static_cast<int>(project->trackIdList().size()) : 0;
    return commandDispatcher()->dispatch(selectStepCommand(step.val, trackCount))
           .then<Ret>(this, [](const Response& response, auto resolve) {
        return resolve(response.ret);
    });
}
