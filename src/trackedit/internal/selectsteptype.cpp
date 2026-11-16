/*
* Audacity: A Digital Audio Editor
*/
#include "selectsteptype.h"

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

std::string SelectEnhancedStepType::id() const
{
    return SELECT_ENHANCED_STEP_TYPE;
}

TranslatableString SelectEnhancedStepType::title() const
{
    return SELECT_ENHANCED_STEP_TITLE;
}

String SelectEnhancedStepType::summary(const Val& data) const
{
    const RetVal<SelectEnhancedStep> step = selectStepFromVal(data);
    if (!step.ret) {
        return String::fromStdString(step.ret.text());
    }

    return selectStepSummary(step.val);
}

Val SelectEnhancedStepType::defaultData() const
{
    return toVal(SelectEnhancedStep {});
}

std::optional<UriQuery> SelectEnhancedStepType::editUri() const
{
    return UriQuery(SELECT_STEP_DIALOG_URI);
}

async::Promise<Ret> SelectEnhancedStepType::execute(const Val& data) const
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
