/*
 * Audacity: A Digital Audio Editor
 */
#include "deletebehavioronboardingscenario.h"

namespace au::trackedit {
bool DeleteBehaviorOnboardingScenario::showOnboardingDialog() const
{
    muse::UriQuery uri("audacity://trackedit/delete_behavior");
    uri.addParam("deleteBehavior", muse::Val(static_cast<int>(DeleteBehavior::LeaveGap)));
    uri.addParam("closeGapBehavior", muse::Val(static_cast<int>(configuration()->closeGapBehavior())));
    const muse::RetVal<muse::Val> rv = interactive()->openSync(uri);
    if (!rv.ret.success()) {
        return false;
    }
    const muse::ValMap map = rv.val.toMap();
    IF_ASSERT_FAILED(map.count("deleteBehavior") && map.count("closeGapBehavior")) {
        return false;
    }
    const auto deleteBehavior = static_cast<DeleteBehavior>(map.at("deleteBehavior").toInt());
    IF_ASSERT_FAILED(deleteBehavior != DeleteBehavior::NotSet) {
        return false;
    }
    const auto closeGapBehavior = static_cast<CloseGapBehavior>(map.at("closeGapBehavior").toInt());
    configuration()->setDeleteBehavior(deleteBehavior);
    configuration()->setCloseGapBehavior(closeGapBehavior);

    return true;
}

void DeleteBehaviorOnboardingScenario::showFollowupDialog() const
{
    const muse::UriQuery followupUri("audacity://trackedit/delete_behavior_followup");
    const muse::RetVal<muse::Val> rv = interactive()->openSync(followupUri);
    const std::string action = rv.val.toString();
    if (!action.empty()) {
        IF_ASSERT_FAILED(actionExists(action)) {
            return;
        }
        dispatcher()->dispatch(muse::actions::ActionCode(action));
    }
}

bool DeleteBehaviorOnboardingScenario::actionExists(const std::string& action) const
{
    const auto actionList = dispatcher()->actionList();
    return std::find(actionList.begin(), actionList.end(), action) != actionList.end();
}
}
