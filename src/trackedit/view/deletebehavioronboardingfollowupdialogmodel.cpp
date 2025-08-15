/*
 * Audacity: A Digital Audio Editor
 */

#include "deletebehavioronboardingfollowupdialogmodel.h"

namespace au::trackedit {
void DeleteBehaviorOnboardingFollowupDialogModel::openDeleteBehaviorPreferences() const
{
    dispatcher()->dispatch("preference-dialog");
}

void DeleteBehaviorOnboardingFollowupDialogModel::openShortcutsPreferences() const
{
    dispatcher()->dispatch("preference-dialog");
}
}
