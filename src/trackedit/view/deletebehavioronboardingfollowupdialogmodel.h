/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"

#include <QObject>

namespace au::trackedit {
class DeleteBehaviorOnboardingFollowupDialogModel : public QObject
{
    Q_OBJECT

    muse::Inject<muse::actions::IActionsDispatcher> dispatcher;

public:
    Q_INVOKABLE void openDeleteBehaviorPreferences() const;
    Q_INVOKABLE void openShortcutsPreferences() const;
};
}
