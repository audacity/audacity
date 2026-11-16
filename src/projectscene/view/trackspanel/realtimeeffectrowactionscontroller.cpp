/*
* Audacity: A Digital Audio Editor
*/

#include "realtimeeffectrowactionscontroller.h"

using namespace au::projectscene;

RealtimeEffectRowActionsController::RealtimeEffectRowActionsController(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void RealtimeEffectRowActionsController::init()
{
    if (m_initialized) {
        return;
    }

    projectSceneActionsController()->realtimeEffectMoveUpRequested().onNotify(this, [this]() {
        if (m_enabled) {
            emit moveUpRequested();
        }
    });
    projectSceneActionsController()->realtimeEffectMoveDownRequested().onNotify(this, [this]() {
        if (m_enabled) {
            emit moveDownRequested();
        }
    });

    m_initialized = true;
}

bool RealtimeEffectRowActionsController::enabled() const
{
    return m_enabled;
}

void RealtimeEffectRowActionsController::setEnabled(bool enabled)
{
    if (m_enabled == enabled) {
        return;
    }

    m_enabled = enabled;
    emit enabledChanged();
}
