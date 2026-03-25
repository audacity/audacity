/*
* Audacity: A Digital Audio Editor
*/

#include "realtimeeffectrowactionscontroller.h"

using namespace au::projectscene;
using namespace muse::actions;

static const ActionCode REALTIME_EFFECT_MOVE_UP_CODE("realtime-effect-move-up");
static const ActionCode REALTIME_EFFECT_MOVE_DOWN_CODE("realtime-effect-move-down");

RealtimeEffectRowActionsController::RealtimeEffectRowActionsController(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void RealtimeEffectRowActionsController::init()
{
    if (m_initialized) {
        return;
    }

    dispatcher()->reg(this, REALTIME_EFFECT_MOVE_UP_CODE, [this]() {
        emit moveUpRequested();
    });
    dispatcher()->reg(this, REALTIME_EFFECT_MOVE_DOWN_CODE, [this]() {
        emit moveDownRequested();
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

bool RealtimeEffectRowActionsController::canReceiveAction(const ActionCode&) const
{
    return m_enabled;
}
