/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectsectionmodel.h"

using namespace muse;
using namespace au::projectscene;

RealtimeEffectSectionModel::RealtimeEffectSectionModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void RealtimeEffectSectionModel::load()
{
    configuration()->isEffectsPanelVisibleChanged().onNotify(this, [this]() {
        emit showEffectsSectionChanged();
    });

    dispatcher()->reg(this, "toggle-effects", [this] {
        const bool shouldShow = !configuration()->isEffectsPanelVisible();
        configuration()->setIsEffectsPanelVisible(shouldShow);
        if (shouldShow) {
            emit focusEffectsPanelRequested();
        }
    });

    dispatcher()->reg(this, "add-realtime-effects", [this] {
        const bool shouldShow = !configuration()->isEffectsPanelVisible();
        configuration()->setIsEffectsPanelVisible(shouldShow);
        if (shouldShow) {
            emit focusEffectsPanelRequested();
        }
    });

    emit showEffectsSectionChanged();
}

bool RealtimeEffectSectionModel::prop_showEffectsSection() const
{
    return configuration()->isEffectsPanelVisible();
}

void RealtimeEffectSectionModel::prop_setShowEffectsSection(bool show)
{
    configuration()->setIsEffectsPanelVisible(show);
}
