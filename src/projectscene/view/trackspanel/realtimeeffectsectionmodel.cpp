/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectsectionmodel.h"

using namespace muse;
using namespace au::projectscene;

RealtimeEffectSectionModel::RealtimeEffectSectionModel(QObject* parent)
    : QObject(parent)
{
}

void RealtimeEffectSectionModel::load()
{
    configuration()->isEffectsPanelVisibleChanged().onNotify(this, [this]() {
        emit showEffectsSectionChanged();
    });

    dispatcher()->reg(this, "toggle-effects", [this] {
        configuration()->setIsEffectsPanelVisible(!configuration()->isEffectsPanelVisible());
    });

    dispatcher()->reg(this, "add-realtime-effects", [this] {
        configuration()->setIsEffectsPanelVisible(!configuration()->isEffectsPanelVisible());
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
