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
    configuration()->isEffectsPanelVisible().ch.onReceive(this, [this](bool)
    {
        emit showEffectsSectionChanged();
    });

    dispatcher()->reg(this, "toggle-effects", [this] {
        configuration()->setIsEffectsPanelVisible(!configuration()->isEffectsPanelVisible().val);
    });

    dispatcher()->reg(this, "add-realtime-effects", [this] {
        configuration()->setIsEffectsPanelVisible(!configuration()->isEffectsPanelVisible().val);
    });
}

bool RealtimeEffectSectionModel::prop_showEffectsSection() const
{
    return configuration()->isEffectsPanelVisible().val;
}

void RealtimeEffectSectionModel::prop_setShowEffectsSection(bool show)
{
    configuration()->setIsEffectsPanelVisible(show);
}
