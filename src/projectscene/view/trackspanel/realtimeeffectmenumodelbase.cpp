/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectmenumodelbase.h"
#include "effects/effects_base/irealtimeeffectservice.h"

using namespace au::projectscene;

RealtimeEffectMenuModelBase::RealtimeEffectMenuModelBase(QObject* parent)
    : AbstractMenuModel(parent)
{
}

void RealtimeEffectMenuModelBase::load()
{
    AbstractMenuModel::load();

    effectsProvider()->effectMetaListChanged().onNotify(this, [this]
    { doPopulateMenu(); });

    trackSelection()->selectedTrackIdChanged().onNotify(this, [this]
    {
        beginResetModel();
        endResetModel();
        onSelectedTrackIdChanged();
    });

    doLoad();
}

std::optional<au::trackedit::TrackId> RealtimeEffectMenuModelBase::trackId() const
{
    return m_isMasterTrack ? au::effects::IRealtimeEffectService::masterTrackId : trackSelection()->selectedTrackId();
}

void RealtimeEffectMenuModelBase::prop_setIsMasterTrack(bool isMasterTrack)
{
    if (m_isMasterTrack == isMasterTrack) {
        return;
    }
    m_isMasterTrack = isMasterTrack;
    emit isMasterTrackChanged();
}
