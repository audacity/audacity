/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectmenumodelbase.h"

using namespace au::projectscene;

RealtimeEffectMenuModelBase::RealtimeEffectMenuModelBase(QObject* parent)
    : AbstractMenuModel(parent) {}

void RealtimeEffectMenuModelBase::load()
{
    AbstractMenuModel::load();

    effectsProvider()->effectMetaListChanged().onNotify(this, [this]
    { populateMenu(); });

    doLoad();
}

au::trackedit::TrackId RealtimeEffectMenuModelBase::trackId() const
{
    return m_trackId.value_or(-1);
}

void RealtimeEffectMenuModelBase::setTrackId(au::trackedit::TrackId trackId)
{
    if (m_trackId == trackId) {
        return;
    }
    beginResetModel();
    m_trackId = trackId;
    endResetModel();
    // TODO: I don't think this is necessary
    emit trackIdChanged();
}
