/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectmenumodelbase.h"

using namespace au::projectscene;

RealtimeEffectMenuModelBase::RealtimeEffectMenuModelBase(QObject* parent)
    : AbstractMenuModel(parent) {}

au::trackedit::TrackId RealtimeEffectMenuModelBase::trackId() const
{
    return m_trackId;
}

void RealtimeEffectMenuModelBase::setTrackId(au::trackedit::TrackId trackId)
{
    if (trackId < 0 || m_trackId == trackId) {
        return;
    }
    beginResetModel();
    m_trackId = trackId;
    endResetModel();
    // TODO: I don't think this is necessary
    emit trackIdChanged();
}
