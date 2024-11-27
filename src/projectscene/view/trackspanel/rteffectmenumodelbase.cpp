/*
 * Audacity: A Digital Audio Editor
 */
#include "rteffectmenumodelbase.h"

using namespace au::projectscene;

RtEffectMenuModelBase::RtEffectMenuModelBase(QObject* parent)
    : AbstractMenuModel(parent) {}

au::trackedit::TrackId RtEffectMenuModelBase::trackId() const
{
    return m_trackId;
}

void RtEffectMenuModelBase::setTrackId(au::trackedit::TrackId trackId)
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
