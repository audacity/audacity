/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectmenumodelbase.h"

using namespace au::projectscene;

RealtimeEffectMenuModelBase::RealtimeEffectMenuModelBase(QObject* parent)
    : AbstractMenuModel(parent)
{
}

void RealtimeEffectMenuModelBase::setTrackId(std::optional<au::trackedit::TrackId> trackId)
{
    if (m_trackId == trackId) {
        return;
    }
    beginResetModel();
    m_trackId = std::move(trackId);
    endResetModel();
    onTrackIdChanged();
}

void RealtimeEffectMenuModelBase::load()
{
    AbstractMenuModel::load();

    effectsProvider()->effectMetaListChanged().onNotify(this, [this]
    { populateMenu(); });

    selectionController()->selectedTrackAdded().onReceive(this, [this](au::trackedit::TrackId trackId)
    { setTrackId(trackId); });

    selectionController()->tracksSelected().onReceive(this, [this](const au::trackedit::TrackIdList& tracks) {
        if (tracks.empty()) {
            setTrackId(std::nullopt);
        } else {
            setTrackId(tracks.back());
        }
    });

    doLoad();
}

const std::optional<au::trackedit::TrackId>& RealtimeEffectMenuModelBase::trackId() const
{
    return m_trackId;
}

void RealtimeEffectMenuModelBase::resetList()
{
    beginResetModel();
    doResetList();
    endResetModel();
}

void RealtimeEffectMenuModelBase::removeTrack(const au::trackedit::TrackId& trackId)
{
    beginResetModel();
    doRemoveTrack(trackId);
    endResetModel();
}

void RealtimeEffectMenuModelBase::beginResetModel()
{
    AbstractMenuModel::beginResetModel();
}

void RealtimeEffectMenuModelBase::endResetModel()
{
    AbstractMenuModel::endResetModel();
}
