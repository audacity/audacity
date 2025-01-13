/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectmenumodelbase.h"

using namespace au::projectscene;

RealtimeEffectMenuModelBase::RealtimeEffectMenuModelBase(QObject* parent)
    : AbstractMenuModel(parent)
{
}

void RealtimeEffectMenuModelBase::load()
{
    AbstractMenuModel::load();

    effectsProvider()->effectMetaListChanged().onNotify(this, [this]
    { populateMenu(); });

    trackSelection()->selectedTrackIdChanged().onNotify(this, [this]
    {
        beginResetModel();
        endResetModel();
        onTrackIdChanged();
    });

    doLoad();
}

std::optional<au::trackedit::TrackId> RealtimeEffectMenuModelBase::trackId() const
{
    return trackSelection()->selectedTrackId();
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
