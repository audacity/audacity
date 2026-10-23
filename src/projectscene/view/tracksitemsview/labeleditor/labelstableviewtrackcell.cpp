/*
* Audacity: A Digital Audio Editor
*/
#include "labelstableviewtrackcell.h"

#include "trackedit/trackeditcommands.h"

using namespace au::projectscene;

std::optional<au::trackedit::TrackId> LabelsTableViewTrackCell::itemTrackId(const muse::uicomponents::MenuItem& item)
{
    const muse::rcommand::Params params = item.params();
    if (!params.contains(trackedit::TRACKEDIT_TRACK_ID_PARAM)) {
        return std::nullopt;
    }

    return trackedit::TrackId(params.at(trackedit::TRACKEDIT_TRACK_ID_PARAM).toInt64());
}

LabelsTableViewTrackCell::LabelsTableViewTrackCell(QObject* parent)
    : muse::uicomponents::TableViewCell(parent)
{
}

LabelsTableViewTrackCell::LabelsTableViewTrackCell(const TableViewCell* other)
{
    setValue(other->value());
}

muse::uicomponents::MenuItemList LabelsTableViewTrackCell::availableTracks() const
{
    return m_availableTracks;
}

void LabelsTableViewTrackCell::setAvailableTracks(const muse::uicomponents::MenuItemList& tracks)
{
    if (m_availableTracks == tracks) {
        return;
    }

    m_availableTracks = tracks;
    emit availableTracksChanged();
}

au::trackedit::TrackId LabelsTableViewTrackCell::currentTrackId() const
{
    return m_trackId;
}

void LabelsTableViewTrackCell::setCurrentTrackId(const trackedit::TrackId& trackId)
{
    if (m_trackId == trackId) {
        return;
    }

    m_trackId = trackId;

    for (muse::uicomponents::MenuItem* item : m_availableTracks) {
        item->setChecked(itemTrackId(*item) == trackId);
    }
}
