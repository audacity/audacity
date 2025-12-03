/*
* Audacity: A Digital Audio Editor
*/
#include "labelstableviewtrackcell.h"

using namespace au::projectscene;

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
    m_trackId = trackId;
}
