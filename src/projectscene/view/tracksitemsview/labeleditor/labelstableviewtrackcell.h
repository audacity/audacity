/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "uicomponents/qml/Muse/UiComponents/internal/tableviewcell.h" // todo: public?
#include "uicomponents/qml/Muse/UiComponents/menuitem.h"

#include "trackedit/trackedittypes.h"

namespace au::projectscene {
class LabelsTableViewTrackCell : public muse::uicomponents::TableViewCell
{
    Q_OBJECT
    QML_ELEMENT;

    Q_PROPERTY(muse::uicomponents::MenuItemList availableTracks READ availableTracks NOTIFY availableTracksChanged FINAL)

    Q_PROPERTY(au::trackedit::TrackId currentTrackId READ currentTrackId NOTIFY valueChanged FINAL)

public:
    explicit LabelsTableViewTrackCell(QObject* parent = nullptr);
    LabelsTableViewTrackCell(const TableViewCell* other);

    muse::uicomponents::MenuItemList availableTracks() const;
    void setAvailableTracks(const muse::uicomponents::MenuItemList& tracks);

    trackedit::TrackId currentTrackId() const;
    void setCurrentTrackId(const trackedit::TrackId& trackId);

signals:
    void availableTracksChanged();

private:
    muse::uicomponents::MenuItemList m_availableTracks;
    trackedit::TrackId m_trackId;
};
}
