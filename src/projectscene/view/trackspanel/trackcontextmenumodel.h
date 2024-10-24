/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "uicomponents/view/abstractmenumodel.h"
#include "types/projectscenetypes.h"

namespace au::projectscene {
class TrackContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT
    Q_PROPERTY(trackedit::TrackId trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)

public:
    TrackContextMenuModel() = default;

    Q_INVOKABLE void load() override;

    trackedit::TrackId trackId() const;
    void setTrackId(const trackedit::TrackId& newTrackId);
    void handleMenuItem(const QString& itemId) override;

signals:
    void trackIdChanged();
    void trackRenameRequested();

private:
    trackedit::TrackId m_trackId;
};
}
