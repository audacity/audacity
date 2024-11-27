/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "uicomponents/view/abstractmenumodel.h"
#include "trackedit/trackedittypes.h"
#include <QObject>

namespace au::projectscene {
class RtEffectMenuModelBase : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT
    Q_PROPERTY(au::trackedit::TrackId trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged)

public:
    explicit RtEffectMenuModelBase(QObject* parent = nullptr);

    au::trackedit::TrackId trackId() const;
    void setTrackId(au::trackedit::TrackId trackId);

signals:
    void trackIdChanged();

protected:
    au::trackedit::TrackId m_trackId = -1;
};
}
