/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "context/iglobalcontext.h"

#include "framework/global/modularity/ioc.h"

#include <QObject>

namespace au::spectrogram {
class TrackSpectrogramSettingsDialogModel : public QObject, public muse::Injectable
{
    Q_OBJECT

    Q_PROPERTY(int trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged)

    muse::Inject<au::context::IGlobalContext> globalContext { this };
public:

    TrackSpectrogramSettingsDialogModel(QObject* parent = nullptr);

    int trackId() const { return m_trackId; }
    void setTrackId(int value);

    Q_INVOKABLE void requestSpectrogramUpdate();

signals:
    void trackIdChanged();

private:
    int m_trackId = -1;
};
}
