/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>

#include "global/async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "playback/iplaybackcontroller.h"
#include "playback/iplaybackconfiguration.h"
#include "record/irecordcontroller.h"

#include "playback/view/common/playbackmetermodel.h"

namespace au::projectscene {
class TrackViewStateModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)
    Q_PROPERTY(int trackHeight READ trackHeight NOTIFY trackHeightChanged FINAL)
    Q_PROPERTY(bool isTrackCollapsed READ isTrackCollapsed NOTIFY isTrackCollapsedChanged FINAL)
    Q_PROPERTY(double channelHeightRatio READ channelHeightRatio NOTIFY channelHeightRatioChanged FINAL)

    Q_PROPERTY(bool isPlaying READ isPlaying NOTIFY isPlayingChanged FINAL)
    Q_PROPERTY(bool isRecording READ isRecording NOTIFY isRecordingChanged FINAL)

    Q_PROPERTY(playback::PlaybackMeterModel * meterModel READ meterModel NOTIFY meterModelChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<playback::IPlaybackController> playbackController;
    muse::Inject<playback::IPlaybackConfiguration> playbackConfiguration;
    muse::Inject<record::IRecordController> recordController;

public:
    TrackViewStateModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    QVariant trackId() const;
    void setTrackId(const QVariant& newTrackId);

    int trackHeight() const;
    Q_INVOKABLE void changeTrackHeight(int deltaY);

    bool isTrackCollapsed() const;
    double channelHeightRatio() const;
    Q_INVOKABLE void changeChannelHeightRatio(double ratio);

    playback::PlaybackMeterModel* meterModel() const;

    bool isPlaying() const;
    bool isRecording() const;

signals:
    void trackIdChanged();
    void trackHeightChanged();
    void isTrackCollapsedChanged();
    void channelHeightRatioChanged();
    void meterModelChanged();
    void isPlayingChanged();
    void isRecordingChanged();

private:
    IProjectViewStatePtr viewState() const;

    trackedit::TrackId m_trackId = -1;
    muse::ValCh<int> m_trackHeight;
    muse::ValCh<bool> m_isTrackCollapsed;
    muse::ValCh<double> m_channelHeightRatio;

    playback::PlaybackMeterModel* m_meterModel = nullptr;
};
}
