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
#include "playback/view/common/playbackmetermodel.h"
#include "record/irecordcontroller.h"

namespace au::projectscene {
//! NOTE This is model need for synchronization of view (scroll, height and so on)
//! Operation in context of track (if trackId set)
//! and context of all tracks (if trackId not set)
class TracksViewStateModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    // Context of elements
    Q_PROPERTY(QVariant trackId READ trackId WRITE setTrackId NOTIFY trackIdChanged FINAL)
    Q_PROPERTY(int trackHeight READ trackHeight NOTIFY trackHeightChanged FINAL)
    Q_PROPERTY(bool isTrackCollapsed READ isTrackCollapsed NOTIFY isTrackCollapsedChanged FINAL)

    // Context of user interaction
    Q_PROPERTY(int tracksVericalY READ tracksVericalY NOTIFY tracksVericalYChanged FINAL)
    Q_PROPERTY(bool tracksVerticalScrollLocked READ tracksVerticalScrollLocked NOTIFY tracksVerticalScrollLockedChanged FINAL)
    Q_PROPERTY(int tracksVerticalScrollPadding READ tracksVerticalScrollPadding FINAL)

    Q_PROPERTY(bool altPressed READ altPressed NOTIFY altPressedChanged FINAL)
    Q_PROPERTY(bool ctrlPressed READ ctrlPressed NOTIFY ctrlPressedChanged FINAL)

    Q_PROPERTY(bool isPlaying READ isPlaying NOTIFY isPlayingChanged FINAL)
    Q_PROPERTY(bool isRecording READ isRecording NOTIFY isRecordingChanged FINAL)
    Q_PROPERTY(bool snapEnabled READ snapEnabled NOTIFY snapEnabledChanged FINAL)

    Q_PROPERTY(playback::PlaybackMeterStyle::MeterStyle meterStyle READ meterStyle NOTIFY meterStyleChanged FINAL)
    Q_PROPERTY(playback::PlaybackMeterModel * meterModel READ meterModel NOTIFY meterModelChanged FINAL)

    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<playback::IPlaybackController> playbackController;
    muse::Inject<playback::IPlaybackConfiguration> playbackConfiguration;
    muse::Inject<record::IRecordController> recordController;

public:
    TracksViewStateModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    // Context of elements
    QVariant trackId() const;
    void setTrackId(const QVariant& newTrackId);
    int trackHeight() const;
    bool isTrackCollapsed() const;
    bool tracksVerticalScrollLocked() const;
    int tracksVerticalScrollPadding() const;
    playback::PlaybackMeterStyle::MeterStyle meterStyle() const;
    playback::PlaybackMeterModel* meterModel() const;

    // context of user interaction
    int tracksVericalY() const;
    bool altPressed() const;
    bool ctrlPressed() const;
    bool isPlaying() const;
    bool isRecording() const;

    Q_INVOKABLE void changeTracksVericalY(int deltaY);
    Q_INVOKABLE void setMouseY(double y);

    Q_INVOKABLE void requestVerticalScrollLock();
    Q_INVOKABLE void requestVerticalScrollUnlock();

    Q_INVOKABLE void changeTrackHeight(int deltaY);

    Q_INVOKABLE bool snapEnabled();

signals:
    // Context of elements
    void trackIdChanged();
    void trackHeightChanged();
    void isTrackCollapsedChanged();

    // Context of user interaction
    void tracksVericalYChanged();
    void tracksVerticalScrollLockedChanged();
    void altPressedChanged();
    void ctrlPressedChanged();
    void isPlayingChanged();
    void isRecordingChanged();

    void snapEnabledChanged();

    void meterStyleChanged();
    void meterModelChanged();
private:
    static constexpr int m_tracksVerticalScrollPadding = 228;

    IProjectViewStatePtr viewState() const;

    // Context of elements
    trackedit::TrackId m_trackId = -1;
    muse::ValCh<int> m_trackHeight;
    muse::ValCh<bool> m_isTrackCollapsed;

    // Context of user interaction
    muse::ValCh<int> m_tracksVericalY;
    muse::ValCh<bool> m_tracksVerticalScrollLocked;

    muse::ValCh<bool> m_altPressed;
    muse::ValCh<bool> m_ctrlPressed;

    playback::PlaybackMeterModel* m_meterModel = nullptr;

    bool m_snapEnabled;
};
}
