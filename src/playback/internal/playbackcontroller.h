/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/async/asyncable.h"
#include "framework/global/async/channel.h"
#include "framework/global/iapplication.h"
#include "framework/actions/actionable.h"
#include "framework/actions/iactionsdispatcher.h"

#include "audio/iaudiodevicesprovider.h"
#include "context/iglobalcontext.h"
#include "playback/iplaybackconfiguration.h"
#include "playback/iplayback.h"
#include "playback/itransport.h"
#include "record/irecordcontroller.h"

namespace au::playback {
class PlaybackController : public muse::actions::Actionable, public muse::async::Asyncable, public muse::Contextable
{
public:
    muse::GlobalInject<au::playback::IPlaybackConfiguration> playbackConfiguration;
    muse::GlobalInject<muse::IApplication> application;

    muse::ContextInject<au::context::IGlobalContext> globalContext { this };
    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::ContextInject<record::IRecordController> recordController { this };
    muse::ContextInject<audio::IAudioDevicesProvider> audioDevicesProvider { this };
    muse::ContextInject<playback::IPlayback> playback { this };
    muse::ContextInject<playback::ITransport> transport { this };

public:
    PlaybackController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void init();
    void deinit();

    bool actionChecked(const muse::actions::ActionCode& actionCode) const;
    muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const;

    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

private:
    friend class TransportTests;

    void togglePlayAction();
    void playTracksAction(const muse::actions::ActionQuery& q);
    void pauseAction();
    void stopAction();
    void rewindToStartAction();
    void rewindToEndAction();
    void onSeekAction(const muse::actions::ActionQuery& q);
    void onChangePlaybackRegionAction(const muse::actions::ActionQuery& q);

    void togglePlayRepeats();
    void toggleAutomaticallyPan();

    void toggleLoopPlayback();
    void clearLoopRegion();
    void setLoopRegionToSelection();
    void setSelectionToLoop();
    void setLoopRegionInOut();
    void setSelectionFollowsLoopRegion();

    // Audio device/configuration actions: resolve the selected index against the
    // current device list, then call the matching method on the transport.
    void setAudioApi(const muse::actions::ActionQuery& q);
    void setAudioOutputDevice(const muse::actions::ActionQuery& q);
    void setAudioInputDevice(const muse::actions::ActionQuery& q);
    void setInputChannels(const muse::actions::ActionQuery& q);
    void rescanAudioDevices();

    void changeAudioDeviceFromQuery(const muse::actions::ActionQuery& q, const std::string& indexParam,
                                    const std::vector<std::string>& options, const std::function<void(const std::string&)>& applyValue);

    void notifyActionCheckedChanged(const muse::actions::ActionCode& actionCode);

    playback::IPlayerPtr m_player;
    muse::async::Channel<muse::actions::ActionCode> m_actionCheckedChanged;
};
}
