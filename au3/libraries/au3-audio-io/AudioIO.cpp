/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioIO.cpp

  Copyright 2000-2004:
  Dominic Mazzoni
  Joshua Haberman
  Markus Meyer
  Matt Brubeck

  This program is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

********************************************************************//**

\class AudioIoCallback
\brief AudioIoCallback implements the audio backend's realtime callback.
The callback needs to be responsive, has no GUI, and copies data into and
out of the sound card buffers.  It also sends data to the meters.


*//*****************************************************************//**

\class AudioIO
\brief AudioIO plays and records sound through the system audio backend.

  Great care and attention to detail are necessary for understanding and
  modifying this system.  The code in this file is run from three
  different thread contexts: the UI thread, the disk thread (which
  this file creates and maintains; in the code, this is called the
  Audio Thread), and the audio backend's callback thread.
  To highlight this deliniation, the file is divided into three parts
  based on what thread context each function is intended to run in.

  \todo run through all functions called from audio threads
  to verify they are thread-safe. Note that synchronization of the style:
  "A sets flag to signal B, B clears flag to acknowledge completion"
  is not thread safe in a general multiple-CPU context. For example,
  B can write to a buffer and set a completion flag. The flag write can
  occur before the buffer write due to out-of-order execution. Then A
  can see the flag and read the buffer before buffer writes complete.

*//****************************************************************//**

\class AudioIOListener
\brief Monitors record play start/stop and new sample blocks.  Has
callbacks for these events.

*//****************************************************************//**

\class AudioIOStartStreamOptions
\brief struct holding stream options, including a pointer to the
time warp info and AudioIOListener and whether the playback is looped.

*//*******************************************************************/
#include "AudioIO.h"

#include "AudioIOExt.h"
#include "AudioIOListener.h"

#include "au3-math/float_cast.h"
#include "au3-math/Resample.h"
#include "au3-math/Gain.h"
#include "au3-audio-devices/DeviceManager.h"

#include <cfloat>
#include <cstring>
#include <math.h>
#include <stdlib.h>
#include <algorithm>
#include <numeric>
#include <optional>

#ifdef __WXMSW__
#include <malloc.h>
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include "au3-audio-devices/RtAudioBackend.h"

#include <wx/wxcrtvararg.h>
#include <wx/log.h>
#include <wx/time.h>
#include <wx/debug.h>

#if defined(__WXMAC__) || defined(__WXMSW__)
#include <wx/power.h>
#endif

#include "au3-channel/Channel.h"
#include "au3-mixer/Mix.h"
#include "RingBuffer.h"
#include "au3-project-rate/Decibels.h"
#include "au3-preferences/Prefs.h"
#include "au3-project/Project.h"
#include "au3-transactions/TransactionScope.h"

#include "au3-realtime-effects/RealtimeEffectManager.h"
#include "au3-project-rate/QualitySettings.h"
#include "au3-basic-ui/BasicUI.h"
#include "au3-wave-track/WaveTrack.h"

namespace {
float GetAbsValue(const float* buffer, size_t frames, size_t step)
{
    auto sptr = buffer;
    float peak = -1.0f;

    for (unsigned long i = 0; i < frames; i++) {
        peak = std::max(peak, fabs(*sptr));
        sptr += step;
    }
    std::clamp(peak, -1.0f, 1.0f);

    return peak;
}
}

AudioIO* AudioIO::Get()
{
    return static_cast< AudioIO* >(AudioIOBase::Get());
}

AudioIoCallback::Track::Track(std::shared_ptr<const PlayableSequence> seq)
    : mSequence(seq)
{
    assert(seq && seq->FindChannelGroup());
}

AudioIoCallback::Track::~Track() = default;

int64_t AudioIoCallback::Track::trackId() const
{
    const auto* waveTrack = dynamic_cast<const WaveTrack*>(mSequence->FindChannelGroup());
    return waveTrack ? waveTrack->GetId() : -1;
}

struct AudioIoCallback::TransportState {
    TransportState(std::weak_ptr<AudacityProject> wOwningProject,
                   const ConstPlayableSequences& playbackSequences,
                   unsigned numPlaybackChannels, double sampleRate, size_t audioThreadBufferSize)
    {
        if (auto pOwningProject = wOwningProject.lock();
            pOwningProject && numPlaybackChannels > 0) {
            // Setup for realtime playback at the rate of the realtime
            // stream, not the rate of the sample sequence.
            mpRealtimeInitialization.emplace(
                move(wOwningProject), sampleRate, numPlaybackChannels, audioThreadBufferSize);
            // The following adds a new effect processor for each logical sequence.
            for (size_t i = 0, cnt = playbackSequences.size(); i < cnt; ++i) {
                // An array only of non-null pointers should be given to us
                const auto vt = playbackSequences[i].get();
                const auto pGroup = vt ? vt->FindChannelGroup() : nullptr;
                if (!pGroup) {
                    assert(false);
                    continue;
                }
                mpRealtimeInitialization
                ->AddGroup(*pGroup, numPlaybackChannels, sampleRate, audioThreadBufferSize);
            }
        }
    }

    std::optional<RealtimeEffects::InitializationScope> mpRealtimeInitialization;
};

// static
int AudioIoCallback::mNextStreamToken = 0;
double AudioIoCallback::mCachedBestRateOut;
bool AudioIoCallback::mCachedBestRatePlaying;
bool AudioIoCallback::mCachedBestRateCapturing;

#ifdef __WXGTK__
// Might #define this for a useful thing on Linux
   #undef REALTIME_ALSA_THREAD
#else
// never on the other operating systems
   #undef REALTIME_ALSA_THREAD
#endif

//////////////////////////////////////////////////////////////////////
//
//     UI Thread Context
//
//////////////////////////////////////////////////////////////////////

void AudioIO::Init()
{
    auto pAudioIO = safenew AudioIO();
    ugAudioIO.reset(pAudioIO);
    pAudioIO->StartThread();
}

void AudioIO::Deinit()
{
    ugAudioIO.reset();
}

bool AudioIO::ValidateDeviceNames(const wxString& play, const wxString& rec)
{
    const int p = getPlayDevIndex(play);
    const int r = getRecordDevIndex(rec);
    if (p < 0 || r < 0) {
        return false;
    }
    // Both devices must live under the same compiled api — RtAudio does not
    // mix apis in a single stream.
    return audacity::rta::apiIndexFromEncoded(p)
        == audacity::rta::apiIndexFromEncoded(r);
}

AudioIO::AudioIO()
{
    if (!std::atomic<double> {}.is_lock_free()) {
        // If this check fails, then the atomic<double> members in AudioIO.h
        // might be changed to atomic<float> to be more efficient with some
        // loss of precision.  That could be conditionally compiled depending
        // on the platform.
        wxASSERT(false);
    }

    // This ASSERT because of casting in the callback
    // functions where we cast a tempFloats buffer to a (short*) buffer.
    // We have to ASSERT in the GUI thread, if we are to see it properly.
    wxASSERT(sizeof(short) <= sizeof(float));

    mAudioThreadShouldCallSequenceBufferExchangeOnce
    .store(false, std::memory_order_relaxed);
    mAudioThreadSequenceBufferExchangeLoopRunning
    .store(false, std::memory_order_relaxed);
    mAudioThreadSequenceBufferExchangeLoopActive
    .store(false, std::memory_order_relaxed);

    mAudioThreadAcknowledge.store(Acknowledge::eNone, std::memory_order_relaxed);

    mNumPauseFrames = 0;
    mLastRecordingOffset = 0.0;
    mNumCaptureChannels = 0;
    mSilenceLevel = 0.0;

    ResetMeters();

    // RtAudio doesn't need a global init — instances do their own setup.
    // Device enumeration availability is checked on first DeviceManager use.

    SetMixerOutputVol(AudioIOPlaybackVolume.Read());

    mLastPlaybackTimeMillis = 0;
}

void AudioIO::StartThread()
{
    mAudioThread = std::thread(AudioThread, ref(mFinishAudioThread));
}

AudioIO::~AudioIO()
{
    if (!mOwningProject.expired()) {
        // Unlikely that this will be destroyed earlier than any projects, but
        // be prepared anyway
        ResetOwningProject();
    }

    mStream.close();

    /* Delete is a "graceful" way to stop the thread.
       (Kill is the not-graceful way.) */

    // This causes reentrancy issues during application shutdown
    // wxTheApp->Yield();

    mFinishAudioThread.store(true, std::memory_order_release);
    mAudioThread.join();
}

std::shared_ptr<RealtimeEffectState>
AudioIO::AddState(AudacityProject& project,
                  ChannelGroup* pGroup, const PluginID& id)
{
    RealtimeEffects::InitializationScope* pInit = nullptr;
    if (mpTransportState && mpTransportState->mpRealtimeInitialization) {
        if (auto pProject = GetOwningProject(); pProject.get() == &project) {
            pInit = &*mpTransportState->mpRealtimeInitialization;
        }
    }
    return RealtimeEffectManager::Get(project).AddState(pInit, pGroup, id);
}

std::shared_ptr<RealtimeEffectState>
AudioIO::ReplaceState(AudacityProject& project,
                      ChannelGroup* pGroup, size_t index, const PluginID& id)
{
    RealtimeEffects::InitializationScope* pInit = nullptr;
    if (mpTransportState && mpTransportState->mpRealtimeInitialization) {
        if (auto pProject = GetOwningProject(); pProject.get() == &project) {
            pInit = &*mpTransportState->mpRealtimeInitialization;
        }
    }
    return RealtimeEffectManager::Get(project)
           .ReplaceState(pInit, pGroup, index, id);
}

void AudioIO::RemoveState(AudacityProject& project,
                          ChannelGroup* pGroup,
                          const std::shared_ptr<RealtimeEffectState> pState)
{
    RealtimeEffects::InitializationScope* pInit = nullptr;
    if (mpTransportState && mpTransportState->mpRealtimeInitialization) {
        if (auto pProject = GetOwningProject(); pProject.get() == &project) {
            pInit = &*mpTransportState->mpRealtimeInitialization;
        }
    }
    RealtimeEffectManager::Get(project).RemoveState(pInit, pGroup, pState);
}

void AudioIO::SetMixer(int /*inputSource*/, float /*recordVolume*/,
                       float playbackVolume)
{
    SetMixerOutputVol(playbackVolume);
    AudioIOPlaybackVolume.Write(playbackVolume);

    // Input source/volume are not surfaced under the RtAudio backend (no
    // hardware-mixer equivalent). Record-side gain is applied in software in
    // the audio callback if needed.
}

void AudioIO::GetMixer(int* recordDevice, float* recordVolume,
                       float* playbackVolume)
{
    *playbackVolume = GetMixerOutputVol();
    *recordDevice = 0;
    *recordVolume = 1.0f;
}

bool AudioIO::InputMixerWorks()
{
    return mInputMixerWorks;
}

wxArrayString AudioIO::GetInputSourceNames()
{
    return {};
}

bool AudioIO::StartAudioStream(const AudioIOStartStreamOptions& options,
                                   unsigned int numPlaybackChannels, unsigned int numCaptureChannels)
{
    namespace rta = audacity::rta;

    auto sampleRate = options.rate;
    mNumPauseFrames = 0;
    SetOwningProject(options.pProject);
    bool success = false;
    auto cleanup = finally([&]{
        if (!success) {
            ResetOwningProject();
        }
    });

    if (mOwningProject.expired()) {
        return false;
    }

    ResetMeters();

    // Pick a rate that both devices support; 0.0 means no common rate.
    mRate = GetBestRate(numCaptureChannels > 0, numPlaybackChannels > 0, sampleRate);

    // RtAudio cannot mix apis in a single stream and uses float32 throughout
    // — match what AudioIO mixes into. Internal int24 conversion is preserved.
    if (mCaptureFormat == int24Sample) {
        mCaptureFormat = floatSample;
    }

    mNumPlaybackChannels = numPlaybackChannels;
    mNumCaptureChannels  = numCaptureChannels;

    const int playEncoded = (numPlaybackChannels > 0) ? getPlayDevIndex()   : -1;
    const int recEncoded  = (numCaptureChannels  > 0) ? getRecordDevIndex() : -1;

    int apiIndex = -1;
    if (playEncoded >= 0) {
        apiIndex = rta::apiIndexFromEncoded(playEncoded);
    } else if (recEncoded >= 0) {
        apiIndex = rta::apiIndexFromEncoded(recEncoded);
    }
    if (apiIndex < 0) {
        return false;
    }

    const RtAudio::Api api = rta::apiFromIndex(apiIndex);
    if (api == RtAudio::UNSPECIFIED) {
        return false;
    }

    // Track ALSA/JACK so downstream code can still branch on it for known
    // quirks (latency-trust, real-time scheduling intent).
    mUsingAlsa = (api == RtAudio::LINUX_ALSA);
    mUsingJack = (api == RtAudio::UNIX_JACK);

    if (numPlaybackChannels > 0) {
        mOutputMeter = options.playbackMeter;
    }
    if (numCaptureChannels > 0) {
        SetCaptureMeter(mOwningProject.lock(), options.captureMeter);
    }

    StartMeters();

    const auto latencyDuration = AudioIOLatencyDuration.Read();

    rta::DuplexStream::Config cfg;
    cfg.api             = api;
    cfg.inputDeviceId   = (recEncoded  >= 0) ? rta::deviceIdFromEncoded(recEncoded)  : rta::DuplexStream::kNoDevice;
    cfg.outputDeviceId  = (playEncoded >= 0) ? rta::deviceIdFromEncoded(playEncoded) : rta::DuplexStream::kNoDevice;
    cfg.inputChannels   = numCaptureChannels;
    cfg.outputChannels  = numPlaybackChannels;
    cfg.sampleRate      = static_cast<unsigned int>(mRate);
    // Translate the user's latency-ms preference to a buffer size in frames,
    // clamped to a power-of-two-ish range RtAudio's hosts negotiate well.
    {
        unsigned int frames = static_cast<unsigned int>((latencyDuration / 1000.0) * mRate);
        if (frames < 64)   frames = 64;
        if (frames > 8192) frames = 8192;
        cfg.bufferFrames = frames;
    }

    // (Linux, bug 1885) ALSA devices can take a moment to settle after a
    // rescan; allow a few retries before giving up.
    unsigned int maxTries = 1;
#ifdef __WXGTK__
    {
        using namespace std::chrono;
        if (DeviceManager::Instance()->GetTimeSinceRescan() < 10s) {
            maxTries = 5;
        }
    }
#endif

    rta::DuplexStream::Result openRes = rta::DuplexStream::Result::OpenFailed;
    for (unsigned int tries = 0; tries < maxTries; tries++) {
        openRes = mStream.open(cfg, [this](const float* in, float* out,
                                           unsigned int frames, double t,
                                           RtAudioStreamStatus s) {
            audacityAudioCallback(in, out, frames, t, s, nullptr);
        });
        if (openRes == rta::DuplexStream::Result::Ok) {
            break;
        }
        wxLogDebug("Attempt %u to open audio stream failed: %s",
                   1 + tries, mStream.lastErrorText().c_str());
        using namespace std::chrono;
        std::this_thread::sleep_for(1s);
    }

    if (openRes != rta::DuplexStream::Result::Ok) {
        return false;
    }

    const double outputLatency = mUsingJack
        // JACK: PA used to mis-report latency for non-system ports; trust the
        // user's preference as it's typically far closer than the driver's
        // estimate. RtAudio shares this caveat.
        ? (latencyDuration / 1000.0)
        : mStream.outputLatencySeconds();
    const double inputLatency  = mStream.inputLatencySeconds();

    if (AudioIOAutomaticLatencyCompensation.Read()) {
        mRecordingSchedule.mLatencyCompensation = -inputLatency - outputLatency;
    }

    mHardwarePlaybackLatencyMs    = outputLatency * 1000.0;
    mHardwareCaptureLatencyMs     = inputLatency  * 1000.0;
    mHardwarePlaybackLatencyFrames = lrint(outputLatency * mRate);
#ifdef __WXGTK__
    // ALSA's reported latency tends to be one period short; triple the frame
    // count empirically — same value the previous implementation used.
    if (mUsingAlsa) {
        mHardwarePlaybackLatencyFrames *= 3;
    }
#endif

#if (defined(__WXMAC__) || defined(__WXMSW__)) && wxCHECK_VERSION(3, 1, 0)
    if (mStream.isOpen()) {
        wxPowerResource::Acquire(wxPOWER_RESOURCE_SCREEN, _("Audacity Audio"));
    }
#endif

    return success = true;
}

wxString AudioIO::LastStreamErrorText()
{
    return wxString::FromUTF8(mStream.lastErrorText().c_str());
}

void AudioIO::SetOwningProject(
    const std::shared_ptr<AudacityProject>& pProject)
{
    if (!mOwningProject.expired()) {
        wxASSERT(false);
        ResetOwningProject();
    }

    mOwningProject = pProject;
}

void AudioIO::ResetOwningProject()
{
    mOwningProject.reset();
}

void AudioIO::ResetCaptureRouting()
{
    mCaptureChannelLayout.clear();
    mTrackChannelSourceMap.clear();
    mCaptureNeedsMixdown = false;
}

void AudioIO::ConfigureCaptureRouting(size_t inputChannelsCount)
{
    ResetCaptureRouting();

    if (mCaptureSequences.empty() || inputChannelsCount == 0) {
        return;
    }

    for (size_t trackIdx = 0; trackIdx < mCaptureSequences.size(); ++trackIdx) {
        const auto nChannels = mCaptureSequences[trackIdx]->NChannels();
        for (size_t channelIdx = 0; channelIdx < nChannels; ++channelIdx) {
            mCaptureChannelLayout.push_back({ trackIdx, channelIdx });
        }
    }

    if (mCaptureChannelLayout.empty()) {
        return;
    }

    const auto captureChannelsCount = mCaptureChannelLayout.size();

    mTrackChannelSourceMap.resize(captureChannelsCount);

    // Only allow up/down mixing for mono<->stereo cases
    const bool canMix = (inputChannelsCount <= 2 && captureChannelsCount <= 2);

    if (canMix && inputChannelsCount == 2 && captureChannelsCount == 1) {
        // Stereo input -> Mono track: mix both input channels to mono
        mCaptureNeedsMixdown = true;
        mTrackChannelSourceMap[0].push_back(0);
        mTrackChannelSourceMap[0].push_back(1);
    } else if (canMix && inputChannelsCount == 1 && captureChannelsCount == 2) {
        // Mono input -> Stereo track: duplicate mono to both channels
        mCaptureNeedsMixdown = false;
        mTrackChannelSourceMap[0].push_back(0);
        mTrackChannelSourceMap[1].push_back(0);
    } else {
        // 1 to 1 mapping
        mCaptureNeedsMixdown = false;
        for (size_t t = 0; t < std::min(captureChannelsCount, inputChannelsCount); ++t) {
            mTrackChannelSourceMap[t].push_back(t);
        }
    }
}

void AudioIO::StartMonitoring(const AudioIOStartStreamOptions& options)
{
    if (IsBusy()) {
        return;
    }

    if (IsMonitoring() && options.inputMonitoring != mSoftwarePlaythrough) {
        // muting or unmuting monitoring, so we need to restart it
        StopMonitoring();
    }

    if (IsMonitoring()) {
        // already monitoring, nothing to do
        return;
    }

    const auto captureFormat = QualitySettings::SampleFormatChoice();
    const auto captureChannels = AudioIORecordChannels.Read();
    mSoftwarePlaythrough = options.inputMonitoring;
    int playbackChannels = 0;

    if (mSoftwarePlaythrough) {
        playbackChannels = 2;
    }

    // FIXME: TRAP_ERR StartAudioStream (a PaError may be present)
    // but StartAudioStream function only returns true or false.
    mUsingAlsa = false;
    mCaptureFormat = captureFormat;
    mCaptureRate = 44100.0; // Shouldn't matter
    const bool success = StartAudioStream(options,
                                              static_cast<unsigned int>(playbackChannels),
                                              static_cast<unsigned int>(captureChannels));

    const auto pOwningProject = mOwningProject.lock();
    if (!success) {
        using namespace BasicUI;
        const auto msg = XO("Error opening recording device.\nError code: %s")
                         .Format(Get()->LastStreamErrorText());
        ShowErrorDialog(*ProjectFramePlacement(pOwningProject.get()),
                        XO("Error"), msg, wxT("Error_opening_sound_device"),
                        ErrorDialogOptions { ErrorDialogType::ModalErrorReport });
        return;
    }

    Publish({ pOwningProject.get(), AudioIOEvent::MONITOR, true });

    const auto sres = mStream.start();
    mLastStreamError = (sres == audacity::rta::DuplexStream::Result::Ok) ? 0 : 1;

    auto pListener = GetListener();
    if ((mLastStreamError == 0) && pListener) {
        pListener->OnAudioIORate((int)mRate);
    }
}

void AudioIO::StopMonitoring()
{
    if (IsMonitoring()) {
        StopStream();
        WaitWhileBusy();
    }
}

void AudioIO::WaitWhileBusy() const
{
    while (IsBusy()) {
        using namespace std::chrono;
        std::this_thread::sleep_for(100ms);
    }
}

int AudioIO::StartStream(const TransportSequences& sequences,
                         double t0, double t1, double mixerLimit,
                         const AudioIOStartStreamOptions& options)
{
    // precondition
    assert(std::all_of(
               sequences.playbackSequences.begin(), sequences.playbackSequences.end(),
               [](const auto& pSequence){
        const auto pGroup
            =pSequence ? pSequence->FindChannelGroup() : nullptr;
        return pGroup;
    }
               ));

    const auto& pStartTime = options.pStartTime;
    t1 = std::min(t1, mixerLimit);

    mLostSamples = 0;
    mLostCaptureIntervals.clear();
    mDetectDropouts
        =gPrefs->Read(WarningDialogKey(wxT("DropoutDetected")), true) != 0;
    auto cleanup = finally([this] { ClearRecordingException(); });

    if (IsBusy()) {
        return 0;
    }

    StopMonitoring();

    // We just want to set mStreamToken to -1 - this way avoids
    // an extremely rare but possible race condition, if two functions
    // somehow called StartStream at the same time...
    mStreamToken--;
    if (mStreamToken != -1) {
        return 0;
    }

    // TODO: we don't really need to close and reopen stream if the
    // format matches; however it's kind of tricky to keep it open...
    //
    //   if (sampleRate == mRate &&
    //       playbackChannels == mNumPlaybackChannels &&
    //       captureChannels == mNumCaptureChannels &&
    //       captureFormat == mCaptureFormat) {

    mSoftwarePlaythrough = options.inputMonitoring;
    mPauseRec = SoundActivatedRecord.Read();
    gPrefs->Read(wxT("/AudioIO/Microfades"), &mbMicroFades, false);
    int silenceLevelDB;
    gPrefs->Read(wxT("/AudioIO/SilenceLevel"), &silenceLevelDB, -50);
    int dBRange = DecibelScaleCutoff.Read();
    if (silenceLevelDB < -dBRange) {
        silenceLevelDB = -dBRange + 3;
        // meter range was made smaller than SilenceLevel
        // so set SilenceLevel reasonable

        // PRL:  update prefs, or correct it only in-session?
        // The behavior (as of 2.3.1) was the latter, the code suggested that
        // the intent was the former;  I preserve the behavior, but uncomment
        // this if you disagree.
        // gPrefs->Write(wxT("/AudioIO/SilenceLevel"), silenceLevelDB);
        // gPrefs->Flush();
    }
    mSilenceLevel = DB_TO_LINEAR(silenceLevelDB); // meter goes -dBRange dB -> 0dB

    // Clamp lead-in time so we don't play before time 0
    const auto leadInTime = std::max(0.0, std::min(t0, options.leadInTime));
    mRecordingSchedule = {};
    mRecordingSchedule.mLeadInTime = leadInTime;
    mRecordingSchedule.mLatencyCompensation = AudioIOLatencyCompensation.Read() / 1000.0;
    mRecordingSchedule.mDuration = t1 - t0;
    if (options.pCrossfadeData) {
        mRecordingSchedule.mCrossfadeData.swap(*options.pCrossfadeData);
    }

    mListener = options.listener;
    mRate    = options.rate;

    mSeek    = 0;
    mLastRecordingOffset = 0;
    mCaptureSequences = sequences.captureSequences;
    mPlaybackSequences = sequences.playbackSequences;
    mPlaybackTracks = {
        sequences.playbackSequences.begin(),
        sequences.playbackSequences.end()
    };
    ResetCaptureRouting();

    bool commit = false;
    auto cleanupSequences = finally([&]{
        if (!commit) {
            // Don't keep unnecessary shared pointers to sequences
            mPlaybackSequences.clear();
            mPlaybackTracks.clear();
            mCaptureSequences.clear();
            for (auto& ext : Extensions()) {
                ext.AbortOtherStream();
            }

            // Don't cause a busy wait in the audio thread after stopping scrubbing
            mPlaybackSchedule.ResetMode();
        }
    });

    mPlaybackBuffers.clear();
    mScratchBuffers.clear();
    mScratchPointers.clear();
    mPlaybackMixers.clear();
    mCaptureBuffers.clear();
    mResample.clear();
    ResetCaptureRouting();
    mPlaybackSchedule.mTimeQueue.Clear();

    mPlaybackSchedule.Init(
        t0, t1, options, mCaptureSequences.empty() ? nullptr : &mRecordingSchedule);

    unsigned int playbackChannels = 0;
    size_t numCaptureChannels = 0;
    sampleFormat captureFormat = floatSample;
    double captureRate = 44100.0;

    auto pListener = GetListener();

    if (sequences.playbackSequences.size() > 0
        || sequences.otherPlayableSequences.size() > 0) {
        playbackChannels = 2;
    }

    if (mSoftwarePlaythrough) {
        playbackChannels = 2;
    }

    if (mCaptureSequences.size() > 0) {
        size_t requestedInputChannels = AudioIORecordChannels.Read();
        if (requestedInputChannels == 0) {
            requestedInputChannels = 1;
        }
        ConfigureCaptureRouting(requestedInputChannels);
        numCaptureChannels = requestedInputChannels;
        // I don't deal with the possibility of the capture sequences
        // having different sample formats, since it will never happen
        // with the current code.  This code wouldn't *break* if this
        // assumption was false, but it would be sub-optimal.  For example,
        // if the first sequence was 16-bit and the second sequence was 24-bit,
        // we would set the sound card to capture in 16 bits and the second
        // sequence wouldn't get the benefit of all 24 bits the card is capable
        // of.
        const auto& sequence0 = mCaptureSequences[0];
        captureFormat = sequence0->GetSampleFormat();
        captureRate = sequence0->GetRate();

        // Tell project that we are about to start recording
        if (pListener) {
            pListener->OnAudioIOStartRecording();
        }
    }

    bool successAudio;

    mCaptureFormat = captureFormat;
    mCaptureRate = captureRate;
    successAudio
        =StartAudioStream(options, playbackChannels, numCaptureChannels);

    // Call this only after reassignment of mRate that might happen in the
    // previous call.
    mPlaybackSchedule.GetPolicy().Initialize(mPlaybackSchedule, mRate);

    AudioStreamInfo streamInfoForExt {};
    const AudioStreamInfo* pStreamInfoForExt = nullptr;
    if (mStream.isOpen()) {
        streamInfoForExt.structVersion = 1;
        streamInfoForExt.inputLatency  = mStream.inputLatencySeconds();
        streamInfoForExt.outputLatency = mStream.outputLatencySeconds();
        streamInfoForExt.sampleRate    = mRate;
        pStreamInfoForExt = &streamInfoForExt;
    }
    auto range = Extensions();
    successAudio = successAudio
                   && std::all_of(range.begin(), range.end(),
                                  [&, this, t0](auto& ext){
        return ext.StartOtherStream(sequences, pStreamInfoForExt, t0, mRate);
    });

    if (!successAudio) {
        if (pListener && numCaptureChannels > 0) {
            pListener->OnAudioIOStopRecording();
        }
        mStreamToken = 0;

        return 0;
    }

    {
        double mixerStart = t0;
        if (pStartTime) {
            mixerStart = std::min(mixerStart, *pStartTime);
        }
        if (!AllocateBuffers(options, sequences,
                             mixerStart, mixerLimit, options.rate)) {
            return 0;
        }
    }

    mpTransportState = std::make_unique<TransportState>(mOwningProject, mPlaybackSequences, mNumPlaybackChannels, mRate,
                                                        mPlaybackSamplesToCopy);

    if (pStartTime) {
        // Calculate the NEW time position
        const auto time = *pStartTime;

        // Main thread's initialization of mTime
        mPlaybackSchedule.SetSequenceTime(time);
        mPlaybackSchedule.GetPolicy().OffsetSequenceTime(mPlaybackSchedule, 0);

        // Reset mixer positions for all playback sequences
        for (auto& mixer : mPlaybackMixers) {
            mixer->Reposition(time);
        }
    }

    // Now that we are done with AllocateBuffers() and SetSequenceTime():
    mPlaybackSchedule.mTimeQueue.Prime(mPlaybackSchedule.GetSequenceTime());
    // else recording only without overdub

    // We signal the audio thread to call SequenceBufferExchange, to prime the RingBuffers
    // so that they will have data in them when the stream starts.  Having the
    // audio thread call SequenceBufferExchange here makes the code more predictable, since
    // SequenceBufferExchange will ALWAYS get called from the Audio thread.
    mAudioThreadShouldCallSequenceBufferExchangeOnce
    .store(true, std::memory_order_release);

    while (mAudioThreadShouldCallSequenceBufferExchangeOnce
           .load(std::memory_order_acquire)) {
        using namespace std::chrono;
        auto interval = 50ms;
        if (options.playbackStreamPrimer) {
            interval = options.playbackStreamPrimer();
        }
        std::this_thread::sleep_for(interval);
    }

    if (mNumPlaybackChannels > 0 || mNumCaptureChannels > 0) {
        // ALSA realtime scheduling is handled internally by the audio
        // backend; no explicit hook required here.

        //
        // Generate a unique value each time, to be returned to
        // clients accessing the AudioIO API, so they can query if they
        // are the ones who have reserved AudioIO or not.
        //
        // It is important to set this before setting the audio stream in
        // motion -- otherwise it may play an unspecified number of leading
        // zeroes.
        mStreamToken = (++mNextStreamToken);

        // This affects AudioThread (not the audio callback).
        // Probably not needed so urgently before audio thread start for usual
        // playback, since our ring buffers have been primed already with 4 sec
        // of audio, but then we might be scrubbing, so do it.
        StartAudioThread();

        mForceFadeOut.store(false, std::memory_order_relaxed);

        const auto startRes = mStream.start();
        if (startRes != audacity::rta::DuplexStream::Result::Ok) {
            mStreamToken = 0;
            StopAudioThread();
            if (pListener && mNumCaptureChannels > 0) {
                pListener->OnAudioIOStopRecording();
            }
            StartStreamCleanup();
            BasicUI::ShowMessageBox(
                Verbatim(wxString::FromUTF8(mStream.lastErrorText().c_str())));
            return 0;
        }
    }

    // Update UI display only now, after all possibilities for error are past.
    if (pListener) {
        // advertise the chosen I/O sample rate to the UI
        pListener->OnAudioIORate((int)mRate);
    }

    auto pOwningProject = mOwningProject.lock();
    if (mNumPlaybackChannels > 0) {
        Publish({ pOwningProject.get(), AudioIOEvent::PLAYBACK, true });
    }
    if (mNumCaptureChannels > 0) {
        Publish({ pOwningProject.get(), AudioIOEvent::CAPTURE, true });
    }

    commit = true;

    WaitForAudioThreadStarted();

    return mStreamToken;
}

void AudioIO::DelayActions(bool recording)
{
    mDelayingActions = recording;
}

bool AudioIO::DelayingActions() const
{
    return mDelayingActions || (mStream.isOpen() && mNumCaptureChannels > 0);
}

void AudioIO::CallAfterRecording(PostRecordingAction action)
{
    if (!action) {
        return;
    }

    {
        std::lock_guard<std::mutex> guard{ mPostRecordingActionMutex };
        if (mPostRecordingAction) {
            // Enqueue it, even if perhaps not still recording,
            // but it wasn't cleared yet
            mPostRecordingAction = [
                prevAction = std::move(mPostRecordingAction),
                nextAction = std::move(action)
                                   ]{ prevAction(); nextAction(); };
            return;
        } else if (DelayingActions()) {
            mPostRecordingAction = std::move(action);
            return;
        }
    }

    // Don't delay it except until idle time.
    // (Recording might start between now and then, but won't go far before
    // the action is done.  So the system isn't bulletproof yet.)
    BasicUI::CallAfter(move(action));
}

bool AudioIO::AllocateBuffers(
    const AudioIOStartStreamOptions& options,
    const TransportSequences& sequences, double t0, double t1, double sampleRate)
{
    bool success = false;
    auto cleanup = finally([&]{
        if (!success) {
            StartStreamCleanup(false);
        }
    });

    auto& policy = mPlaybackSchedule.GetPolicy();
    auto times = policy.SuggestedBufferTimes(mPlaybackSchedule);

    //
    // The (audio) stream has been opened successfully (assuming we tried
    // to open it). We now proceed to
    // allocate the memory structures the stream will need.
    //

    //
    // The RingBuffer sizes, and the max amount of the buffer to
    // fill at a time, both grow linearly with the number of
    // sequences.  This allows us to scale up to many sequences without
    // killing performance.
    //

    // real playback time to produce with each filling of the buffers
    // by the Audio thread (except at the end of playback):
    // usually, make fillings fewer and longer for less CPU usage.
    // What Audio thread produces for playback is then consumed by the audio
    // thread, in many smaller pieces.
    double playbackTime = lrint(times.batchSize.count() * mRate) / mRate;

    wxASSERT(playbackTime >= 0);
    mPlaybackSamplesToCopy = playbackTime * mRate;

    // Capacity of the playback buffer.
    mPlaybackRingBufferSecs = times.ringBufferDelay;

    mCaptureRingBufferSecs
        =4.5 + 0.5 * std::min(size_t(16), mNumCaptureChannels);
    mMinCaptureSecsToCopy
        =0.2;

    bool bDone;
    do{
        bDone = true; // assume success
        try
        {
            if (mNumPlaybackChannels > 0) {
                // Allocate output buffers.
                // Allow at least 2x of the buffer latency.
                auto playbackBufferSize
                    =std::max((size_t)lrint(mRate * mPlaybackRingBufferSecs.count()), mHardwarePlaybackLatencyFrames * 2);

                // Make playbackBufferSize a multiple of mPlaybackSamplesToCopy
                playbackBufferSize = mPlaybackSamplesToCopy
                                     * ((playbackBufferSize + mPlaybackSamplesToCopy - 1) / mPlaybackSamplesToCopy);

                // Adjust mPlaybackRingBufferSecs correspondingly
                mPlaybackRingBufferSecs = PlaybackPolicy::Duration { playbackBufferSize / mRate };

                mPlaybackBuffers.resize(0);
                mProcessingBuffers.resize(0);
                mMasterBuffers.resize(0);

                // Always make at least one playback buffer, in case of
                // MIDI playback without any audio
                if (mPlaybackSequences.empty()) {
                    mPlaybackBuffers.resize(1);
                } else {
                    mPlaybackBuffers.resize(mNumPlaybackChannels);
                    mProcessingBuffers.resize(std::accumulate(
                                                  mPlaybackSequences.begin(),
                                                  mPlaybackSequences.end(),
                                                  0, [](int n, auto& seq) { return n + seq->NChannels(); }
                                                  ));
                    for (auto& buffer : mProcessingBuffers) {
                        buffer.reserve(playbackBufferSize);
                    }

                    mMasterBuffers.resize(mNumPlaybackChannels);
                    for (auto& buffer : mMasterBuffers) {
                        buffer.reserve(playbackBufferSize);
                    }

                    // Number of scratch buffers depends on device playback channels
                    if (mNumPlaybackChannels > 0) {
                        mScratchBuffers.resize(mNumPlaybackChannels * 2 + 1);
                        mScratchPointers.clear();
                        for (auto& buffer : mScratchBuffers) {
                            buffer.Allocate(playbackBufferSize, floatSample);
                            mScratchPointers.push_back(
                                reinterpret_cast<float*>(buffer.ptr()));
                        }
                    }
                }

                std::generate(
                    mPlaybackBuffers.begin(),
                    mPlaybackBuffers.end(),
                    [=]{ return std::make_unique<RingBuffer>(floatSample, playbackBufferSize); }
                    );

                mPlaybackMixers.clear();

                const auto& warpOptions
                    =policy.MixerWarpOptions(mPlaybackSchedule);

                mPlaybackQueueMinimum = lrint(mRate * times.latency.count());
                mPlaybackQueueMinimum
                    =std::min(mPlaybackQueueMinimum, playbackBufferSize);

                // Limit the mPlaybackQueueMinimum to the hardware latency
                mPlaybackQueueMinimum
                    =std::max(mPlaybackQueueMinimum, mHardwarePlaybackLatencyFrames);

                // Make mPlaybackQueueMinimum a multiple of mPlaybackSamplesToCopy
                mPlaybackQueueMinimum = mPlaybackSamplesToCopy
                                        * ((mPlaybackQueueMinimum + mPlaybackSamplesToCopy - 1) / mPlaybackSamplesToCopy);

                // Bug 1763 - We must fade in from zero to avoid a click on starting.
                mOldPlaybackVolume = 0.0f;
                for (unsigned int i = 0; i < mPlaybackSequences.size(); i++) {
                    const auto& pSequence = mPlaybackSequences[i];

                    // By the precondition of StartStream which is sole caller of
                    // this function:
                    assert(pSequence->FindChannelGroup());
                    // use sequence time for the end time, not real time!
                    double startTime, endTime;
                    if (!sequences.leadInTimeSequences.empty()) {
                        startTime = mPlaybackSchedule.mT0;
                    } else {
                        startTime = t0;
                    }

                    if (make_iterator_range(sequences.leadInTimeSequences)
                        .contains(pSequence)) {
                        // Stop playing this sequence after lead-in time
                        endTime = t0;
                    } else {
                        // Pass t1 -- not mT1 as may have been adjusted for latency
                        // -- so that overdub recording stops playing back samples
                        // at the right time, though transport may continue to
                        // record
                        endTime = t1;
                    }

                    Mixer::Inputs mixSequences;
                    mixSequences.push_back(Mixer::Input { pSequence });
                    mPlaybackMixers.emplace_back(std::make_unique<Mixer>(
                                                     std::move(mixSequences), std::nullopt,
                                                     // Don't throw for read errors, just play silence:
                                                     false, warpOptions, startTime, endTime,
                                                     pSequence->NChannels(),
                                                     std::max(mPlaybackSamplesToCopy, mPlaybackQueueMinimum),
                                                     false, // not interleaved
                                                     mRate, floatSample,
                                                     false, // low quality dithering and resampling
                                                     nullptr, // no custom mix-down
                                                     Mixer::ApplyVolume::Discard // don't apply volume
                                                     ));
                }

                const auto timeQueueSize = 1
                                           + (playbackBufferSize + TimeQueueGrainSize - 1)
                                           / TimeQueueGrainSize;
                mPlaybackSchedule.mTimeQueue.Init(timeQueueSize);
            }

            const size_t playbackBufferSize = std::max((size_t)lrint(
                                                           mRate * mPlaybackRingBufferSecs.count()), mHardwarePlaybackLatencyFrames * 2);
            for (auto& track : mPlaybackTracks) {
                for (auto& buffer : track.mBuffers) {
                    buffer.reset();
                }

                for (size_t i = 0; i < mNumPlaybackChannels; ++i) {
                    track.mBuffers[i] = std::make_unique<RingBuffer>(
                        floatSample, playbackBufferSize);
                }
            }

            if (mNumCaptureChannels > 0) {
                // Allocate input buffers.  For every input sequence we allocate
                // a ring buffer of five seconds
                auto captureBufferSize
                    =(size_t)(mRate * mCaptureRingBufferSecs + 0.5);

                // In the extraordinarily rare case that we can't even afford
                // 100 samples, just give up.
                if (captureBufferSize < 100) {
                    BasicUI::ShowMessageBox(XO("Out of memory!"));
                    return false;
                }

                mCaptureBuffers.resize(0);
                mCaptureBuffers.resize(mNumCaptureChannels);
                mResample.resize(0);
                mResample.resize(mNumCaptureChannels);
                mFactor = sampleRate / mRate;

                for (unsigned int i = 0; i < mNumCaptureChannels; ++i) {
                    mCaptureBuffers[i] = std::make_unique<RingBuffer>(
                        mCaptureFormat, captureBufferSize);
                    mResample[i]
                        =std::make_unique<Resample>(true, mFactor, mFactor);
                    // constant rate resampling
                }
            }
        }
        catch (std::bad_alloc&)
        {
            // Oops!  Ran out of memory.  This is pretty rare, so we'll just
            // try deleting everything, halving our buffer size, and try again.
            StartStreamCleanup(true);
            mPlaybackRingBufferSecs *= 0.5;
            mPlaybackSamplesToCopy /= 2;
            mCaptureRingBufferSecs *= 0.5;
            mMinCaptureSecsToCopy *= 0.5;
            bDone = false;

            // In the extraordinarily rare case that we can't even afford 100
            // samples, just give up.
            auto playbackBufferSize
                =(size_t)lrint(mRate * mPlaybackRingBufferSecs.count());
            if (playbackBufferSize < 100 || mPlaybackSamplesToCopy < 100) {
                BasicUI::ShowMessageBox(XO("Out of memory!"));
                return false;
            }
        }
    } while (!bDone);

    success = true;
    return true;
}

void AudioIO::StartStreamCleanup(bool bOnlyBuffers)
{
    mpTransportState.reset();

    mPlaybackBuffers.clear();
    mPlaybackTracks.clear();
    mScratchBuffers.clear();
    mScratchPointers.clear();
    mPlaybackMixers.clear();
    mCaptureBuffers.clear();
    mResample.clear();
    mPlaybackSchedule.mTimeQueue.Clear();

    if (!bOnlyBuffers) {
        mStream.stop();
        mStream.close();
        mStreamToken = 0;
    }

    mPlaybackSchedule.GetPolicy().Finalize(mPlaybackSchedule);
}

bool AudioIO::IsAvailable(AudacityProject& project) const
{
    auto pOwningProject = mOwningProject.lock();
    return !pOwningProject || pOwningProject.get() == &project;
}

void AudioIO::StopStream()
{
    StopMeters();
    ResetMeters();

    auto cleanup = finally([this] {
        ClearRecordingException();
        mRecordingSchedule.mCrossfadeData.clear(); // free arrays
    });

    if (!mStream.isOpen()) {
        mStreamToken = 0;
        return;
    }

    // DV: This code seems to be unnecessary.
    // We do not leave mPortStreamV19 open in stopped
    // state. (Do we?)
    // This breaks WASAPI backend, as it sets the `running`
    // flag to `false` asynchronously.
    // (Historically a backend patch broke the
    // IsStreamStopped() == !IsStreamActive() invariant; no longer relevant.)

#if (defined(__WXMAC__) || defined(__WXMSW__)) && wxCHECK_VERSION(3, 1, 0)
    // Re-enable system sleep
    wxPowerResource::Release(wxPOWER_RESOURCE_SCREEN);
#endif

    if (mAudioThreadSequenceBufferExchangeLoopRunning
        .load(std::memory_order_relaxed)) {
        // audio callback can use the information that we are stopping to fade
        // out the audio.  Give audio callback a chance to do so.
        mForceFadeOut.store(true, std::memory_order_relaxed);
        auto latency = static_cast<long>(AudioIOLatencyDuration.Read());
        // If we can gracefully fade out in 200ms, with the faded-out play buffers making it through
        // the sound card, then do so.  If we can't, don't wait around.  Just stop quickly and accept
        // there will be a click.
        if (mbMicroFades && (latency < 150)) {
            using namespace std::chrono;
            std::this_thread::sleep_for(milliseconds { latency + 50 });
        }
    }

    wxMutexLocker locker(mSuspendAudioThread);

    //
    // We got here in one of two ways:
    //
    // 1. The user clicked the stop button and we therefore want to stop
    //    as quickly as possible.  So we use AbortStream().  If this is
    //    the case the audio stream is still in the Running state
    //    (see audio backend state machine).
    //
    // 2. The callback told the backend to stop the stream since it had
    //    reached the end of the selection.  The UI thread discovered
    //    this by noticing that AudioIO::IsActive() returned false.
    //    IsActive() (which calls Pa_GetStreamActive()) will not return
    //    false until all buffers have finished playing, so we can call
    //    AbortStream without losing any samples.  If this is the case
    //    we are in the "callback finished state" (see audio backend state
    //    machine docs).
    //
    // The moral of the story: We can call AbortStream safely, without
    // losing samples.
    //
    // DMM: This doesn't seem to be true; it seems to be necessary to
    // call StopStream if the callback brought us here, and AbortStream
    // if the user brought us here.
    //
    StopAudioThread();

    if (mStream.isOpen()) {
        mStream.stop();
        mStream.close();
    }

    // We previously told AudioThread to stop processing, now let's
    // be sure it has really stopped before resetting mpTransportState
    WaitForAudioThreadStopped();

    for ( auto& ext : Extensions()) {
        ext.StopOtherStream();
    }

    auto pListener = GetListener();

    // If there's no token, we were just monitoring, so we can
    // skip this next part...
    if (mStreamToken > 0) {
        // In either of the above cases, we want to make sure that any
        // capture data that made it into the audio callback makes it
        // to the target RecordableSequence.  To do this, we ask the audio thread
        // to call SequenceBufferExchange one last time (it normally would not do
        // so since Pa_GetStreamActive() would now return false
        ProcessOnceAndWait();
    }

    // No longer need effects processing. This must be done after the stream is stopped
    // to prevent the callback from being invoked after the effects are finalized.
    mpTransportState.reset();

    //
    // Everything is taken care of.  Now, just free all the resources
    // we allocated in StartStream()
    //
    mPlaybackBuffers.clear();
    mScratchBuffers.clear();
    mScratchPointers.clear();
    mPlaybackMixers.clear();
    mPlaybackSchedule.mTimeQueue.Clear();
    mPlaybackTracks.clear();

    if (mStreamToken > 0) {
        //
        // Offset all recorded sequences to account for latency
        //
        if (mCaptureSequences.size() > 0) {
            mCaptureBuffers.clear();
            mResample.clear();

            //
            // We only apply latency correction when we actually played back
            // sequences during the recording. If we did not play back sequences,
            // there's nothing we could be out of sync with. This also covers the
            // case that we do not apply latency correction when recording the
            // first sequence in a project.
            //

            for (auto& sequence : mCaptureSequences) {
                // The calls to Flush
                // may cause exceptions because of exhaustion of disk space.
                // Stop those exceptions here, or else they propagate through too
                // many parts of Audacity that are not effects or editing
                // operations.  GuardedCall ensures that the user sees a warning.

                // Also be sure to Flush each sequence, at the top of the
                // guarded call, relying on the guarantee that the sequence will be
                // left in a flushed state, though the append buffer may be lost.

                GuardedCall([&] {
                    // use No-fail-guarantee that sequence is flushed,
                    // Partial-guarantee that some initial length of the recording
                    // is saved.
                    // See comments in SequenceBufferExchange().
                    sequence->Flush();
                });
            }

            if (!mLostCaptureIntervals.empty()) {
                // This scope may combine many insertions of silence
                // into one transaction, lessening the number of checkpoints
                std::optional<TransactionScope> pScope;
                if (auto pOwningProject = mOwningProject.lock()) {
                    pScope.emplace(*pOwningProject, "Dropouts");
                }
                for (auto& interval : mLostCaptureIntervals) {
                    auto& start = interval.first;
                    auto duration = interval.second;
                    for (auto& sequence : mCaptureSequences) {
                        GuardedCall([&] {
                            sequence->InsertSilence(start, duration);
                        });
                    }
                }
                if (pScope) {
                    pScope->Commit();
                }
            }

            if (pListener) {
                pListener->OnCommitRecording();
            }
        }
    }

    if (pListener && mNumCaptureChannels > 0) {
        pListener->OnAudioIOStopRecording();
    }

    BasicUI::CallAfter([this]{
        if (mStream.isOpen() && mNumCaptureChannels > 0) {
            // Recording was restarted between StopStream and idle time
            // So the actions can keep waiting
            return;
        }
        // In case some other thread was waiting on the mutex too:
        std::this_thread::yield();
        std::lock_guard<std::mutex> guard { mPostRecordingActionMutex };
        if (mPostRecordingAction) {
            mPostRecordingAction();
            mPostRecordingAction = {};
        }
        DelayActions(false);
    });

    //
    // Only set token to 0 after we're totally finished with everything
    //
    bool wasMonitoring = mStreamToken == 0;
    mStreamToken = 0;

    {
        auto pOwningProject = mOwningProject.lock();
        if (mNumPlaybackChannels > 0) {
            Publish({ pOwningProject.get(), AudioIOEvent::PLAYBACK, false });
        }
        if (mNumCaptureChannels > 0) {
            Publish({ pOwningProject.get(),
                      wasMonitoring
                      ? AudioIOEvent::MONITOR
                      : AudioIOEvent::CAPTURE,
                      false });
        }
    }

    ResetOwningProject();

    mNumCaptureChannels = 0;
    mNumPlaybackChannels = 0;

    mPlaybackSequences.clear();
    mCaptureSequences.clear();
    ResetCaptureRouting();

    mPlaybackSchedule.GetPolicy().Finalize(mPlaybackSchedule);

    if (pListener) {
        // Tell UI to hide sample rate
        pListener->OnAudioIORate(0);
    }

    // Don't cause a busy wait in the audio thread after stopping scrubbing
    mPlaybackSchedule.ResetMode();
}

void AudioIO::SeekStream(double seconds)
{
    mSeek = seconds;
}

void AudioIO::SetPaused(bool state, bool publish)
{
    if (state != IsPaused()) {
        if (auto pOwningProject = mOwningProject.lock()) {
            // The realtime effects manager may remain "active" but becomes
            // "suspended" or "resumed".
            auto& em = RealtimeEffectManager::Get(*pOwningProject);
            em.SetSuspended(state);
        }
    }

    mPaused.store(state, std::memory_order_relaxed);

    if (publish) {
        Publish({ mOwningProject.lock().get(), AudioIOEvent::PAUSE, state });
    }
}

double AudioIO::GetBestRate(bool capturing, bool playing, double sampleRate)
{
    // Check if we can use the cached value
    if (mCachedBestRateIn != 0.0 && mCachedBestRateIn == sampleRate
        && mCachedBestRatePlaying == playing && mCachedBestRateCapturing == capturing) {
        return mCachedBestRateOut;
    }

    if (capturing) {
        wxLogDebug(wxT("AudioIO::GetBestRate() for capture"));
    }
    if (playing) {
        wxLogDebug(wxT("AudioIO::GetBestRate() for playback"));
    }
    wxLogDebug(wxT("GetBestRate() suggested rate %.0lf Hz"), sampleRate);

    long requestedRate = static_cast<long>(sampleRate);
    long supportedRate = 0;

    if (capturing && !playing) {
        supportedRate = GetClosestSupportedCaptureRate(-1, sampleRate);
    } else if (playing && !capturing) {
        supportedRate = GetClosestSupportedPlaybackRate(-1, sampleRate);
    } else { // we assume capturing and playing - the alternative would be a
             // bit odd
        supportedRate = GetClosestSupportedSampleRate(-1, -1, sampleRate);
    }

    /* if we get here, there is a problem - the project rate isn't supported
     * on our hardware, so we can't use it. */

    if (supportedRate == 0) {
        /* we're stuck - there are no supported rates with this hardware. Error */
        wxLogDebug(wxT("GetBestRate() Error - no supported sample rates"));
    } else if (supportedRate != requestedRate) {
        wxLogDebug(wxT("GetBestRate() Returning highest supported rate - %.0ld Hz"), supportedRate);
    }

    mCachedBestRateIn = sampleRate;
    mCachedBestRateOut = supportedRate;
    mCachedBestRatePlaying = playing;
    mCachedBestRateCapturing = capturing;
    return supportedRate;
}

double AudioIO::GetStreamTime()
{
    // Sequence time readout for the main thread
    return mPlaybackSchedule.GetSequenceTime();
}

//////////////////////////////////////////////////////////////////////
//
//     Audio Thread Context
//
//////////////////////////////////////////////////////////////////////

//! Sits in a thread loop reading and writing audio.
void AudioIO::AudioThread(std::atomic<bool>& finish)
{
    enum class ProcessingState {
        eSkipProcessing, ePrimeProcessing, eMonitoringProcessing, eCallbackProcessing
    } lastState = ProcessingState::eSkipProcessing;
    AudioIO* const gAudioIO = AudioIO::Get();
    while (!finish.load(std::memory_order_acquire)) {
        using Clock = std::chrono::steady_clock;
        auto loopPassStart = Clock::now();
        auto& schedule = gAudioIO->mPlaybackSchedule;
        const auto interval = schedule.GetPolicy().SleepInterval(schedule);

        // Set LoopActive outside the tests to avoid race condition
        gAudioIO->mAudioThreadSequenceBufferExchangeLoopActive
        .store(true, std::memory_order_relaxed);
        if (gAudioIO->mAudioThreadShouldCallSequenceBufferExchangeOnce
            .load(std::memory_order_acquire)) {
            gAudioIO->SequenceBufferExchange();
            gAudioIO->mAudioThreadShouldCallSequenceBufferExchangeOnce
            .store(false, std::memory_order_release);

            lastState = ProcessingState::ePrimeProcessing;
        } else if (gAudioIO->mAudioThreadSequenceBufferExchangeLoopRunning
                   .load(std::memory_order_relaxed)) {
            if (lastState != ProcessingState::eCallbackProcessing) {
                // Main thread has told us to start - acknowledge that we do
                gAudioIO->mAudioThreadAcknowledge.store(Acknowledge::eStart,
                                                        std::memory_order::memory_order_release);
            }
            lastState = ProcessingState::eCallbackProcessing;

            // We call the processing after raising the acknowledge flag, because the main thread
            // only needs to know that the message was seen.
            //
            // This is unlike the case with mAudioThreadShouldCallSequenceBufferExchangeOnce where the
            // store really means that the one-time exchange was done.

            gAudioIO->SequenceBufferExchange();
        } else {
            if ((lastState == ProcessingState::eCallbackProcessing)
                || (lastState == ProcessingState::eMonitoringProcessing)
                || (lastState == ProcessingState::ePrimeProcessing)) {
                // Main thread has told us to stop; (actually: to neither process "once" nor "loop running")
                // acknowledge that we received the order and that no more processing will be done.
                gAudioIO->mAudioThreadAcknowledge.store(Acknowledge::eStop,
                                                        std::memory_order::memory_order_release);
            }
            lastState = ProcessingState::eSkipProcessing;

            if (gAudioIO->IsMonitoring()) {
                lastState = ProcessingState::eMonitoringProcessing;
            }
        }

        gAudioIO->mAudioThreadSequenceBufferExchangeLoopActive
        .store(false, std::memory_order_relaxed);

        std::this_thread::sleep_until(loopPassStart + interval);
    }
}

size_t AudioIoCallback::MinValue(
    const RingBuffers& buffers, size_t (RingBuffer::*pmf)() const)
{
    return std::accumulate(buffers.begin(), buffers.end(),
                           std::numeric_limits<size_t>::max(),
                           [pmf](auto value, auto& pBuffer){
        return std::min(value, (pBuffer.get()->*pmf)());
    });
}

size_t AudioIO::GetCommonlyFreePlayback()
{
    auto commonlyAvail = MinValue(mPlaybackBuffers, &RingBuffer::AvailForPut);
    // MB: subtract a few samples because the code in SequenceBufferExchange has rounding
    // errors
    return commonlyAvail - std::min(size_t(10), commonlyAvail);
}

size_t AudioIoCallback::GetCommonlyReadyPlayback()
{
    return MinValue(mPlaybackBuffers, &RingBuffer::AvailForGet);
}

size_t AudioIoCallback::GetCommonlyWrittenForPlayback()
{
    return MinValue(mPlaybackBuffers, &RingBuffer::WrittenForGet);
}

size_t AudioIO::GetCommonlyAvailCapture()
{
    return MinValue(mCaptureBuffers, &RingBuffer::AvailForGet);
}

// This method is the data gateway between the audio thread (which
// communicates with the disk) and the audio callback thread
// (which communicates with the audio device).
void AudioIO::SequenceBufferExchange()
{
    FillPlayBuffers();
    DrainRecordBuffers();
}

void AudioIO::FillPlayBuffers()
{
    std::optional<RealtimeEffects::ProcessingScope> pScope;
    if (mpTransportState && mpTransportState->mpRealtimeInitialization) {
        pScope.emplace(
            *mpTransportState->mpRealtimeInitialization, mOwningProject);
    }

    if (mNumPlaybackChannels == 0) {
        return;
    }

    // It is possible that some buffers will have more samples available than
    // others. This could happen if we hit this code during the audio backend's
    // callback. Also, if in a previous pass, unequal numbers of samples were
    // discarded from ring buffers for differing latencies.

    // To keep things simple, we write no more data than is vacant in
    // ALL buffers, and advance the global time by that much.
    auto nAvailable = GetCommonlyFreePlayback();

    // Don't fill the buffers at all unless we can do
    // at least mPlaybackSamplesToCopy. This improves performance
    // by not always trying to process tiny chunks, eating the
    // CPU unnecessarily.
    if (nAvailable < mPlaybackSamplesToCopy) {
        return;
    }

    // More than mPlaybackSamplesToCopy might be copied:
    // May produce a larger amount when initially priming the buffer, or
    // perhaps again later in play to avoid underfilling the queue and
    // falling behind the real-time demand on the consumer side in the
    // callback.
    auto GetNeeded = [&]() -> size_t {
        // Note that reader might concurrently consume between loop passes below
        // So this might not be nondecreasing
        auto nReady = GetCommonlyWrittenForPlayback();
        return mPlaybackQueueMinimum - std::min(mPlaybackQueueMinimum, nReady);
    };
    auto nNeeded = GetNeeded();

    // wxASSERT( nNeeded <= nAvailable );

    auto Flush = [&]{
        /* The flushing of all the Puts to the RingBuffers is lifted out of the
        do-loop in ProcessPlaybackSlices, and also after transformation of the
        stream for realtime effects.

        It's only here that a release is done on the atomic variable that
        indicates the readiness of sample data to the consumer.  That atomic
        also synchronizes the use of the TimeQueue.
        */
        for (const auto& pBuffer : mPlaybackBuffers) {
            pBuffer->Flush();
        }

        for (const auto& track : mPlaybackTracks) {
            for (auto& buffer : track.mBuffers) {
                buffer->Flush();
            }
        }
    };

    while (true) {
        // Limit maximum buffer size (increases performance)
        auto available = std::min(nAvailable,
                                  std::max(nNeeded, mPlaybackSamplesToCopy));

        // After each loop pass or after break
        Finally Do{ Flush };

        if (!ProcessPlaybackSlices(pScope, available)) {
            // We are not making progress.  May fail to satisfy the minimum but
            // won't loop forever
            break;
        }

        // Loop again to satisfy the minimum queue requirement in case there
        // was discarding of processed data for effect latencies
        nNeeded = GetNeeded();
        if (nNeeded == 0) {
            break;
        }

        // Might increase because the reader consumed some
        nAvailable = GetCommonlyFreePlayback();
    }
}

#define stackAllocate(T, count) static_cast<T*>(alloca(count * sizeof(T)))

bool AudioIO::ProcessPlaybackSlices(
    std::optional<RealtimeEffects::ProcessingScope>& pScope, size_t available)
{
    auto& policy = mPlaybackSchedule.GetPolicy();

    // msmeyer: When playing a very short selection in looped
    // mode, the selection must be copied to the buffer multiple
    // times, to ensure, that the buffer has a reasonable size
    // This is the purpose of this loop.
    // PRL: or, when scrubbing, we may get work repeatedly from the
    // user interface.
    bool done = false;
    bool progress = false;

    // remember initial processing buffer offsets
    // they may be different depending on latencies
    const auto processingBufferOffsets = stackAllocate(size_t, mProcessingBuffers.size());
    for (unsigned n = 0; n < mProcessingBuffers.size(); ++n) {
        processingBufferOffsets[n] = mProcessingBuffers[n].size();
    }

    do {
        const auto slice
            =policy.GetPlaybackSlice(mPlaybackSchedule, available);
        const auto&[frames, toProduce] = slice;
        progress = progress || toProduce > 0;

        // Update the time queue.  This must be done before writing to the
        // ring buffers of samples, for proper synchronization with the
        // consumer side in the audio thread, which reads the time
        // queue after reading the sample queues.  The sample queues use
        // atomic variables, the time queue doesn't.
        mPlaybackSchedule.mTimeQueue.Producer(mPlaybackSchedule, slice);

        // mPlaybackMixers correspond one-to-one with mPlaybackSequences
        size_t iSequence = 0;
        // mPlaybackBuffers correspond many-to-one with mPlaybackSequences
        size_t iBuffer = 0;
        for (auto& mixer : mPlaybackMixers) {
            // The mixer here isn't actually mixing: it's just doing
            // resampling, format conversion, and possibly time track
            // warping
            if (frames > 0) {
                size_t produced = 0;

                if (toProduce) {
                    produced = mixer->Process(toProduce);
                }

                //wxASSERT(produced <= toProduce);
                // Copy (non-interleaved) mixer outputs to one or more ring buffers
                const auto nChannels = mPlaybackSequences[iSequence]->NChannels();

                const auto appendPos = mProcessingBuffers[iBuffer].size();
                for (size_t j = 0; j < nChannels; ++j) {
                    // mPlaybackBuffers correspond many-to-one with mPlaybackSequences
                    auto& buffer = mProcessingBuffers[iBuffer + j];
                    //Sufficient size should have been reserved in AllocateBuffers
                    //But for some latency values (> aprox. 100ms) pre-allocated
                    //buffer could be not large enough.
                    //Preserve what was written to the buffer during previous pass, don't discard
                    buffer.resize(buffer.size() + frames, 0);

                    const auto warpedSamples = mixer->GetBuffer(j);
                    std::copy_n(
                        reinterpret_cast<const float*>(warpedSamples),
                        produced,
                        buffer.data() + appendPos);
                    std::fill_n(
                        buffer.data() + appendPos + produced,
                        frames - produced,
                        .0f);
                }

                iBuffer += nChannels;
                ++iSequence;
            }
        }

        available -= frames;
        // wxASSERT(available >= 0); // don't assert on this thread
        if (mPlaybackSequences.empty()) {
            // Produce silence in the single ring buffer
            mPlaybackBuffers[0]->Put(nullptr, floatSample, 0, frames);
        }

        done = policy.RepositionPlayback(mPlaybackSchedule, mPlaybackMixers,
                                         frames, available);
    } while (available && !done);

    //stop here if there are no sample sources to process...
    if (mPlaybackSequences.empty()) {
        return progress;
    }

    // Do any realtime effect processing for each individual sample source,
    // after all the little slices have been written.
    if (pScope) {
        const auto pointers = stackAllocate(float*, mNumPlaybackChannels);

        int bufferIndex = 0;
        for (const auto& seq : mPlaybackSequences) {
            if (!seq) {
                continue;//no similar check in convert-to-float part
            }
            const auto channelGroup = seq->FindChannelGroup();
            if (!channelGroup) {
                continue;
            }

            // Are there more output device channels than channels of vt?
            // Such as when a mono sequence is processed for stereo play?
            // Then supply some non-null fake input buffers, because the
            // various ProcessBlock overrides of effects may crash without it.
            // But it would be good to find the fixes to make this unnecessary.
            auto scratch = &mScratchPointers[mNumPlaybackChannels + 1];

            //skip samples that are already processed
            const auto offset = processingBufferOffsets[bufferIndex];
            //number of newly written samples
            const auto len = mProcessingBuffers[bufferIndex].size() - offset;

            if (len > 0) {
                for (unsigned i = 0, cnt = std::min(seq->NChannels(), mNumPlaybackChannels); i < cnt; ++i) {
                    pointers[i] = mProcessingBuffers[bufferIndex + i].data() + offset;
                }

                for (unsigned i = seq->NChannels(); i < mNumPlaybackChannels; ++i) {
                    pointers[i] = *scratch++;
                    std::fill_n(pointers[i], len, .0f);
                }

                const auto discardable = pScope->Process(channelGroup, &pointers[0],
                                                         mScratchPointers.data(),
                                                         // The single dummy output buffer:
                                                         mScratchPointers[mNumPlaybackChannels],
                                                         mNumPlaybackChannels, len);
                // Check for asynchronous user changes in mute, solo status
                const auto silenced = SequenceShouldBeSilent(*seq);
                for (int i = 0; i < seq->NChannels(); ++i) {
                    auto& buffer = mProcessingBuffers[bufferIndex + i];
                    buffer.erase(buffer.begin() + offset, buffer.begin() + offset + discardable);
                    if (silenced) {
                        //TODO: fade out smoothly
                        std::fill_n(buffer.data() + offset, len - discardable, 0);
                    }
                }
            }
            bufferIndex += seq->NChannels();
        }
    }

    //samples at the beginning could have been discarded
    //in the previous step, proceed with the number of samples
    //equal to the shortest buffer size available
    auto samplesAvailable = std::min_element(
        mProcessingBuffers.begin(),
        mProcessingBuffers.end(),
        [](auto& first, auto& second) { return first.size() < second.size(); }
        )->size();

    //introduced latency may be too high...
    //but we don't discard what already was written
    if (samplesAvailable == 0) {
        return progress;
    }

    //Prepare master buffers.
    auto cleanup = finally([=] {
        for (auto& buffer : mMasterBuffers) {
            buffer.clear();
        }
    });

    for (auto& buffer : mMasterBuffers) {
        //assert(buffer.size() == 0);
        //assert(buffer.capacity() >= samplesAvailable);
        buffer.resize(samplesAvailable, 0);
    }

    {
        unsigned bufferIndex = 0;
        for (auto& track :  mPlaybackTracks) {
            auto seq = track.mSequence;
            if (!seq) {
                continue;
            }

            auto& buffers = track.mBuffers;
            const auto numChannels = seq->NChannels();
            if (numChannels > 1) {
                for (unsigned n = 0, cnt = std::min(numChannels, mNumPlaybackChannels); n < cnt; ++n) {
                    const float volume = seq->GetChannelVolume(n);
                    for (unsigned i = 0; i < samplesAvailable; ++i) {
                        mProcessingBuffers[bufferIndex + n][i] *= volume;
                        mMasterBuffers[n][i] += mProcessingBuffers[bufferIndex + n][i];
                    }

                    //Copy per track data to ring buffers
                    buffers[n]->Put(
                        reinterpret_cast<constSamplePtr>(mProcessingBuffers[bufferIndex + n].data()),
                        floatSample,
                        samplesAvailable, 0);
                }
            } else if (numChannels == 1) {
                float maxVolume = 0.0f;

                for (unsigned n = 0; n < mNumPlaybackChannels; ++n) {
                    maxVolume = std::max(maxVolume, seq->GetChannelVolume(n));
                }

                // Mix mono source is duplicated into every output channel
                // accounting for panning
                for (unsigned n = 0; n < mNumPlaybackChannels; ++n) {
                    const float volume = seq->GetChannelVolume(n);
                    for (unsigned i = 0; i < samplesAvailable; ++i) {
                        mMasterBuffers[n][i] += mProcessingBuffers[bufferIndex][i] * volume;
                    }
                }

                for (unsigned i = 0; i < samplesAvailable; ++i) {
                    mProcessingBuffers[bufferIndex][i] *= maxVolume;
                }

                for (unsigned n = 0; n < mNumPlaybackChannels; ++n) {
                    //Copy per track data to ring buffers
                    buffers[n]->Put(
                        reinterpret_cast<constSamplePtr>(mProcessingBuffers[bufferIndex].data()),
                        floatSample,
                        samplesAvailable, 0);
                }
            }
            bufferIndex += seq->NChannels();
        }
    }

    //remove only samples that were processed in previous step
    for (auto& buffer : mProcessingBuffers) {
        buffer.erase(buffer.begin(), buffer.begin() + samplesAvailable);
    }

    // Do any realtime effect processing, after all the little
    // slices have been written. This time we use mixed source created in
    // previous step
    size_t masterBufferOffset = 0;//The amount of samples to be discarded
    if (pScope) {
        const auto pointers = stackAllocate(float*, mNumPlaybackChannels);
        for (unsigned i = 0; i < mNumPlaybackChannels; ++i) {
            pointers[i] = mMasterBuffers[i].data();
        }

        masterBufferOffset = pScope->Process(
            RealtimeEffectManager::MasterGroup,
            &pointers[0],
            mScratchPointers.data(),
            // The single dummy output buffer:
            mScratchPointers[mNumPlaybackChannels],
            mNumPlaybackChannels, samplesAvailable);

        // wxASSERT(samplesAvailable >= masterBufferOffset); // don't assert on this thread
        samplesAvailable -= masterBufferOffset;
    }

    if (samplesAvailable == 0) {
        return progress;
    }

    {
        unsigned bufferIndex = 0;
        for (auto& buffer : mMasterBuffers) {
            mPlaybackBuffers[bufferIndex++]->Put(
                reinterpret_cast<constSamplePtr>(buffer.data()) + masterBufferOffset * sizeof(float),
                floatSample,
                samplesAvailable,
                0
                );
        }

        // Discard equal amounts of samples on per track buffers
        if (masterBufferOffset > 0) {
            for (auto& track: mPlaybackTracks) {
                for (size_t channel = 0; channel < mNumPlaybackChannels; channel++) {
                    auto& ringBuffer = track.mBuffers[channel];
                    auto discarded = ringBuffer->Unput(masterBufferOffset);
                    assert(discarded == masterBufferOffset);
                }
            }
        }
    }

    return progress;
}

void AudioIO::DrainRecordBuffers()
{
    if (mRecordingException || mCaptureSequences.empty()) {
        return;
    }

    if (mTrackChannelSourceMap.empty() && mNumCaptureChannels > 0) {
        ConfigureCaptureRouting(mNumCaptureChannels);
    }

    auto delayedHandler = [this] ( AudacityException* pException ) {
        // In the main thread, stop recording
        // This is one place where the application handles disk
        // exhaustion exceptions from RecordableSequence operations, without
        // rolling back to the last pushed undo state.  Instead, partial recording
        // results are pushed as a NEW undo state.  For this reason, as
        // commented elsewhere, we want an exception safety guarantee for
        // the output RecordableSequences, after the failed append operation, that
        // the sequences remain as they were after the previous successful
        // (block-level) appends.

        // Note that the Flush in StopStream() may throw another exception,
        // but StopStream() contains that exception, and the logic in
        // AudacityException::DelayedHandlerAction prevents redundant message
        // boxes.
        StopStream();
        WaitWhileBusy();

        DefaultDelayedHandlerAction(pException);
        for (auto& pSequence: mCaptureSequences) {
            pSequence->RepairChannels();
        }
    };

    GuardedCall([&] {
        // start record buffering
        const auto avail = GetCommonlyAvailCapture(); // samples
        const auto remainingTime = std::max(0.0, mRecordingSchedule.ToConsume());
        // This may be a very big double number:
        const auto remainingSamples = remainingTime * mRate;
        bool latencyCorrected = true;

        const double deltat = avail / mRate;

        if (mAudioThreadShouldCallSequenceBufferExchangeOnce
            .load(std::memory_order_relaxed)
            || deltat >= mMinCaptureSecsToCopy) {
            // Append captured samples to the end of the RecordableSequences.
            // (WaveTracks have their own buffering for efficiency.)
            const size_t hardwareChannels = std::min(mNumCaptureChannels, mCaptureBuffers.size());
            if (hardwareChannels == 0 || mCaptureChannelLayout.empty()) {
                return;
            }

            struct CapturedChannelData {
                SampleBuffer buffer;
                size_t size { 0 };
                sampleFormat format { floatSample };
            };

            std::vector<CapturedChannelData> captured(hardwareChannels);
            const bool forceFloatCapture = mCaptureNeedsMixdown
                                           || !mRecordingSchedule.mCrossfadeData.empty();

            for (size_t i = 0; i < hardwareChannels; ++i) {
                size_t discarded = 0;

                if (!mRecordingSchedule.mLatencyCorrected) {
                    const auto correction = mRecordingSchedule.TotalCorrection();
                    // Leftward shift
                    // discard some samples from the ring buffers.
                    if (correction < 0) {
                        size_t size = floor(mRecordingSchedule.ToDiscard() * mRate);
                        // The ring buffer might have grown concurrently -- don't discard more
                        // than the "avail" value noted above.
                        discarded = mCaptureBuffers[i]->Discard(std::min(avail, size));

                        if (discarded < size) {
                            // We need to visit this again to complete the
                            // discarding.
                            latencyCorrected = false;
                        }
                    }
                }

                if (avail <= discarded) {
                    captured[i].size = 0;
                    continue;
                }

                size_t toGet = avail - discarded;
                SampleBuffer temp;
                size_t size = 0;
                sampleFormat format = mCaptureFormat;

                if (mFactor == 1.0) {
                    // Take captured samples directly
                    size = toGet;
                    // Change to float for crossfade/mix calculation
                    format = forceFloatCapture ? floatSample : mCaptureFormat;
                    temp.Allocate(size, format);
                    const auto got = mCaptureBuffers[i]->Get(temp.ptr(), format, toGet);
                    wxUnusedVar(got);
                    if (double(size) > remainingSamples) {
                        size = floor(remainingSamples);
                    }
                } else {
                    size = lrint(toGet * mFactor);
                    format = floatSample;
                    SampleBuffer temp1(toGet, floatSample);
                    temp.Allocate(size, format);
                    const auto got = mCaptureBuffers[i]->Get(temp1.ptr(), floatSample, toGet);
                    wxUnusedVar(got);
                    /* we are re-sampling on the fly. The last resampling call
                     * must flush any samples left in the rate conversion buffer
                     * so that they get recorded
                     */
                    if (toGet > 0) {
                        if (double(toGet) > remainingSamples) {
                            toGet = floor(remainingSamples);
                        }
                        const auto results = mResample[i]->Process(mFactor,
                                                                   (float*)temp1.ptr(), toGet,
                                                                   !IsStreamActive(),
                                                                   (float*)temp.ptr(), size);
                        size = results.second;
                    }
                }

                captured[i].buffer = std::move(temp);
                captured[i].size = size;
                captured[i].format = format;
            }

            const auto ensureFloatView = [&](size_t srcIndex, size_t sampleCount) -> const float* {
                auto& src = captured[srcIndex];
                if (src.format != floatSample) {
                    SampleBuffer converted(sampleCount, floatSample);
                    SamplesToFloats(src.buffer.ptr(), src.format,
                                    reinterpret_cast<float*>(converted.ptr()),
                                    sampleCount, 1, 1);
                    src.buffer = std::move(converted);
                    src.format = floatSample;
                    src.size = sampleCount;
                }
                return reinterpret_cast<const float*>(src.buffer.ptr());
            };

            bool newBlocks = false;
            size_t trackChannelIndex = 0;
            const auto correction = mRecordingSchedule.TotalCorrection();

            for (const auto& info : mCaptureChannelLayout) {
                if (trackChannelIndex >= mTrackChannelSourceMap.size()) {
                    break;
                }

                const auto& sources = mTrackChannelSourceMap[trackChannelIndex];
                if (sources.empty()) {
                    ++trackChannelIndex;
                    continue;
                }

                const auto firstSource = sources.front();
                if (firstSource >= captured.size()) {
                    ++trackChannelIndex;
                    continue;
                }

                size_t size = captured[firstSource].size;
                sampleFormat format = captured[firstSource].format;
                constSamplePtr dataPtr = captured[firstSource].buffer.ptr();
                SampleBuffer mixBuffer;
                SampleBuffer floatConversionBuffer;

                if (sources.size() > 1) {
                    size = captured[firstSource].size;
                    for (auto srcIndex : sources) {
                        if (srcIndex >= captured.size()) {
                            size = 0;
                            break;
                        }
                        size = std::min(size, captured[srcIndex].size);
                    }

                    if (size > 0) {
                        mixBuffer.Allocate(size, floatSample);
                        auto dest = reinterpret_cast<float*>(mixBuffer.ptr());
                        std::fill(dest, dest + size, 0.0f);

                        for (auto srcIndex : sources) {
                            const auto* srcPtr = ensureFloatView(srcIndex, size);
                            for (size_t s = 0; s < size; ++s) {
                                dest[s] += srcPtr[s];
                            }
                        }

                        const float gain = 1.0f / static_cast<float>(sources.size());
                        for (size_t s = 0; s < size; ++s) {
                            dest[s] *= gain;
                        }

                        dataPtr = mixBuffer.ptr();
                        format = floatSample;
                    }
                }

                if (!dataPtr || size == 0) {
                    ++trackChannelIndex;
                    continue;
                }

                if (double(size) > remainingSamples) {
                    size = floor(remainingSamples);
                }

                auto& sequence = mCaptureSequences[info.sequenceIndex];
                if (!sequence) {
                    ++trackChannelIndex;
                    continue;
                }

                if (!mRecordingSchedule.mLatencyCorrected && correction >= 0) {
                    // Rightward shift
                    // Once only (per sequence per recording), insert some initial
                    // silence.
                    const auto silenceSamples = static_cast<size_t>(floor(correction * mRate * mFactor));
                    if (silenceSamples > 0) {
                        SampleBuffer silence(silenceSamples, mCaptureFormat);
                        ClearSamples(silence.ptr(), mCaptureFormat, 0, silenceSamples);
                        newBlocks = sequence->Append(info.channelIndex, silence.ptr(), mCaptureFormat,
                                                     silenceSamples, 1, narrowestSampleFormat) || newBlocks;
                    }
                }

                const bool needsCrossfade
                    = trackChannelIndex < mRecordingSchedule.mCrossfadeData.size()
                      && !mRecordingSchedule.mCrossfadeData[trackChannelIndex].empty();

                if (needsCrossfade && format != floatSample) {
                    floatConversionBuffer.Allocate(size, floatSample);
                    SamplesToFloats(dataPtr, format,
                                    reinterpret_cast<float*>(floatConversionBuffer.ptr()),
                                    size, 1, 1);
                    dataPtr = floatConversionBuffer.ptr();
                    format = floatSample;
                }

                if (needsCrossfade) {
                    auto* dst = reinterpret_cast<float*>(const_cast<samplePtr>(dataPtr));
                    const auto& data = mRecordingSchedule.mCrossfadeData[trackChannelIndex];
                    auto crossfadeStart = static_cast<size_t>(floor(mRecordingSchedule.Consumed() * mCaptureRate));
                    if (crossfadeStart < data.size()) {
                        const auto crossfadeLength = std::min<size_t>(size, data.size() - crossfadeStart);
                        auto ratio = double(crossfadeStart) / data.size();
                        const auto ratioStep = 1.0 / data.size();
                        const float* src = data.data() + crossfadeStart;
                        for (size_t s = 0; s < crossfadeLength; ++s) {
                            dst[s] = ratio * dst[s] + (1.0 - ratio) * src[s];
                            ratio += ratioStep;
                        }
                    }
                }

                // Now append
                // see comment in second handler about guarantee
                newBlocks = sequence->Append(info.channelIndex, dataPtr, format,
                                             size, 1, narrowestSampleFormat) || newBlocks;

                ++trackChannelIndex;
            } // end loop over capture channels

            // Now update the recording schedule position
            mRecordingSchedule.mPosition += avail / mRate;
            mRecordingSchedule.mLatencyCorrected = latencyCorrected;

            auto pListener = GetListener();
            if (pListener && newBlocks) {
                pListener->OnAudioIONewBlocks();
            }
        }
        // end of record buffering
    },
                // handler
                [this] ( AudacityException* pException ) {
        if (pException) {
            // So that we don't attempt to fill the recording buffer again
            // before the main thread stops recording
            SetRecordingException();
            return;
        } else {
            // Don't want to intercept other exceptions (?)
            throw;
        }
    },
                delayedHandler);
}

void AudioIoCallback::SetListener(
    const std::shared_ptr< AudioIOListener >& listener)
{
    if (IsBusy()) {
        return;
    }

    mListener = listener;
}

static void DoSoftwarePlaythrough(constSamplePtr inputBuffer,
                                  sampleFormat inputFormat,
                                  unsigned inputChannels,
                                  float* outputBuffer,
                                  unsigned long len)
{
    for (unsigned int i=0; i < inputChannels; i++) {
        auto inputPtr = inputBuffer + (i * SAMPLE_SIZE(inputFormat));

        SamplesToFloats(inputPtr, inputFormat,
                        outputBuffer + i, len, inputChannels, 2);
    }

    // One mono input channel goes to both output channels...
    if (inputChannels == 1) {
        for (int i=0; i < len; i++) {
            outputBuffer[2 * i + 1] = outputBuffer[2 * i];
        }
    }
}

void audacityAudioCallback(const float* inputBuffer, float* outputBuffer,
                           unsigned int framesPerBuffer, double streamTime,
                           unsigned int statusFlags, void* userData)
{
    // Translate rta's single streamTime into PA's tri-field struct. We share
    // the value across input and output buffer times so the
    // (inputBufferAdcTime - outputBufferDacTime) offset comes out to zero —
    // the actual duplex latency is already baked into
    // mRecordingSchedule.mLatencyCompensation at StartAudioStream time.
    AudioStreamCallbackTimeInfo timeInfo;
    timeInfo.inputBufferAdcTime  = streamTime;
    timeInfo.currentTime         = streamTime;
    timeInfo.outputBufferDacTime = streamTime;

    // RtAudioStreamStatus is a 32-bit bitmask; map RTAUDIO_INPUT_OVERFLOW to
    // AudioStream* flags so the inner callback can read them uniformly.
    AudioStreamCallbackFlags paStatus = 0;
    if (statusFlags & RTAUDIO_INPUT_OVERFLOW) {
        paStatus |= AudioStreamInputOverflow;
    }

    auto gAudioIO = AudioIO::Get();
    (void)gAudioIO->AudioCallback(
        reinterpret_cast<constSamplePtr>(inputBuffer),
        outputBuffer, framesPerBuffer,
        &timeInfo, paStatus, userData);
}

// Stop recording if 'silence' is detected
// Start recording if sound detected.
//
//   By using CallAfter(), we can schedule the call to the toolbar
//   to run in the main GUI thread after the next event loop iteration.
//   That's important, because Pause() updates GUI, such as status bar,
//   and that should NOT happen in this audio non-gui thread.
void AudioIoCallback::CheckSoundActivatedRecordingLevel(
    float* inputSamples,
    unsigned long framesPerBuffer)
{
    // Quick returns if next to nothing to do.
    if (!mPauseRec) {
        return;
    }

    float maxPeak = 0.;
    for ( unsigned long i = 0, cnt = framesPerBuffer * mNumCaptureChannels; i < cnt; ++i ) {
        float sample = fabs(*(inputSamples++));
        if (sample > maxPeak) {
            maxPeak = sample;
        }
    }

    bool bShouldBePaused = maxPeak < mSilenceLevel;
    if (bShouldBePaused != IsPaused()) {
        auto pListener = GetListener();
        if (pListener) {
            pListener->OnSoundActivationThreshold();
        }
    }
}

// Limit values to -1.0..+1.0
void ClampBuffer(float* pBuffer, unsigned long len)
{
    for (unsigned i = 0; i < len; i++) {
        pBuffer[i] = std::clamp(pBuffer[i], -1.0f, 1.0f);
    }
}

// return true, IFF we have fully handled the callback.
//
// Mix and copy to the output buffer
// from our intermediate playback buffers
//
bool AudioIoCallback::FillOutputBuffers(
    float* outputFloats,
    unsigned long framesPerBuffer,
    float* outputMeterFloats)
{
    const auto numPlaybackSequences = mPlaybackSequences.size();
    const auto numPlaybackChannels = mNumPlaybackChannels;

    mMaxFramesOutput = 0;

    // Quick returns if next to nothing to do.
    if (mStreamToken <= 0
        || !outputFloats
        || numPlaybackChannels <= 0) {
        // So that UpdateTimePosition() will be correct, in case of MIDI play with
        // no audio output channels
        mMaxFramesOutput = framesPerBuffer;
        return false;
    }

    if (mSeek && !mPlaybackSchedule.GetPolicy().AllowSeek(mPlaybackSchedule)) {
        mSeek = 0.0;
    }

    if (mSeek) {
        mCallbackReturn = CallbackDoSeek();
        return true;
    }

    // Choose a common size to take from all ring buffers
    const auto currentlyAvailableFramesAcrossBuffers = std::min<size_t>(framesPerBuffer, GetCommonlyReadyPlayback());

    // Poke: If there are no playback sequences, then check playback
    // completion condition and do early return
    // PRL:  Also consume frbom the single playback ring buffer
    if (numPlaybackSequences == 0) {
        mMaxFramesOutput = mPlaybackBuffers[0]->Discard(currentlyAvailableFramesAcrossBuffers);
        CallbackCheckCompletion(mCallbackReturn, 0);
        mLastPlaybackTimeMillis = ::wxGetUTCTimeMillis();
        return false;
    }

    // JKC: The original code attempted to be faster by doing nothing on silenced audio.
    // This, IMHO, is 'premature optimisation'.  Instead clearer and cleaner code would
    // simply use a volume of 0.0 for silent audio and go on through to the stage of
    // applying that 0.0 volume to the data mixed into the buffer.
    // Then (and only then) we would have if needed fast paths for:
    // - Applying a uniform volume of 0.0.
    // - Applying a uniform volume of 1.0.
    // - Applying some other uniform volume.
    // - Applying a linearly interpolated volume.
    // I would expect us not to need the fast paths, since linearly interpolated volume
    // is very cheap to process.

    // ------ MEMORY ALLOCATION ----------------------
    // These are small structures.
    const auto tempBufs = stackAllocate(float*, numPlaybackChannels);

    // And these are larger structures....
    for (unsigned int c = 0; c < numPlaybackChannels; c++) {
        tempBufs[c] = stackAllocate(float, framesPerBuffer);
    }
    // ------ End of MEMORY ALLOCATION ---------------

    auto playbackVolume = GetMixerOutputVol();
    if (mForceFadeOut.load(std::memory_order_relaxed) || IsPaused()) {
        playbackVolume = 0.0;
    }

    for (unsigned n = 0; n < numPlaybackChannels; ++n) {
        decltype(framesPerBuffer) numberOfRetrievedFrames = mPlaybackBuffers[n]->Get(
            reinterpret_cast<samplePtr>(tempBufs[n]),
            floatSample,
            currentlyAvailableFramesAcrossBuffers
            );

        if (numberOfRetrievedFrames < framesPerBuffer) {
            // This used to happen normally at the end of non-looping
            // plays, but it can also be an anomalous case where the
            // supply from SequenceBufferExchange fails to keep up with the
            // real-time demand in this thread (see bug 1932). We
            // must supply something to the sound card, so pad it with
            // zeroes and not random garbage.
            memset((void*)&tempBufs[n][numberOfRetrievedFrames], 0,
                   (framesPerBuffer - numberOfRetrievedFrames) * sizeof(float));
        }

        // PRL:  More recent rewrites of SequenceBufferExchange should guarantee a
        // padding out of the ring buffers so that equal lengths are
        // available, so maxLen ought to increase from 0 only once
        mMaxFramesOutput = std::max(mMaxFramesOutput, numberOfRetrievedFrames);

        numberOfRetrievedFrames = mMaxFramesOutput;

        // Realtime effect transformation of the sound used to happen here
        // but it is now done already on the producer side of the RingBuffer

        // Mix the results with the existing output (software playthrough) and
        // apply panning.  If post panning effects are desired, the panning would
        // need to be be split out from the mixing and applied in a separate step.

        // Our channels aren't silent.  We need to pass their data on.
        //
        // Each channel in the sequences can output to more than one channel on
        // the device. For example mono channels output to both left and right
        // output channels.
        if (numberOfRetrievedFrames > 0) {
            if (n == 0) {
                using namespace std::chrono;
                const auto now = steady_clock::now();
                const auto adcTime = now + milliseconds(static_cast<int>(mHardwarePlaybackLatencyMs));
                mAudioCallbackInfoQueue.Put({ adcTime, static_cast<int>(numberOfRetrievedFrames) });
            }

            // Output volume emulation: possibly copy meter samples, then
            // apply volume, then copy to the output buffer
            if (outputMeterFloats != outputFloats) {
                for ( unsigned i = 0; i < numberOfRetrievedFrames; ++i) {
                    outputMeterFloats[numPlaybackChannels * i + n]
                        +=playbackVolume * tempBufs[n][i];
                }
            }

            auto oldVolume = mOldPlaybackVolume;
            // if no microfades, jump in volume.
            if (!mbMicroFades) {
                oldVolume = playbackVolume;
            }

            // Linear interpolate.
            // PRL todo:  choose denominator differently, so it doesn't depend on
            // framesPerBuffer, which is influenced by the portAudio implementation in
            // opaque ways
            const float deltaVolume = (playbackVolume - oldVolume) / numberOfRetrievedFrames;
            for (unsigned i = 0; i < numberOfRetrievedFrames; i++) {
                outputFloats[numPlaybackChannels * i + n]
                    +=(oldVolume + deltaVolume * i) * tempBufs[n][i];
            }
        }
        CallbackCheckCompletion(mCallbackReturn, numberOfRetrievedFrames);
    }

    mOldPlaybackVolume = playbackVolume;

    // wxASSERT( maxLen == toGet );

    mLastPlaybackTimeMillis = ::wxGetUTCTimeMillis();

    ClampBuffer(outputFloats, framesPerBuffer * numPlaybackChannels);
    if (outputMeterFloats != outputFloats) {
        ClampBuffer(outputMeterFloats, framesPerBuffer * numPlaybackChannels);
    }

    return false;
}

void AudioIoCallback::UpdateTimePosition(unsigned long framesPerBuffer)
{
    // Quick returns if next to nothing to do.
    if (mStreamToken <= 0) {
        return;
    }

    // Update the position seen by drawing code
    mPlaybackSchedule.SetSequenceTime(
        mPlaybackSchedule.mTimeQueue.Consumer(framesPerBuffer, mRate));
}

// return true, IFF we have fully handled the callback.
//
// Copy from the input buffers to our intermediate recording buffers.
//
void AudioIoCallback::DrainInputBuffers(
    constSamplePtr inputBuffer,
    unsigned long framesPerBuffer,
    const AudioStreamCallbackFlags statusFlags,
    float* tempFloats)
{
    const auto numPlaybackChannels = mNumPlaybackChannels;
    const auto numCaptureChannels = mNumCaptureChannels;

    // Quick returns if next to nothing to do.
    if (mStreamToken <= 0) {
        return;
    }
    if (!inputBuffer) {
        return;
    }
    if (numCaptureChannels <= 0) {
        return;
    }

    // If there are no playback sequences, and we are recording, then the
    // earlier checks for being past the end won't happen, so do it here.
    if (mPlaybackSchedule.GetPolicy().Done(mPlaybackSchedule, 0)) {
        mCallbackReturn = AudioCallbackComplete;
    }

    // The error likely from a too-busy CPU falling behind real-time data
    // is AudioStreamInputOverflow
    bool inputError
        =(statusFlags & (AudioStreamInputOverflow))
          && !(statusFlags & AudioStreamPrimingOutput);

    // But it seems it's easy to get false positives, at least on Mac
    // So we have not decided to enable this extra detection yet in
    // production

    size_t len = framesPerBuffer;
    for (unsigned t = 0; t < numCaptureChannels; t++) {
        len = std::min(len, mCaptureBuffers[t]->AvailForPut());
    }

    if (mSimulateRecordingErrors && 100LL * rand() < RAND_MAX) {
        // Make spurious errors for purposes of testing the error
        // reporting
        len = 0;
    }

    // A different symptom is that len < framesPerBuffer because
    // the other thread, executing SequenceBufferExchange, isn't consuming fast
    // enough from mCaptureBuffers; maybe it's CPU-bound, or maybe the
    // storage device it writes is too slow
    if (mDetectDropouts
        && ((mDetectUpstreamDropouts.load(std::memory_order_relaxed)
             && inputError)
            || len < framesPerBuffer)) {
        // Assume that any good partial buffer should be written leftmost
        // and zeroes will be padded after; label the zeroes.
        auto start = mPlaybackSchedule.GetSequenceTime()
                     + len / mRate + mRecordingSchedule.mLatencyCompensation;
        auto duration = (framesPerBuffer - len) / mRate;
        auto pLast = mLostCaptureIntervals.empty()
                     ? nullptr : &mLostCaptureIntervals.back();
        if (pLast
            && fabs(pLast->first + pLast->second - start) < 0.5 / mRate) {
            // Make one bigger interval, not two butting intervals
            pLast->second = start + duration - pLast->first;
        } else {
            mLostCaptureIntervals.emplace_back(start, duration);
        }
    }

    if (len < framesPerBuffer) {
        mLostSamples += (framesPerBuffer - len);
        wxPrintf(wxT("lost %d samples\n"), (int)(framesPerBuffer - len));
    }

    if (len <= 0) {
        return;
    }

    // We have an ASSERT in the AudioIO constructor to alert us to
    // possible issues with the (short*) cast.  We'd have a problem if
    // sizeof(short) > sizeof(float) since our buffers are sized for floats.
    for (unsigned t = 0; t < numCaptureChannels; t++) {
        // dmazzoni:
        // Un-interleave.  Ugly special-case code required because the
        // capture channels could be in three different sample formats;
        // it'd be nice to be able to call CopySamples, but it can't
        // handle multiplying by the gain and then clipping.  Bummer.

        switch (mCaptureFormat) {
        case floatSample: {
            auto inputFloats = (const float*)inputBuffer;
            for (unsigned i = 0; i < len; i++) {
                tempFloats[i]
                    =inputFloats[numCaptureChannels * i + t];
            }
        } break;
        case int24Sample:
            // We should never get here. Audacity's int24Sample format
            // is different from the backend's sample format and so we
            // request float samples when recording in
            // 24-bit samples.
            wxASSERT(false);
            break;
        case int16Sample: {
            auto inputShorts = (const short*)inputBuffer;
            short* tempShorts = (short*)tempFloats;
            for ( unsigned i = 0; i < len; i++) {
                float tmp = inputShorts[numCaptureChannels * i + t];
                tmp = std::clamp(tmp, -32768.0f, 32767.0f);
                tempShorts[i] = (short)(tmp);
            }
        } break;
        } // switch

        // JKC: mCaptureFormat must be for samples with sizeof(float) or
        // fewer bytes (because tempFloats is sized for floats).  All
        // formats are 2 or 4 bytes, so we are OK.
        const auto put
            =mCaptureBuffers[t]->Put(
                  (samplePtr)tempFloats, mCaptureFormat, len);
        // wxASSERT(put == len);
        // but we can't assert in this thread
        wxUnusedVar(put);
        mCaptureBuffers[t]->Flush();
    }
}

// return true, IFF we have fully handled the callback.
// Prime the output buffer with 0's, optionally adding in the playthrough.
void AudioIoCallback::DoPlaythrough(
    constSamplePtr inputBuffer,
    float* outputBuffer,
    unsigned long framesPerBuffer,
    float* outputMeterFloats)
{
    const auto numCaptureChannels = mNumCaptureChannels;
    const auto numPlaybackChannels = mNumPlaybackChannels;

    // Quick returns if next to nothing to do.
    if (!outputBuffer) {
        return;
    }
    if (numPlaybackChannels <= 0) {
        return;
    }

    float* outputFloats = outputBuffer;
    for (unsigned i = 0; i < framesPerBuffer * numPlaybackChannels; i++) {
        outputFloats[i] = 0.0;
    }

    if (inputBuffer && mSoftwarePlaythrough) {
        DoSoftwarePlaythrough(inputBuffer, mCaptureFormat,
                              numCaptureChannels,
                              outputBuffer, framesPerBuffer);
    }

    // Copy the results to outputMeterFloats if necessary
    if (outputMeterFloats != outputFloats) {
        for (unsigned i = 0; i < framesPerBuffer * numPlaybackChannels; ++i) {
            outputMeterFloats[i] = outputFloats[i];
        }
    }
}

/* Send data to recording VU meter if applicable */
// Also computes rms
void AudioIoCallback::SendVuInputMeterData(const float* inputSamples, unsigned long framesPerBuffer, const TimePoint& dacTime)
{
    if (framesPerBuffer == 0) {
        return;
    }

    auto inputMeter = mInputMeter.lock();
    if (!inputMeter) {
        return;
    }

    if (!inputSamples) {
        return;
    }

    PushInputMeterValues(inputMeter, inputSamples, framesPerBuffer, dacTime);
}

/* Send data to playback VU meter if applicable */
void AudioIoCallback::SendVuOutputMeterData(const float* outputMeterFloats, unsigned long framesPerBuffer, const TimePoint& dacTime)
{
    if (framesPerBuffer == 0) {
        return;
    }

    auto outputMeter = mOutputMeter.lock();
    if (!outputMeter) {
        return;
    }

    if (!outputMeterFloats) {
        return;
    }

    PushMasterOutputMeterValues(outputMeter, outputMeterFloats, mNumPlaybackChannels, framesPerBuffer, dacTime);
    PushTrackMeterValues(outputMeter, framesPerBuffer, dacTime);
}

void AudioIoCallback::PushInputMeterValues(const IMeterSenderPtr& sender, const float* values, unsigned long frames,
                                           const TimePoint& dacTime)
{
    if (frames == 0) {
        return;
    }

    // Update meter tracks
    auto sptr = values;
    for (const auto& sequence : mCaptureSequences) {
        auto nChannels = sequence->NChannels();
        const int64_t id = sequence->GetRecordableSequenceId();
        for (size_t ch = 0; ch < nChannels; ch++) {
            // Map track channel to input channel, wrapping if track has more channels than input
            size_t inputCh = ch % mNumCaptureChannels;
            sender->push(ch, { sptr + inputCh, frames, mNumCaptureChannels, dacTime }, IMeterSender::TrackId { id });
        }
    }

    // Update main meter
    // If the input source has more than 2 channels it will be splitted on multiple mono sequences
    if (mNumCaptureChannels <= 2) {
        for (size_t ch = 0; ch < mNumCaptureChannels; ++ch) {
            sender->push(ch, { sptr + ch, frames, mNumCaptureChannels, dacTime });
        }
    } else {
        constexpr size_t maxMainTrackChannels = 2;
        const auto mainTrackInput = stackAllocate(float, frames * maxMainTrackChannels);
        std::memset(mainTrackInput, 0, frames * maxMainTrackChannels * sizeof(float));

        for (size_t i = 0; i < frames; ++i) {
            for (size_t seqNum = 0; seqNum < mCaptureSequences.size(); seqNum++) {
                const auto channel = seqNum % maxMainTrackChannels;
                mainTrackInput[channel * frames + i] = std::max(
                    mainTrackInput[channel * frames + i], *sptr);
                sptr++;
            }
        }

        for (size_t ch = 0; ch < maxMainTrackChannels; ++ch) {
            sender->push(ch, { mainTrackInput + ch * frames, frames, 1, dacTime });
        }
    }
}

void AudioIoCallback::PushMasterOutputMeterValues(const IMeterSenderPtr& sender, const float* values, uint8_t channels,
                                                  unsigned long frames,
                                                  const TimePoint& dacTime)
{
    auto sptr = values;
    for (size_t ch = 0; ch < channels; ++ch) {
        auto sptr = values + ch;
        sender->push(ch, { sptr, frames, channels, dacTime });
    }
}

void AudioIoCallback::PushTrackMeterValues(const IMeterSenderPtr& sender, unsigned long frames, const TimePoint& dacTime)
{
    auto stackBuffer = stackAllocate(float, frames);

    for (const Track& track: mPlaybackTracks) {
        const auto nChannels = track.mSequence->NChannels();
        for (size_t nch = 0; nch < nChannels; ++nch) {
            const auto& buffer = track.mBuffers[nch];
            size_t len = buffer->Get(
                reinterpret_cast<samplePtr>(stackBuffer),
                floatSample,
                frames
                );

            sender->push(nch, { stackBuffer, len, 1, dacTime }, IMeterSender::TrackId { track.trackId() });
        }
    }
}

unsigned AudioIoCallback::CountSoloingSequences()
{
    const auto numPlaybackSequences = mPlaybackSequences.size();

    // MOVE_TO: CountSoloingSequences() function
    unsigned numSolo = 0;
    for (unsigned t = 0; t < numPlaybackSequences; t++ ) {
        if (mPlaybackSequences[t]->GetSolo()) {
            numSolo++;
        }
    }
    auto range = Extensions();
    numSolo += std::accumulate(range.begin(), range.end(), 0,
                               [](unsigned sum, auto& ext){
        return sum + ext.CountOtherSolo();
    });
    return numSolo;
}

// TODO: Consider making the two sequence status functions into member
// functions of sequence objects

// true IFF the sequence should be silent.
// The sequence may not yet be silent, since it may still be
// fading out.
bool AudioIoCallback::SequenceShouldBeSilent(const PlayableSequence& ps)
{
    return !ps.GetSolo() && (
        // Cut if somebody else is soloing
        mbHasSoloSequences
        ||// Cut if we're muted (and not soloing)
        ps.GetMute()
        );
}

AudioIoCallback::AudioIoCallback()
{
    auto& factories = AudioIOExt::GetFactories();
    for (auto& factory: factories) {
        if (auto pExt = factory(mPlaybackSchedule)) {
            mAudioIOExt.push_back(move(pExt));
        }
    }
}

AudioIoCallback::~AudioIoCallback()
{
}

int AudioIoCallback::AudioCallback(
    constSamplePtr inputBuffer, float* outputBuffer,
    unsigned long framesPerBuffer,
    const AudioStreamCallbackTimeInfo* timeInfo,
    const AudioStreamCallbackFlags statusFlags, void* WXUNUSED(userData))
{
    // Poll sequences for change of state.
    // (User might click mute and solo buttons.)
    mbHasSoloSequences = CountSoloingSequences() > 0;
    mCallbackReturn = AudioCallbackContinue;

    if (IsPaused()
        // PRL:  Why was this added?  Was it only because of the mysterious
        // initial leading zeroes, now solved by setting mStreamToken early?
        // JKC: I think it's used for the MIDI time cursor.  See comments
        // at head of file about AudioTime().
        || mStreamToken <= 0
        ) {
        mNumPauseFrames += framesPerBuffer;
    }

    for ( auto& ext : Extensions()) {
        ext.ComputeOtherTimings(mRate, IsPaused(),
                                timeInfo,
                                framesPerBuffer);
        ext.FillOtherBuffers(
            mRate, mNumPauseFrames, IsPaused(), mbHasSoloSequences);
    }

    // ------ MEMORY ALLOCATIONS -----------------------------------------------
    // tempFloats will be a reusable scratch pad for (possibly format converted)
    // audio data.  One temporary use is for the InputMeter data.
    const auto numPlaybackChannels = mNumPlaybackChannels;
    const auto numCaptureChannels = mNumCaptureChannels;
    const auto tempFloats = stackAllocate(float,
                                          framesPerBuffer * std::max(numCaptureChannels, numPlaybackChannels));

    bool bVolEmulationActive
        =(outputBuffer && GetMixerOutputVol() != 1.0);
    // outputMeterFloats is the scratch pad for the output meter.
    // we can often reuse the existing outputBuffer and save on allocating
    // something new.
    const auto outputMeterFloats = bVolEmulationActive
                                   ? stackAllocate(float, framesPerBuffer * numPlaybackChannels)
                                   : outputBuffer;
    // ----- END of MEMORY ALLOCATIONS ------------------------------------------

    const auto levelDisplayTime = std::chrono::steady_clock::now()
                                  + std::chrono::milliseconds(static_cast<int>(mHardwarePlaybackLatencyMs));

    if (inputBuffer && numCaptureChannels) {
        float* inputSamples;

        if (mCaptureFormat == floatSample) {
            inputSamples = (float*)inputBuffer;
        } else {
            SamplesToFloats(reinterpret_cast<constSamplePtr>(inputBuffer),
                            mCaptureFormat, tempFloats, framesPerBuffer * numCaptureChannels);
            inputSamples = tempFloats;
        }

        SendVuInputMeterData(inputSamples, framesPerBuffer, levelDisplayTime);

        // This function may queue up a pause or resume.
        // TODO this is a bit dodgy as it toggles the Pause, and
        // relies on an idle event to have handled that, so could
        // queue up multiple toggle requests and so do nothing.
        // Eventually it will sort itself out by random luck, but
        // the net effect is a delay in starting/stopping sound activated
        // recording.
        CheckSoundActivatedRecordingLevel(
            inputSamples,
            framesPerBuffer);
    }

    // Even when paused, we do playthrough.
    // Initialise output buffer to zero or to playthrough data.
    // Initialise output meter values.
    DoPlaythrough(
        inputBuffer,
        outputBuffer,
        framesPerBuffer,
        outputMeterFloats);

    // Test for no sequence audio to play (because we are paused and have faded
    // out)
    if (IsPaused() && ((!mbMicroFades) || mOldPlaybackVolume == 0.0f)) {
        return mCallbackReturn;
    }

    // To add sequence output to output (to play sound on speaker)
    // possible exit, if we were seeking.
    if (FillOutputBuffers(
            outputBuffer,
            framesPerBuffer,
            outputMeterFloats)) {
        return mCallbackReturn;
    }

    // To capture input into sequence (sound from microphone)
    DrainInputBuffers(
        inputBuffer,
        framesPerBuffer,
        statusFlags,
        tempFloats);

    SendVuOutputMeterData(outputMeterFloats, framesPerBuffer, levelDisplayTime);

    return mCallbackReturn;
}

int AudioIoCallback::CallbackDoSeek()
{
    const int token = mStreamToken;
    wxMutexLocker locker(mSuspendAudioThread);
    if (token != mStreamToken) {
        // This stream got destroyed while we waited for it
        return AudioCallbackAbort;
    }

    // Pause audio thread and wait for it to finish
    //
    // [PM] the following 8 lines of code could be probably replaced by
    // a single call to StopAudioThreadAndWait()
    //
    // CAUTION: when trying the above, you must also replace the setting of the
    // atomic before the return, with a call to StartAudioThread()
    //
    // If that works, then we can remove mAudioThreadSequenceBufferExchangeLoopActive,
    // as it will become unused; consequently, the AudioThread loop would get simpler too.
    //
    mAudioThreadSequenceBufferExchangeLoopRunning
    .store(false, std::memory_order_relaxed);

    while (mAudioThreadSequenceBufferExchangeLoopActive
           .load(std::memory_order_relaxed))
    {
        using namespace std::chrono;
        std::this_thread::sleep_for(50ms);
    }

    // Calculate the NEW time position, in the audio callback
    const auto time
        =mPlaybackSchedule.GetPolicy().OffsetSequenceTime(mPlaybackSchedule, mSeek);

    mPlaybackSchedule.SetSequenceTime(time);
    mSeek = 0.0;

    // Reset mixer positions and flush buffers for all sequences
    for (auto& mixer : mPlaybackMixers) {
        mixer->Reposition(time, true);
    }
    for (auto& buffer : mPlaybackBuffers) {
        const auto toDiscard = buffer->AvailForGet();
        const auto discarded = buffer->Discard(toDiscard);
        // wxASSERT( discarded == toDiscard );
        // but we can't assert in this thread
        wxUnusedVar(discarded);
    }
    for (auto& track : mPlaybackTracks) {
        for (auto& buffer : track.mBuffers) {
            const auto toDiscard = buffer->AvailForGet();
            buffer->Discard(toDiscard);
        }
    }

    mPlaybackSchedule.mTimeQueue.Prime(time);

    // Reload the ring buffers
    ProcessOnceAndWait();

    // Reenable the audio thread
    mAudioThreadSequenceBufferExchangeLoopRunning
    .store(true, std::memory_order_relaxed);

    return AudioCallbackContinue;
}

void AudioIoCallback::CallbackCheckCompletion(
    int& callbackReturn, unsigned long len)
{
    if (IsPaused()) {
        return;
    }

    bool done
        =mPlaybackSchedule.GetPolicy().Done(mPlaybackSchedule, len);
    if (!done) {
        return;
    }

    for ( auto& ext : Extensions()) {
        ext.SignalOtherCompletion();
    }
    callbackReturn = AudioCallbackComplete;
}

auto AudioIoCallback::AudioIOExtIterator::operator *() const -> AudioIOExt
&
{
    // Down-cast and dereference are safe because only AudioIOCallback
    // populates the array
    return *static_cast<AudioIOExt*>(mIterator->get());
}

void AudioIoCallback::StartAudioThread()
{
    mAudioThreadSequenceBufferExchangeLoopRunning.store(true, std::memory_order_release);
}

void AudioIoCallback::WaitForAudioThreadStarted()
{
    while (mAudioThreadAcknowledge.load(std::memory_order_acquire) != Acknowledge::eStart)
    {
        using namespace std::chrono;
        std::this_thread::sleep_for(50ms);
    }
    mAudioThreadAcknowledge.store(Acknowledge::eNone, std::memory_order_release);
}

void AudioIoCallback::StopAudioThread()
{
    mAudioThreadSequenceBufferExchangeLoopRunning.store(false, std::memory_order_release);
}

void AudioIoCallback::WaitForAudioThreadStopped()
{
    while (mAudioThreadAcknowledge.load(std::memory_order_acquire) != Acknowledge::eStop)
    {
        using namespace std::chrono;
        std::this_thread::sleep_for(50ms);
    }
    mAudioThreadAcknowledge.store(Acknowledge::eNone, std::memory_order_release);
}

void AudioIoCallback::ProcessOnceAndWait(std::chrono::milliseconds sleepTime)
{
    mAudioThreadShouldCallSequenceBufferExchangeOnce
    .store(true, std::memory_order_release);

    while (mAudioThreadShouldCallSequenceBufferExchangeOnce
           .load(std::memory_order_acquire))
    {
        using namespace std::chrono;
        std::this_thread::sleep_for(sleepTime);
    }
}

bool AudioIO::IsCapturing() const
{
    // Includes a test of mTime, used in the main thread
    return IsStreamActive()
           && GetNumCaptureChannels() > 0
           && mPlaybackSchedule.GetSequenceTime()
           >= mPlaybackSchedule.mT0 + mRecordingSchedule.mLeadInTime;
}

BoolSetting SoundActivatedRecord{ "/AudioIO/SoundActivatedRecord", false };
