/*
* Audacity: A Digital Audio Editor
*/

#include "au3record.h"

#include "timer.h"

#include "global/async/async.h"

#include "playback/playbacktypes.h"

#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-audio-io/ProjectAudioIO.h"
#include "libraries/lib-audio-io/AudioIOListener.h"
#include "libraries/lib-audio-devices/AudioIOBase.h"
#include "libraries/lib-time-frequency-selection/ViewInfo.h"
#include "libraries/lib-track-selection/TrackFocus.h"
#include "libraries/lib-track/Track.h"
#include "libraries/lib-track/PendingTracks.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-stretching-sequence/StretchingSequence.h"
#include "libraries/lib-viewport/Viewport.h"
#include "libraries/lib-audio-devices/Meter.h"

#include "au3audioinput.h"

#include "au3wrap/internal/domconverter.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "au3wrap/internal/domau3types.h"
#include "au3wrap/au3types.h"

#include "recorderrors.h"

#include "log.h"

using namespace muse;
using namespace muse::async;
using namespace au::record;
using namespace au::au3;

constexpr int RATE_NOT_SELECTED = -1;
using WritableSampleTrackArray = std::vector< std::shared_ptr< WritableSampleTrack > >;

namespace au::record {
class RecordingListener : public AudioIOListener, public async::Asyncable
{
public:
    RecordingListener()
        : m_timer(std::chrono::milliseconds(100))
    {
        m_timer.onTimeout(this, [this]() {
            m_updateRequested.notify();
        });
    }

    void OnAudioIORate(int /*rate*/) override { }
    void OnAudioIOStartRecording() override { m_timer.start(); }
    void OnAudioIONewBlocks() override { }
    void OnCommitRecording() override { m_commitRequested.notify(); }
    void OnSoundActivationThreshold() override { }
    void OnAudioIOStopRecording() override
    {
        m_finished.notify();
        m_timer.stop();
    }

    Notification updateRequested() { return m_updateRequested; }
    Notification commitRequested() { return m_commitRequested; }
    Notification finished() { return m_finished; }
    async::Channel<trackedit::ClipKey> recordingClipChanged() { return m_recordingClipChanged; }

private:
    Notification m_updateRequested;
    Notification m_commitRequested;
    Notification m_finished;

    async::Channel<trackedit::ClipKey> m_recordingClipChanged;

    muse::Timer m_timer;
};

std::shared_ptr<RecordingListener> s_recordingListener;

static ProjectAudioIO::DefaultOptions::Scope s_defaultOptionsScope {
    [](Au3Project& project, bool newDefault) -> AudioIOStartStreamOptions {
        auto options = ProjectAudioIO::DefaultOptionsFactory(project, newDefault);
        options.listener = s_recordingListener;
        return options;
    } };
}

struct PropertiesOfSelected
{
    bool allSameRate = false;
    int rateOfSelected = RATE_NOT_SELECTED;
    bool anySelected = false;
};

// GetSelectedProperties collects information about
// currently selected audio tracks
PropertiesOfSelected GetPropertiesOfSelected(const Au3Project& proj)
{
    double rateOfSelection = RATE_NOT_SELECTED;

    PropertiesOfSelected result;
    result.allSameRate = true;

    const auto selectedTracks{
        Au3TrackList::Get(proj).Selected<const Au3WaveTrack>() };

    for (const auto& track : selectedTracks) {
        if (rateOfSelection != RATE_NOT_SELECTED
            && track->GetRate() != rateOfSelection) {
            result.allSameRate = false;
        } else if (rateOfSelection == RATE_NOT_SELECTED) {
            rateOfSelection = track->GetRate();
        }
    }

    result.anySelected = !selectedTracks.empty();
    result.rateOfSelected = rateOfSelection;

    return result;
}

WritableSampleTrackArray ChooseExistingRecordingTracks(Au3Project& proj, bool selectedOnly, double targetRate)
{
    auto p = &proj;
    size_t recordingChannels = std::max(0, AudioIORecordChannels.Read());
    bool strictRules = (recordingChannels <= 2);

    // Iterate over all wave tracks, or over selected wave tracks only.
    // If target rate was specified, ignore all tracks with other rates.
    //
    // In the usual cases of one or two recording channels, seek a first-fit
    // unbroken sub-sequence for which the total number of channels matches the
    // required number exactly.  Never drop inputs or fill only some channels
    // of a track.
    //
    // In case of more than two recording channels, choose tracks only among the
    // selected.  Simply take the earliest wave tracks, until the number of
    // channels is enough.  If there are fewer channels than inputs, but at least
    // one channel, then some of the input channels will be dropped.
    //
    // Resulting tracks may be non-consecutive within the list of all tracks
    // (there may be non-wave tracks between, or non-selected tracks when
    // considering selected tracks only.)

    if (!strictRules && !selectedOnly) {
        return {};
    }

    auto& trackList = Au3TrackList::Get(*p);
    WritableSampleTrackArray candidates;
    std::vector<unsigned> channelCounts;
    size_t totalChannels = 0;
    const auto range = trackList.Any<Au3WaveTrack>();
    for (auto candidate : selectedOnly ? range + &Au3Track::IsSelected : range) {
        if (targetRate != RATE_NOT_SELECTED && candidate->GetRate() != targetRate) {
            continue;
        }

        // count channels in this track
        const auto nChannels = candidate->NChannels();
        if (strictRules && nChannels > recordingChannels) {
            // The recording would under-fill this track's channels
            // Can't use any partial accumulated results
            // either.  Keep looking.
            candidates.clear();
            channelCounts.clear();
            totalChannels = 0;
            continue;
        } else {
            // Might use this but may have to discard some of the accumulated
            while (strictRules
                   && nChannels + totalChannels > recordingChannels) {
                candidates.erase(candidates.begin());
                auto nOldChannels = channelCounts[0];
                assert(nOldChannels > 0);
                channelCounts.erase(channelCounts.begin());
                totalChannels -= nOldChannels;
            }
            candidates.push_back(candidate->SharedPointer<Au3WaveTrack>());
            channelCounts.push_back(nChannels);
            totalChannels += nChannels;
            if (totalChannels >= recordingChannels) {
                // Done!
                return candidates;
            }
        }
    }

    if (!strictRules && !candidates.empty()) {
        // good enough
        return candidates;
    }

    // If the loop didn't exit early, we could not find enough channels
    return {};
}

TransportSequences MakeTransportTracks(Au3TrackList& trackList, bool selectedOnly, bool nonWaveToo)
{
    TransportSequences result;
    {
        const auto range = trackList.Any<Au3WaveTrack>()
                           + (selectedOnly ? &Au3Track::IsSelected : &Au3Track::Any);
        for (auto pTrack : range) {
            result.playbackSequences.push_back(
                StretchingSequence::Create(*pTrack, pTrack->GetClipInterfaces()));
        }
    }
    if (nonWaveToo) {
        const auto range = trackList.Any<const PlayableTrack>()
                           + (selectedOnly ? &Au3Track::IsSelected : &Au3Track::Any);
        for (auto pTrack : range) {
            if (!track_cast<const SampleTrack*>(pTrack)) {
                if (auto pSequence
                        =std::dynamic_pointer_cast<const OtherPlayableSequence>(
                              pTrack->shared_from_this())
                        ) {
                    result.otherPlayableSequences.push_back(pSequence);
                }
            }
        }
    }
    return result;
}

void Au3Record::init()
{
    m_audioInput = std::make_shared<Au3AudioInput>();

    s_recordingListener = std::make_shared<RecordingListener>();

    s_recordingListener->updateRequested().onNotify(this, [this]() {
        auto& pendingTracks = PendingTracks::Get(projectRef());
        pendingTracks.UpdatePendingTracks();
    });

    s_recordingListener->recordingClipChanged().onReceive(this, [this](const trackedit::ClipKey& clipKey) {
        Au3WaveTrack* origWaveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(clipKey.trackId));

        Au3Track* pendingTrack = &PendingTracks::Get(projectRef())
                                 .SubstitutePendingChangedTrack(*origWaveTrack);

        Au3WaveTrack* pendingWaveTrack = dynamic_cast<Au3WaveTrack*>(pendingTrack);
        IF_ASSERT_FAILED(pendingWaveTrack) {
            return;
        }

        auto pendingClipId = DomAccessor::findMatchedClip(pendingWaveTrack, origWaveTrack, clipKey.clipId);
        if (pendingClipId == -1) {
            return;
        }

        std::shared_ptr<Au3WaveClip> pendingClip = DomAccessor::findWaveClip(pendingWaveTrack, pendingClipId);
        IF_ASSERT_FAILED(pendingClip) {
            return;
        }

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

        // IMPORTANT: getting pending clip and assigning it to the clip from original track
        // to make that possible we need to assign orignal clip's ID to the pending clip
        // so onClipChanged accepts it
        auto pendingClipWithFakeId = DomConverter::clip(pendingWaveTrack, pendingClip.get());
        pendingClipWithFakeId.key.clipId = clipKey.clipId;
        prj->notifyAboutClipChanged(pendingClipWithFakeId);
    });

    s_recordingListener->commitRequested().onNotify(this, [this]() {
        auto& pendingTracks = PendingTracks::Get(projectRef());
        pendingTracks.ApplyPendingTracks();
    });
}

muse::Ret Au3Record::start()
{
    //! NOTE: copied from ProjectAudioManager::OnRecord

    //! TODO: should be configurable
    bool altAppearance = false;

    bool bPreferNewTrack = false;
    // gPrefs->Read("/GUI/PreferNewTrackRecord", &bPreferNewTrack, false);
    const bool appendRecord = true;//(altAppearance == bPreferNewTrack);

    Au3Project& project = projectRef();

    const auto& selectedRegion = ViewInfo::Get(project).selectedRegion;
    double t0 = selectedRegion.t0();
    double t1 = selectedRegion.t1();
    // When no time selection, recording duration is 'unlimited'.
    if (t1 == t0) {
        t1 = DBL_MAX;
    }

    auto options = ProjectAudioIO::GetDefaultOptions(project);
    WritableSampleTrackArray existingTracks;

    // Checking the selected tracks: counting them and
    // making sure they all have the same rate
    const auto selectedTracks{ GetPropertiesOfSelected(project) };
    const int rateOfSelected{ selectedTracks.rateOfSelected };
    const bool anySelected{ selectedTracks.anySelected };
    const bool allSameRate{ selectedTracks.allSameRate };

    if (!allSameRate) {
        return make_ret(Err::MismatchedSamplingRatesError);
    }

    if (appendRecord) {
        // Try to find wave tracks to record into.  (If any are selected,
        // try to choose only from them; else if wave tracks exist, may record into any.)
        existingTracks = ChooseExistingRecordingTracks(project, true, rateOfSelected);
        if (!existingTracks.empty()) {
            t0 = std::max(t0,
                          Au3TrackList::Get(project).Selected<const Au3WaveTrack>()
                          .max(&Au3Track::GetEndTime));
            options.rate = rateOfSelected;
        } else {
            if (anySelected && rateOfSelected != options.rate) {
                return make_ret(Err::TooFewCompatibleTracksSelected);
            }

            existingTracks = ChooseExistingRecordingTracks(project, false, options.rate);
            if (!existingTracks.empty()) {
                const auto endTime = accumulate(
                    existingTracks.begin(), existingTracks.end(),
                    std::numeric_limits<double>::lowest(),
                    [](double acc, auto& pTrack) {
                    return std::max(acc, pTrack->GetEndTime());
                }
                    );

                //If there is a suitable track, then adjust t0 so
                //that recording not starts before the end of that track
                t0 = std::max(t0, endTime);
            }
            // If suitable tracks still not found, will record into NEW ones,
            // starting with t0
        }

        // Whether we decided on NEW tracks or not:
        if (t1 <= selectedRegion.t0() && selectedRegion.t1() > selectedRegion.t0()) {
            t1 = selectedRegion.t1();   // record within the selection
        } else {
            t1 = DBL_MAX;        // record for a long, long time
        }
    }

    bool useDuplex = true;
    // gPrefs->Read(wxT("/AudioIO/Duplex"), &useDuplex, true); // todo

    TransportSequences transportTracks;
    if (useDuplex) {
        // Remove recording tracks from the list of tracks for duplex ("overdub")
        // playback.
        /* TODO: set up stereo tracks if that is how the user has set up
          * their preferences, and choose sample format based on prefs */
        transportTracks = MakeTransportTracks(Au3TrackList::Get(project), false, true);
        for (const auto& wt : existingTracks) {
            auto end = transportTracks.playbackSequences.end();
            auto it = std::find_if(
                transportTracks.playbackSequences.begin(), end,
                [&wt](const auto& playbackSequence) {
                return playbackSequence->FindChannelGroup()
                == wt->FindChannelGroup();
            });
            if (it != end) {
                transportTracks.playbackSequences.erase(it);
            }
        }
    }

    std::copy(existingTracks.begin(), existingTracks.end(),
              back_inserter(transportTracks.captureSequences));

    return doRecord(project, transportTracks, t0, t1, altAppearance, options);
}

muse::Ret Au3Record::pause()
{
    if (!canStopAudioStream()) {
        return make_ret(Err::RecordingStopError);
    }

    auto gAudioIO = AudioIO::Get();

    gAudioIO->SetPaused(true);

    return make_ok();
}

muse::Ret Au3Record::stop()
{
    //! NOTE: copied from ProjectAudioManager::Stop
    bool stopStream = true;

    if (!canStopAudioStream()) {
        return make_ret(Err::RecordingStopError);
    }

    auto gAudioIO = AudioIO::Get();

    if (stopStream) {
        gAudioIO->StopStream();
    }

    //Make sure you tell gAudioIO to unpause
    gAudioIO->SetPaused(false);

    // So that we continue monitoring after playing or recording.
    // also clean the MeterQueues
    Au3Project& project = projectRef();
    auto& projectAudioIO = ProjectAudioIO::Get(project);
    auto meter = projectAudioIO.GetPlaybackMeter();
    if (meter) {
        meter->Clear();
    }

    meter = projectAudioIO.GetCaptureMeter();
    if (meter) {
        meter->Clear();
    }

    return make_ok();
}

IAudioInputPtr Au3Record::audioInput() const
{
    return m_audioInput;
}

Au3Project& Au3Record::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

Ret Au3Record::doRecord(Au3Project& project,
                        const TransportSequences& sequences,
                        double t0, double t1,
                        bool altAppearance,
                        const AudioIOStartStreamOptions& options)
{
    //! NOTE: copied fromProjectAudioManager::DoRecord

    auto gAudioIO = AudioIO::Get();
    if (gAudioIO->IsBusy()) {
        LOGE() << "Audio IO is busy";
        return make_ret(Ret::Code::InternalError);
    }

    // projectAudioManager.SetAppending(!altAppearance);

    bool success = false;

    auto transportSequences = sequences;

    // Will replace any given capture tracks with temporaries
    transportSequences.captureSequences.clear();

    const auto p = &project;
    auto& trackList = Au3TrackList::Get(project);

    bool appendRecord = !sequences.captureSequences.empty();

    auto insertEmptyInterval
        =[&](Au3WaveTrack& track, double t0, bool placeholder) {
        wxString name;
        for (auto i = 1;; ++i) {
            //i18n-hint a numerical suffix added to distinguish otherwise like-named clips when new record started
            name = XC("%s #%d", "clip name template")
                   .Format(track.GetName(), i).Translation();
            if (!track.HasClipNamed(name)) {
                break;
            }
        }

        auto clip = track.CreateClip(t0, name);
        // So that the empty clip is not skipped for insertion:
        clip->SetIsPlaceholder(true);
        track.InsertInterval(clip, true);
        if (!placeholder) {
            clip->SetIsPlaceholder(false);
        }
        return clip;
    };

    auto& pendingTracks = PendingTracks::Get(project);

    {
        if (appendRecord) {
            // Append recording:
            // Pad selected/all wave tracks to make them all the same length
            for (const auto& sequence : sequences.captureSequences) {
                Au3WaveTrack* wt{};
                if (!(wt = dynamic_cast<Au3WaveTrack*>(sequence.get()))) {
                    assert(false);
                    continue;
                }
                auto endTime = wt->GetEndTime();

                // If the track was chosen for recording and playback both,
                // remember the original in preroll tracks, before making the
                // pending replacement.
                const auto shared = wt->SharedPointer<Au3WaveTrack>();
                // prerollSequences should be a subset of playbackSequences.
                const auto& range = transportSequences.playbackSequences;
                bool prerollTrack = any_of(range.begin(), range.end(),
                                           [&](const auto& pSequence) {
                    return shared.get() == pSequence->FindChannelGroup();
                });
                if (prerollTrack) {
                    transportSequences.prerollSequences.push_back(shared);
                }

                // End of current track is before or at recording start time.
                // Less than or equal, not just less than, to ensure a clip boundary.
                // when append recording.
                //const auto pending = static_cast<WaveTrack*>(newTrack);
                const auto lastClip = wt->GetRightmostClip();
                // RoundedT0 to have a new clip created when punch-and-roll
                // recording with the cursor in the second half of the space
                // between two samples
                // (https://github.com/audacity/audacity/issues/5113#issuecomment-1705154108)
                const auto recordingStart
                    =std::round(t0 * wt->GetRate()) / wt->GetRate();
                const auto recordingStartsBeforeTrackEnd
                    =lastClip && recordingStart < lastClip->GetPlayEndTime();
                // Recording doesn't start before the beginning of the last clip
                // - or the check for creating a new clip or not should be more
                // general than that ...
                assert(
                    !recordingStartsBeforeTrackEnd
                    || lastClip->WithinPlayRegion(recordingStart));
                Au3WaveTrack::IntervalHolder newClip{};
                if (!recordingStartsBeforeTrackEnd
                    || lastClip->HasPitchOrSpeed()) {
                    newClip = insertEmptyInterval(*wt, t0, true);
                }

                trackedit::ClipKey newClipKey(DomConverter::trackId(wt->GetId()), au::au3::WaveClipID(newClip.get()).id);

                // A function that copies all the non-sample data between
                // wave tracks; in case the track recorded to changes scale
                // type (for instance), during the recording.
                auto updater = [newClipKey](Au3Track& d, const Au3Track& s){
                    assert(d.NChannels() == s.NChannels());
                    auto& dst = static_cast<Au3WaveTrack&>(d);
                    auto& src = static_cast<const Au3WaveTrack&>(s);
                    dst.Init(src);

                    s_recordingListener->recordingClipChanged().send(newClipKey);
                };

                // Get a copy of the track to be appended, to be pushed into
                // undo history only later.
                const auto pending = static_cast<Au3WaveTrack*>(
                    pendingTracks.RegisterPendingChangedTrack(updater, wt)
                    );
                // Source clip was marked as placeholder so that it would not be
                // skipped in clip copying.  Un-mark it and its copy now
                if (newClip) {
                    newClip->SetIsPlaceholder(false);
                }
                if (auto copiedClip = pending->NewestOrNewClip()) {
                    copiedClip->SetIsPlaceholder(false);
                }
                transportSequences.captureSequences.push_back(pending->SharedPointer<Au3WaveTrack>());

                trackedit::Clip _newClip = DomConverter::clip(pending, newClip.get());
                trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
                prj->notifyAboutClipAdded(_newClip);
            }
            pendingTracks.UpdatePendingTracks();
        }

        if (transportSequences.captureSequences.empty()) {
            // recording to NEW track(s).
            bool recordingNameCustom, useTrackNumber, useDateStamp, useTimeStamp;
            wxString defaultTrackName, defaultRecordingTrackName;

            // Count the tracks.
            auto numTracks = trackList.Any<const Au3WaveTrack>().size();

            auto recordingChannels = std::max(1, AudioIORecordChannels.Read());

            gPrefs->Read(wxT("/GUI/TrackNames/RecordingNameCustom"), &recordingNameCustom, false);
            gPrefs->Read(wxT("/GUI/TrackNames/TrackNumber"), &useTrackNumber, false);
            gPrefs->Read(wxT("/GUI/TrackNames/DateStamp"), &useDateStamp, false);
            gPrefs->Read(wxT("/GUI/TrackNames/TimeStamp"), &useTimeStamp, false);
            defaultTrackName = trackList.MakeUniqueTrackName(Au3WaveTrack::GetDefaultAudioTrackNamePreference());
            gPrefs->Read(wxT("/GUI/TrackNames/RecodingTrackName"), &defaultRecordingTrackName, defaultTrackName);

            wxString baseTrackName = recordingNameCustom ? defaultRecordingTrackName : defaultTrackName;

            auto newTracks = WaveTrackFactory::Get(*p).CreateMany(recordingChannels);
            const auto first = *newTracks->begin();
            int trackCounter = 0;
            const auto minimizeChannelView = false;//recordingChannels > 2 && !TracksPrefs::TracksFitVerticallyZoomed.Read();
            for (auto newTrack : newTracks->Any<Au3WaveTrack>()) {
                // Quantize bounds to the rate of the new track.
                if (newTrack == first) {
                    if (t0 < DBL_MAX) {
                        t0 = newTrack->SnapToSample(t0);
                    }
                    if (t1 < DBL_MAX) {
                        t1 = newTrack->SnapToSample(t1);
                    }
                }

                newTrack->MoveTo(t0);
                wxString nameSuffix = wxString(wxT(""));

                if (useTrackNumber) {
                    nameSuffix += wxString::Format(wxT("%d"), 1 + (int)numTracks + trackCounter++);
                }

                if (useDateStamp) {
                    if (!nameSuffix.empty()) {
                        nameSuffix += wxT("_");
                    }
                    nameSuffix += wxDateTime::Now().FormatISODate();
                }

                if (useTimeStamp) {
                    if (!nameSuffix.empty()) {
                        nameSuffix += wxT("_");
                    }
                    nameSuffix += wxDateTime::Now().FormatISOTime();
                }

                // ISO standard would be nice, but ":" is unsafe for file name.
                nameSuffix.Replace(wxT(":"), wxT("-"));

                if (baseTrackName.empty()) {
                    newTrack->SetName(nameSuffix);
                } else if (nameSuffix.empty()) {
                    newTrack->SetName(baseTrackName);
                } else {
                    newTrack->SetName(baseTrackName + wxT("_") + nameSuffix);
                }

                //create a new clip with a proper name before recording is started
                insertEmptyInterval(*newTrack, t0, false);

                transportSequences.captureSequences.push_back(
                    std::static_pointer_cast<Au3WaveTrack>(newTrack->shared_from_this())
                    );

                for (auto channel : newTrack->Channels()) {
                    // ChannelView::Get(*channel).SetMinimized(minimizeChannelView);
                }
            }
            pendingTracks.RegisterPendingNewTracks(std::move(*newTracks));
            // Bug 1548.  First of new tracks needs the focus.
            TrackFocus::Get(project).Set(first);
            if (!trackList.empty()) {
                Viewport::Get(project).ShowTrack(**trackList.rbegin());
            }
        }

        int token
            =gAudioIO->StartStream(transportSequences, t0, t1, t1, options);

        success = (token != 0);

        if (success) {
            ProjectAudioIO::Get(*p).SetAudioIOToken(token);
        } else {
            cancelRecording();

            Ret ret = make_ret(Err::RecordingError);
            ret.setText(String::fromStdString(ret.text()).arg(wxToString(gAudioIO->LastPaErrorString())).toStdString());

            return ret;
        }
    }

    return make_ok();
}

void Au3Record::cancelRecording()
{
    Au3Project& project = projectRef();
    PendingTracks::Get(project).ClearPendingTracks();

    //todo: reset tracks
}

bool Au3Record::canStopAudioStream() const
{
    auto gAudioIO = AudioIO::Get();
    Au3Project& project = projectRef();
    return !gAudioIO->IsStreamActive()
           || gAudioIO->IsMonitoring()
           || gAudioIO->GetOwningProject().get() == &project;
}
