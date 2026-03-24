/*
* Audacity: A Digital Audio Editor
*/

#include "au3record.h"

#include "framework/global/translation.h"
#include "framework/global/log.h"

#include "au3-audio-io/ProjectAudioIO.h"
#include "au3-audio-devices/AudioIOBase.h"
#include "au3-time-frequency-selection/ViewInfo.h"
#include "au3-track/Track.h"
#include "au3-track/PendingTracks.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-stretching-sequence/StretchingSequence.h"
#include "au3-project-rate/ProjectRate.h"

#include "au3wrap/internal/domconverter.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "au3wrap/au3types.h"

#include "au3audioinput.h"
#include "../../recorderrors.h"

using namespace muse;
using namespace muse::async;
using namespace au::record;
using namespace au::au3;

constexpr int RATE_NOT_SELECTED = -1;
using WritableSampleTrackArray = std::vector< std::shared_ptr< WritableSampleTrack > >;

static const muse::actions::ActionQuery PLAYBACK_SEEK_QUERY("action://playback/seek");

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

WritableSampleTrackArray ChooseExistingRecordingTracks(Au3Project& proj, const bool selectedOnly, const double targetRate)
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
            const bool stereoTrackMonoInput
                =(recordingChannels == 1 && nChannels == 2 && candidates.empty());
            if (stereoTrackMonoInput) {
                candidates.clear();
                channelCounts.clear();
                candidates.push_back(candidate->SharedPointer<Au3WaveTrack>());
                channelCounts.push_back(nChannels);
                totalChannels = nChannels;
                break;
            }

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

    if (strictRules && !candidates.empty()) {
        const bool allowDownmix
            =(recordingChannels == 2 && totalChannels == 1 && candidates.size() == 1);
        const bool allowUpmix
            =(recordingChannels == 1 && totalChannels == 2 && candidates.size() == 1
              && !channelCounts.empty() && channelCounts.front() == 2);
        if (allowDownmix || allowUpmix) {
            return candidates;
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
    m_audioInput = std::make_shared<Au3AudioInput>(iocContext());

    audioEngine()->updateRequested().onNotify(this, [this]() {
        if (m_recordData.empty()) {
            return;
        }
        auto& pendingTracks = PendingTracks::Get(projectRef());
        pendingTracks.UpdatePendingTracks();
    });

    audioEngine()->recordingClipChanged().onReceive(this, [this](const au3::Au3TrackId& trackId, const au3::Au3ClipId& clipId) {
        if (m_recordData.empty()) {
            return;
        }
        Au3WaveTrack* origWaveTrack = DomAccessor::findWaveTrack(projectRef(), trackId);

        Au3Track* pendingTrack = PendingTracks::Get(projectRef()).FindPendingTrack(*origWaveTrack);
        if (!pendingTrack) {
            return;
        }

        std::shared_ptr<Au3WaveClip> origClip = DomAccessor::findWaveClip(origWaveTrack, clipId);
        IF_ASSERT_FAILED(origClip) {
            return;
        }

        auto recordedClip = std::find_if(m_recordData.begin(), m_recordData.end(),
                                         [&](const RecordData& r){ return r.clipKey.itemId == clipId; });

        if (!recordedClip->linkedToPendingClip) {
            Au3WaveTrack* pendingWaveTrack = dynamic_cast<Au3WaveTrack*>(pendingTrack);
            IF_ASSERT_FAILED(pendingWaveTrack) {
                return;
            }

            // pending track has only single clip
            std::shared_ptr<Au3WaveClip> pendingClip = DomAccessor::findWaveClip(pendingWaveTrack, size_t(0));
            IF_ASSERT_FAILED(pendingClip) {
                return;
            }

            // audio thread updates the pending clip
            // make original clip point to the pending clip's data
            origClip->LinkToOtherSource(*pendingClip.get());
            recordedClip->linkedToPendingClip = true;
        }

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

        prj->notifyAboutClipChanged(DomConverter::clip(origWaveTrack, origClip.get()));

        if (!muse::RealIsEqual(m_recordPosition.val.to_double(), origClip->GetPlayEndTime())) {
            m_recordPosition.set(origClip->GetPlayEndTime());
        }
    });

    audioEngine()->finished().onNotify(this, [this]() {
        if (m_recordData.empty()) {
            return;
        }
        m_recordPosition.set(m_recordPosition.val);
        m_recordingFinished.notify();
    });

    audioEngine()->commitRequested().onNotify(this, [this]() {
        if (m_recordData.empty()) {
            return;
        }
        for (const RecordData& recordEntry : m_recordData) {
            trackedit::ClipKey clipKey = recordEntry.clipKey;
            Au3WaveTrack* origWaveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));

            std::shared_ptr<Au3WaveClip> origClip = DomAccessor::findWaveClip(origWaveTrack, clipKey.itemId);
            IF_ASSERT_FAILED(origClip) {
                return;
            }

            trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();

            prj->notifyAboutClipChanged(DomConverter::clip(origWaveTrack, origClip.get()));
            m_recordPosition.set(origClip->GetPlayEndTime());
        }

        for (const RecordData& recordEntry : m_recordData) {
            trackedit::ClipKey clipKey = recordEntry.clipKey;
            trackeditInteraction()->makeRoomForClip(clipKey);
        }

        commitRecording();

        muse::actions::ActionQuery q(PLAYBACK_SEEK_QUERY);
        q.addParam("seekTime", muse::Val(globalContext()->recordPosition()));
        q.addParam("triggerPlay", muse::Val(false));
        dispatcher()->dispatch(q);

        auto& pendingTracks = PendingTracks::Get(projectRef());
        pendingTracks.ClearPendingTracks();
        m_recordData.clear();
    });
}

muse::Ret Au3Record::start()
{
    //! NOTE: copied from ProjectAudioManager::OnRecord

    //! TODO: should be configurable
    bool altAppearance = false;

    // TODO: should preferNewTrack be an option?
    // bool bPreferNewTrack = false;
    // gPrefs->Read("/GUI/PreferNewTrackRecord", &bPreferNewTrack, false);
    const bool appendRecord = true;//(altAppearance == bPreferNewTrack);

    Au3Project& project = projectRef();

    double t0 = playbackState()->playbackPosition();
    double t1 = DBL_MAX;
    selectionController()->resetSelectedClips();

    double projectRate = ProjectRate::Get(project).GetRate();

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
            projectRate = rateOfSelected;
        } else {
            if (anySelected && rateOfSelected != projectRate) {
                return make_ret(Err::TooFewCompatibleTracksSelected);
            }

            existingTracks = ChooseExistingRecordingTracks(project, false, projectRate);
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
                return playbackSequence->FindChannelGroup() == wt->FindChannelGroup();
            });
            if (it != end) {
                transportTracks.playbackSequences.erase(it);
            }
        }
    }

    std::copy(existingTracks.begin(), existingTracks.end(),
              back_inserter(transportTracks.captureSequences));

    if (audioEngine()->isBusy()) {
        if (audioEngine()->isCapturing()) {
            return make_ret(Ret::Code::InternalError, std::string("Another project is recording"));
        }
        audioEngine()->stopStream();
    }

    audioEngine()->stopMonitoring();

    return doRecord(project, transportTracks, t0, t1, altAppearance, projectRate);
}

muse::Ret Au3Record::pause()
{
    if (!canStopAudioStream()) {
        return make_ret(Err::RecordingStopError);
    }

    audioEngine()->pauseStream(true);

    return make_ok();
}

muse::Ret Au3Record::stop()
{
    //! NOTE: copied from ProjectAudioManager::Stop
    if (!canStopAudioStream()) {
        return make_ret(Err::RecordingStopError);
    }

    audioEngine()->stopStream();
    //Make sure to unpause
    audioEngine()->pauseStream(false);

    // So that we continue monitoring after playing or recording.
    // also clean the MeterQueues
    Au3Project& project = projectRef();
    auto& projectAudioIO = ProjectAudioIO::Get(project);
    auto playbackMeter = projectAudioIO.GetPlaybackMeter();
    if (playbackMeter) {
        playbackMeter->stop();
    }

    auto captureMeter = projectAudioIO.GetCaptureMeter();
    if (captureMeter) {
        captureMeter->stop();
    }

    return make_ok();
}

muse::Ret Au3Record::punchAndRoll()
{
    Au3Project& project = projectRef();

    // Get cursor position
    double t1 = std::max(0.0, selectionController()->selectionStartTime());

    // Punch and roll at t=0 makes no sense — there's no audio to play as lead-in
    if (t1 == 0.0) {
        return make_ret(Err::PunchAndRollNoValidClipAtCursor);
    }

    // Checking the selected tracks: making sure they all have the same rate
    const auto selectedTracks{ GetPropertiesOfSelected(project) };
    const int rateOfSelected{ selectedTracks.rateOfSelected };
    const bool allSameRate{ selectedTracks.allSameRate };

    if (!allSameRate) {
        return make_ret(Err::MismatchedSamplingRatesError);
    }

    // Find tracks to record into (must be selected)
    auto tracks = ChooseExistingRecordingTracks(project, true, rateOfSelected);
    if (tracks.empty()) {
        return make_ret(Err::PunchAndRollNoTracksSelected);
    }

    // Extract crossfade data from existing audio at the punch point
    std::vector<std::vector<float>> crossfadeData;
    const double crossFadeDuration = std::max(0.0,
        recordConfiguration()->crossfadeDuration() / 1000.0
    );

    bool error = false;
    double newt1 = t1;
    for (const auto& wt : tracks) {
        Au3WaveTrack* waveTrack = dynamic_cast<Au3WaveTrack*>(wt.get());
        if (!waveTrack) {
            continue;
        }
        auto rate = waveTrack->GetRate();
        sampleCount testSample(floor(t1 * rate));
        const auto& intervals = static_cast<const Au3WaveTrack*>(waveTrack)->Intervals();
        auto pred = [rate](sampleCount testSample) {
            return [rate, testSample](const auto& pInterval) {
                auto start = floor(pInterval->Start() * rate + 0.5);
                auto end = floor(pInterval->End() * rate + 0.5);
                auto ts = testSample.as_double();
                return ts >= start && ts < end;
            };
        };
        auto begin = intervals.begin(), end = intervals.end(),
             iter = std::find_if(begin, end, pred(testSample));
        if (iter == end) {
            // Try again, a little to the left (Bug 1890)
            iter = std::find_if(begin, end, pred(testSample - 10));
        }
        if (iter == end) {
            error = true;
        } else {
            // May adjust t1 left
            newt1 = std::min(newt1, (*iter).get()->End() - crossFadeDuration);
        }
    }

    if (error) {
        return make_ret(Err::PunchAndRollNoValidClipAtCursor);
    }

    t1 = newt1;
    for (const auto& wt : tracks) {
        Au3WaveTrack* waveTrack = dynamic_cast<Au3WaveTrack*>(wt.get());
        if (!waveTrack) {
            continue;
        }
        const auto endTime = waveTrack->GetEndTime();
        const auto duration =
            std::max(0.0, std::min(crossFadeDuration, endTime - t1));
        const size_t getLen = floor(duration * waveTrack->GetRate());
        if (getLen > 0) {
            const auto nChannels = std::min<size_t>(2, waveTrack->NChannels());
            crossfadeData.resize(nChannels);
            float* buffers[2]{};
            for (size_t ii = 0; ii < nChannels; ++ii) {
                auto& data = crossfadeData[ii];
                data.resize(getLen);
                buffers[ii] = data.data();
            }
            const sampleCount pos = waveTrack->TimeToLongSamples(t1);
            if (!waveTrack->GetFloats(0, nChannels, buffers, pos, getLen)) {
                return make_ret(Err::RecordingError);
            }
        }
    }

    // Set up transport sequences
    // Unlike AU3, we do NOT clear tracks — AU4 uses non-destructive recording
    bool useDuplex = true;
    TransportSequences transportTracks;
    if (useDuplex) {
        transportTracks = MakeTransportTracks(Au3TrackList::Get(project), false, true);
    } else {
        for (auto& pTrack : tracks) {
            transportTracks.playbackSequences.push_back(
                StretchingSequence::Create(*pTrack, pTrack->GetClipInterfaces()));
        }
    }

    // Recording tracks are also playback tracks (for pre-roll)
    std::copy(tracks.begin(), tracks.end(),
              back_inserter(transportTracks.captureSequences));

    selectionController()->resetSelectedClips();

    if (audioEngine()->isBusy()) {
        if (audioEngine()->isCapturing()) {
            return make_ret(Ret::Code::InternalError, std::string("Another project is recording"));
        }
        audioEngine()->stopStream();
    }

    audioEngine()->stopMonitoring();

    double preRoll = std::max(0.0, std::min(t1, recordConfiguration()->preRollDuration()));

    return doRecord(project, transportTracks, t1, DBL_MAX, false, rateOfSelected, preRoll, &crossfadeData);
}

IAudioInputPtr Au3Record::audioInput() const
{
    return m_audioInput;
}

muse::async::Channel<muse::secs_t> Au3Record::recordPositionChanged() const
{
    return m_recordPosition.ch;
}

secs_t Au3Record::recordPosition() const
{
    return m_recordPosition.val;
}

Notification Au3Record::recordingFinished() const
{
    return m_recordingFinished;
}

Au3Project& Au3Record::projectRef() const
{
    const auto project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

Ret Au3Record::doRecord(Au3Project& project,
                        const TransportSequences& sequences,
                        double t0, double t1,
                        bool altAppearance,
                        const double audioStreamSampleRate,
                        double preRoll,
                        std::vector<std::vector<float>>* crossfadeData)
{
    //! NOTE: copied fromProjectAudioManager::DoRecord

    if (audioEngine()->isBusy()) {
        LOGE() << "Audio IO is busy";
        return make_ret(Ret::Code::InternalError);
    }

    // TODO: what is the meaning of the next line, do we need it?
    // projectAudioManager.SetAppending(!altAppearance);
    UNUSED(altAppearance);

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
            name = (::TranslatableString{ wxT("%s #%d"), {} }.Context("clip name template"))
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
    if (appendRecord) {
        // Append recording:
        // Pad selected/all wave tracks to make them all the same length
        for (const auto& sequence : sequences.captureSequences) {
            Au3WaveTrack* wt{};
            if (!(wt = dynamic_cast<Au3WaveTrack*>(sequence.get()))) {
                assert(false);
                continue;
            }

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

            Au3WaveTrack::IntervalHolder newClip{};
            newClip = insertEmptyInterval(*wt, t0, true);

            // A function that copies all the non-sample data between
            // wave tracks; in case the track recorded to changes scale
            // type (for instance), during the recording.
            auto updater = [this, trackId = wt->GetId(), clipId = newClip->GetId()](Au3Track& d, const Au3Track& s){
                assert(d.NChannels() == s.NChannels());
                auto& dst = static_cast<Au3WaveTrack&>(d);
                auto& src = static_cast<const Au3WaveTrack&>(s);
                dst.Init(src);

                audioEngine()->recordingClipChanged().send(trackId, clipId);
            };

            // Get an empty copy of the track to be recorded into at any position,
            // to be pushed into undo history only later.
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

            m_recordData.push_back(RecordData { trackedit::ClipKey(wt->GetId(), newClip->GetId()), false });

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

        std::vector<WaveTrack::Holder> newTracks;
        if (recordingChannels == 2u) {
            newTracks.push_back(WaveTrackFactory::Get(*p).Create(2));
        } else {
            for (int i = 0; i < recordingChannels; ++i) {
                newTracks.push_back(WaveTrackFactory::Get(*p).Create(1));
            }
        }

        std::vector<std::pair<WaveTrack::Holder, Au3WaveTrack::IntervalHolder> > newTrackHolders;

        int trackCounter = 0;
        for (auto& newTrack : newTracks) {
            // Quantize bounds to the rate of the new track.
            if (t0 < DBL_MAX) {
                t0 = newTrack->SnapToSample(t0);
            }
            if (t1 < DBL_MAX) {
                t1 = newTrack->SnapToSample(t1);
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
            auto newClip = insertEmptyInterval(*newTrack, t0, false);

            newTrackHolders.push_back({ newTrack, newClip });
        }

        auto list = TrackList::Create(p);
        for (auto& w : newTracks) {
            list->Add(std::static_pointer_cast<Track>(std::move(w)));
        }
        pendingTracks.RegisterPendingNewTracks(*list);

        for (auto& [newTrack, newClip] : newTrackHolders) {
            auto updater = [this, trackId = newTrack->GetId(), clipId = newClip->GetId()](Au3Track& d, const Au3Track& s){
                assert(d.NChannels() == s.NChannels());
                auto& dst = static_cast<Au3WaveTrack&>(d);
                auto& src = static_cast<const Au3WaveTrack&>(s);
                dst.Init(src);

                audioEngine()->recordingClipChanged().send(trackId, clipId);
            };

            const auto pending = static_cast<Au3WaveTrack*>(
                pendingTracks.RegisterPendingChangedTrack(updater, newTrack.get())
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

            m_recordData.push_back(RecordData { trackedit::ClipKey(newTrack->GetId(), newClip->GetId()), false });

            trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
            prj->notifyAboutTrackAdded(DomConverter::track(newTrack.get()));
        }
        pendingTracks.UpdatePendingTracks();
    }

    int token = audioEngine()->startStream(transportSequences, t0, t1, t1, project, false, audioStreamSampleRate, preRoll, crossfadeData);

    success = (token != 0);

    if (success) {
        ProjectAudioIO::Get(*p).SetAudioIOToken(token);
    } else {
        cancelRecording();

        Ret ret = make_ret(Err::RecordingError);
        ret.setText(String::fromStdString(ret.text()).arg(audioEngine()->lastErrorString()).toStdString());

        return ret;
    }

    return make_ok();
}

void Au3Record::cancelRecording()
{
    Au3Project& project = projectRef();
    PendingTracks::Get(project).ClearPendingTracks();
}

void Au3Record::commitRecording()
{
    projectHistory()->pushHistoryState(trc("record", "Recorded audio"), trc("record", "Record"));
}

bool Au3Record::canStopAudioStream() const
{
    return audioEngine()->canStopAudioStream(projectRef());
}

au::context::IPlaybackStatePtr Au3Record::playbackState() const
{
    return globalContext()->playbackState();
}
