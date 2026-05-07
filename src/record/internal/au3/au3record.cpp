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

        // Look up by pendingClipId (stable across deferred creation)
        auto recordedClip = std::find_if(m_recordData.begin(), m_recordData.end(),
                                         [&](const RecordData& r){ return r.pendingClipId == clipId; });
        if (recordedClip == m_recordData.end()) {
            return;
        }

        Au3WaveTrack* origWaveTrack = DomAccessor::findWaveTrack(projectRef(), trackId);
        Au3Track* pendingTrack = PendingTracks::Get(projectRef()).FindPendingTrack(*origWaveTrack);
        if (!pendingTrack) {
            return;
        }

        Au3WaveTrack* pendingWaveTrack = dynamic_cast<Au3WaveTrack*>(pendingTrack);
        IF_ASSERT_FAILED(pendingWaveTrack) {
            return;
        }

        // For lead-in-recording with lead-in time: the clip only exists on the pending track.
        // Only create it on the original when the engine is actually capturing (after lead-in time).
        if (recordedClip->deferredClipCreation) {
            if (!audioEngine()->isCapturing()) {
                return; // Still in lead-in time — don't create the clip yet
            }

            std::shared_ptr<Au3WaveClip> pendingClip = DomAccessor::findWaveClip(pendingWaveTrack, clipId);
            IF_ASSERT_FAILED(pendingClip) {
                return;
            }

            // Create a matching clip on the original track at the recording start position
            auto origClip = origWaveTrack->CreateClip(pendingClip->GetPlayStartTime(), pendingClip->GetName());
            origClip->SetIsPlaceholder(true);
            origWaveTrack->InsertInterval(origClip, true);
            origClip->SetIsPlaceholder(false);

            // Store the original clip key for commit
            recordedClip->clipKey = trackedit::ClipKey(origWaveTrack->GetId(), origClip->GetId());

            // Link original clip to pending clip's data
            origClip->LinkToOtherSource(*pendingClip.get());
            recordedClip->linkedToPendingClip = true;
            recordedClip->deferredClipCreation = false;
            rebuildRecordingClipKeys();

            trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
            prj->notifyAboutClipAdded(DomConverter::clip(origWaveTrack, origClip.get()));

            m_recordPosition.set(origClip->GetPlayEndTime());
            return;
        }

        // Normal path: clip already exists on original track
        std::shared_ptr<Au3WaveClip> origClip = DomAccessor::findWaveClip(origWaveTrack, recordedClip->clipKey.itemId);
        IF_ASSERT_FAILED(origClip) {
            return;
        }

        if (!recordedClip->linkedToPendingClip) {
            std::shared_ptr<Au3WaveClip> pendingClip = DomAccessor::findWaveClip(pendingWaveTrack, size_t(0));
            IF_ASSERT_FAILED(pendingClip) {
                return;
            }

            origClip->LinkToOtherSource(*pendingClip.get());
            recordedClip->linkedToPendingClip = true;
        }

        // Invalidate waveform cache on the original clip so new audio data gets painted.
        // MarkChanged() is only called on the pendingClip by the audio engine; the origClip
        // shares data via LinkToOtherSource but its WaveformPainter cache is never notified.
        origClip->MarkChanged();

        trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        prj->notifyAboutClipChanged(DomConverter::clip(origWaveTrack, origClip.get()));

        m_recordPosition.set(origClip->GetPlayEndTime());
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
            // Skip deferred clips that were never created (cancelled during lead-in time)
            if (recordEntry.deferredClipCreation) {
                continue;
            }
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
            // Skip deferred clips that were never created (cancelled during lead-in time)
            if (recordEntry.deferredClipCreation) {
                continue;
            }
            trackedit::ClipKey clipKey = recordEntry.clipKey;
            trackeditInteraction()->makeRoomForClip(clipKey);
        }

        commitRecording();

        muse::actions::ActionQuery q(PLAYBACK_SEEK_QUERY);
        q.addParam("seekTime", muse::Val(m_recordPosition.val));
        q.addParam("triggerPlay", muse::Val(false));
        dispatcher()->dispatch(q);

        auto& pendingTracks = PendingTracks::Get(projectRef());
        pendingTracks.ClearPendingTracks();
        m_recordData.clear();
        rebuildRecordingClipKeys();
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

muse::Ret Au3Record::leadInRecording()
{
    Au3Project& project = projectRef();

    // Get cursor position
    double t1 = std::max(0.0, selectionController()->selectionStartTime().to_double());

    // Lead-in recording at t=0 makes no sense — there's no audio to play as lead-in
    if (t1 == 0.0) {
        return make_ret(Err::LeadInRecordingNoValidClipAtCursor);
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
        return make_ret(Err::LeadInRecordingNoTracksSelected);
    }

    // Try to extract crossfade data from existing audio at the lead-in point.
    // If the cursor is not inside a clip, skip crossfade — just record from that position.
    std::vector<std::vector<float> > crossfadeData;
    const double crossFadeDuration = std::max(0.0,
                                              recordConfiguration()->crossfadeDuration() / 1000.0
                                              );

    bool cursorInClip = true;
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
            iter = std::find_if(begin, end, pred(testSample - 10));
        }
        if (iter == end) {
            cursorInClip = false;
        } else {
            newt1 = std::min(newt1, (*iter).get()->End() - crossFadeDuration);
        }
    }

    // Only extract crossfade samples if the cursor is inside a clip
    if (cursorInClip) {
        t1 = newt1;
        for (const auto& wt : tracks) {
            Au3WaveTrack* waveTrack = dynamic_cast<Au3WaveTrack*>(wt.get());
            if (!waveTrack) {
                continue;
            }
            const auto endTime = waveTrack->GetEndTime();
            const auto duration
                =std::max(0.0, std::min(crossFadeDuration, endTime - t1));
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
                    LOGW() << "Lead-in recording: GetFloats failed at position " << t1
                           << ", proceeding without crossfade";
                    crossfadeData.clear();
                    break;
                }
                break; // Use crossfade data from first track only
            }
        }
    }

    // Set up transport sequences
    // Unlike AU3, we do NOT clear tracks — AU4 uses non-destructive recording
    // Play all tracks (duplex) so user hears context during lead-in time
    TransportSequences transportTracks;
    transportTracks = MakeTransportTracks(Au3TrackList::Get(project), false, true);

    // Recording tracks are also capture targets
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

    double leadInTime = std::max(0.0, std::min(t1, recordConfiguration()->leadInTimeDuration()));

    auto* pCrossfadeData = crossfadeData.empty() ? nullptr : &crossfadeData;
    Ret ret = doRecord(project, transportTracks, t1, DBL_MAX, false, rateOfSelected, leadInTime, pCrossfadeData);

    if (ret) {
        // Initialize recording position to punch point so that cancelling
        // during lead-in returns to the correct position
        m_recordPosition.set(t1);

        // Set playhead to lead-in time start and trigger position tracking.
        // With isDefaultPlayTrackPolicy=false, the stream time starts from
        // t1-leadInTime so this will align with the actual audio position.
        muse::actions::ActionQuery q(PLAYBACK_SEEK_QUERY);
        q.addParam("seekTime", muse::Val(t1 - leadInTime));
        q.addParam("triggerPlay", muse::Val(false));
        dispatcher()->dispatch(q);
    }

    return ret;
}

IAudioInputPtr Au3Record::audioInput() const
{
    return m_audioInput;
}

muse::async::Channel<muse::secs_t> Au3Record::recordPositionChanged() const
{
    return m_recordPosition.ch;
}

const std::vector<au::trackedit::ClipKey>& Au3Record::recordingClipKeys() const
{
    return m_recordingClipKeys;
}

void Au3Record::rebuildRecordingClipKeys()
{
    m_recordingClipKeys.clear();
    m_recordingClipKeys.reserve(m_recordData.size());
    for (const auto& rd : m_recordData) {
        if (!rd.deferredClipCreation) {
            m_recordingClipKeys.push_back(rd.clipKey);
        }
    }
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
                        double leadInTime,
                        std::vector<std::vector<float> >* crossfadeData)
{
    //! NOTE: copied fromProjectAudioManager::DoRecord

    m_recordPosition.set(t0);

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
    const bool deferClipCreation = (leadInTime > 0.0);

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
            // remember the original in leadInTime tracks, before making the
            // pending replacement.
            const auto shared = wt->SharedPointer<Au3WaveTrack>();
            // leadInTimeSequences should be a subset of playbackSequences.
            const auto& range = transportSequences.playbackSequences;
            bool leadInTimeTrack = any_of(range.begin(), range.end(),
                                          [&](const auto& pSequence) {
                return shared.get() == pSequence->FindChannelGroup();
            });
            if (leadInTimeTrack) {
                transportSequences.leadInTimeSequences.push_back(shared);
            }

            if (deferClipCreation) {
                // Lead-in-recording with lead-in time: don't modify the original track yet.
                // Register pending track first (copy of unmodified original), then
                // create the clip only on the pending track. The original track stays
                // clean — the clip will be created on it when recording data arrives.
                auto clipIdHolder = std::make_shared<au3::Au3ClipId>();

                auto updater = [this, trackId = wt->GetId(), clipIdHolder](Au3Track& d, const Au3Track& s){
                    assert(d.NChannels() == s.NChannels());
                    auto& dst = static_cast<Au3WaveTrack&>(d);
                    auto& src = static_cast<const Au3WaveTrack&>(s);
                    dst.Init(src);

                    audioEngine()->recordingClipChanged().send(trackId, *clipIdHolder);
                };

                const auto pending = static_cast<Au3WaveTrack*>(
                    pendingTracks.RegisterPendingChangedTrack(updater, wt)
                    );

                // Insert clip on pending track only
                auto pendingClip = insertEmptyInterval(*pending, t0, false);
                *clipIdHolder = pendingClip->GetId();

                transportSequences.captureSequences.push_back(pending->SharedPointer<Au3WaveTrack>());

                m_recordData.push_back(RecordData { trackedit::ClipKey(), pendingClip->GetId(), false, true });
                rebuildRecordingClipKeys();
                // Don't notify UI — clip will be created on original when recording starts
            } else {
                // Normal recording: create clip on original track immediately
                Au3WaveTrack::IntervalHolder newClip{};
                newClip = insertEmptyInterval(*wt, t0, true);

                auto updater = [this, trackId = wt->GetId(), clipId = newClip->GetId()](Au3Track& d, const Au3Track& s){
                    assert(d.NChannels() == s.NChannels());
                    auto& dst = static_cast<Au3WaveTrack&>(d);
                    auto& src = static_cast<const Au3WaveTrack&>(s);
                    dst.Init(src);

                    audioEngine()->recordingClipChanged().send(trackId, clipId);
                };

                const auto pending = static_cast<Au3WaveTrack*>(
                    pendingTracks.RegisterPendingChangedTrack(updater, wt)
                    );

                if (newClip) {
                    newClip->SetIsPlaceholder(false);
                }
                if (auto copiedClip = pending->NewestOrNewClip()) {
                    copiedClip->SetIsPlaceholder(false);
                }
                transportSequences.captureSequences.push_back(pending->SharedPointer<Au3WaveTrack>());

                m_recordData.push_back(RecordData { trackedit::ClipKey(wt->GetId(), newClip->GetId()), newClip->GetId(), false, false });
                rebuildRecordingClipKeys();

                trackedit::Clip _newClip = DomConverter::clip(pending, newClip.get());
                trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
                prj->notifyAboutClipAdded(_newClip);
            }
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

            m_recordData.push_back(RecordData { trackedit::ClipKey(newTrack->GetId(), newClip->GetId()), newClip->GetId(), false, false });
            rebuildRecordingClipKeys();

            trackedit::ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
            prj->notifyAboutTrackAdded(DomConverter::track(newTrack.get()));
        }
        pendingTracks.UpdatePendingTracks();
    }

    int token
        = audioEngine()->startStream(transportSequences, t0, t1, t1, project, false, audioStreamSampleRate, leadInTime, crossfadeData);

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
