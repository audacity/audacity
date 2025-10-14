/*
* Audacity: A Digital Audio Editor
*/

#include "au3record.h"

#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-audio-io/ProjectAudioIO.h"
#include "libraries/lib-audio-devices/AudioIOBase.h"
#include "libraries/lib-time-frequency-selection/ViewInfo.h"
#include "libraries/lib-track-selection/TrackFocus.h"
#include "libraries/lib-track/Track.h"
#include "libraries/lib-track/PendingTracks.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-stretching-sequence/StretchingSequence.h"
#include "libraries/lib-viewport/Viewport.h"

#include "au3audioinput.h"

#include "au3wrap/internal/domconverter.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "au3wrap/au3types.h"

#include "recorderrors.h"

#include "translation.h"
#include "log.h"
#include "ProjectRate.h"

using namespace muse;
using namespace muse::async;
using namespace au::record;
using namespace au::au3;

constexpr int RATE_NOT_SELECTED = -1;
using WritableSampleTrackArray = std::vector< std::shared_ptr< WritableSampleTrack > >;

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

    audioEngine()->updateRequested().onNotify(this, [this]() {
        auto& pendingTracks = PendingTracks::Get(projectRef());
        pendingTracks.UpdatePendingTracks();
    });

    audioEngine()->recordingClipChanged().onReceive(this, [this](const au3::Au3TrackId& trackId, const au3::Au3ClipId& clipId) {
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
                                         [&](const RecordData& r){ return r.clipKey.objectId == clipId; });

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

    audioEngine()->commitRequested().onNotify(this, [this]() {
        for (const RecordData& recordEntry : m_recordData) {
            trackedit::ClipKey clipKey = recordEntry.clipKey;
            Au3WaveTrack* origWaveTrack = DomAccessor::findWaveTrack(projectRef(), ::TrackId(clipKey.trackId));

            std::shared_ptr<Au3WaveClip> origClip = DomAccessor::findWaveClip(origWaveTrack, clipKey.objectId);
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

        dispatcher()->dispatch("playback-seek", muse::actions::ActionData::make_arg1<double>(globalContext()->recordPosition()));

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

    double t0 = playback()->player()->playbackPosition();
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

Au3Project& Au3Record::projectRef() const
{
    Au3Project* project = reinterpret_cast<Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

Ret Au3Record::doRecord(Au3Project& project,
                        const TransportSequences& sequences,
                        double t0, double t1,
                        bool altAppearance,
                        const double audioStreamSampleRate)
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

        int trackCounter = 0;
        for (auto newTrack : newTracks) {
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

            auto list = TrackList::Create(p);

            for (auto& w : newTracks) {
                list->Add(std::static_pointer_cast<Track>(std::move(w)));
            }

            pendingTracks.RegisterPendingNewTracks(*list);

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

    int token = audioEngine()->startStream(transportSequences, t0, t1, t1, project, false, audioStreamSampleRate);

    success = (token != 0);

    if (success) {
        ProjectAudioIO::Get(*p).SetAudioIOToken(token);
    } else {
        cancelRecording();

        Ret ret = make_ret(Err::RecordingError);
        auto gAudioIO = AudioIO::Get();
        ret.setText(String::fromStdString(ret.text()).arg(wxToString(gAudioIO->LastPaErrorString())).toStdString());

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
    projectHistory()->pushHistoryState(trc("record", "Recorded Audio"), trc("record", "Record"));
}

bool Au3Record::canStopAudioStream() const
{
    auto gAudioIO = AudioIO::Get();
    Au3Project& project = projectRef();
    return !gAudioIO->IsStreamActive()
           || gAudioIO->IsMonitoring()
           || gAudioIO->GetOwningProject().get() == &project;
}
