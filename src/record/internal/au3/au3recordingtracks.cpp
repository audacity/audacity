/*
* Audacity: A Digital Audio Editor
*/

#include "au3recordingtracks.h"

#include "au3-wave-track/WaveTrack.h"

#include "au3wrap/internal/domaccessor.h"

using namespace au::au3;

namespace au::record {
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

WritableSampleTrackArray ChooseExistingRecordingTracks(Au3Project& proj, const bool selectedOnly, const double targetRate,
                                                       const int recordingChannels)
{
    auto p = &proj;
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
    std::vector<int> channelCounts;
    int totalChannels = 0;
    const auto range = trackList.Any<Au3WaveTrack>();
    for (auto candidate : selectedOnly ? range + &Au3Track::IsSelected : range) {
        if (targetRate != RATE_NOT_SELECTED && candidate->GetRate() != targetRate) {
            continue;
        }

        // count channels in this track
        const int nChannels = static_cast<int>(candidate->NChannels());
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

WritableSampleTrackArray ChooseRecordingTracks(Au3Project& proj, const trackedit::TrackId focusedTrackId, const int recordingChannels)
{
    const PropertiesOfSelected selected = GetPropertiesOfSelected(proj);
    if (selected.anySelected) {
        if (!selected.allSameRate) {
            return {};
        }
        return ChooseExistingRecordingTracks(proj, true, selected.rateOfSelected, recordingChannels);
    }

    Au3WaveTrack* focusedTrack = DomAccessor::findWaveTrack(proj, Au3TrackId(focusedTrackId));
    if (!focusedTrack) {
        return {};
    }

    return { focusedTrack->SharedPointer<Au3WaveTrack>() };
}
}
