/**********************************************************************

Audacity: A Digital Audio Editor

MixAndRender.h
@brief Generate mono or stereo track mixing other given tracks

Paul Licameli split from Mix.h

**********************************************************************/

#ifndef __AUDACITY_MIX_AND_RENDER_H
#define __AUDACITY_MIX_AND_RENDER_H

#include "Mix.h"
#include "SampleFormat.h"
#include "Track.h"

class AudacityProject;
class WaveTrack;
class WaveTrackFactory;

#include <memory>

/** @brief Mixes together all input tracks, applying any envelopes, per-track
 * real-time effects, volume, panning and real-time effects in the process.
 *
 * Takes one or more tracks as input; of all the WaveTracs,
 * it mixes together all input tracks, applying any envelopes, per-track
 * real-time effects, volume, panning and real-time effects in the process. The
 * resulting pair of tracks (stereo) are "rendered" and have no effects, volume,
 * panning, or envelopes. Other sorts of tracks are ignored. If the start and
 * end times passed are the same this is taken as meaning no explicit time range
 * to process, and the whole occupied length of the input tracks is processed.
 *
 * Channel group properties of the result are copied from the first input track,
 * except that `newTrackName` is applied when more than one track is mixed.
 *
 * @param newTrackName used only when there is more than one input track (one
 * mono channel or a stereo pair); else the unique track's name is copied
 */
EFFECTS_API Track::Holder MixAndRender(
    const TrackIterRange<const WaveTrack>& trackRange, const Mixer::WarpOptions& warpOptions, const wxString& newTrackName,
    WaveTrackFactory* factory, double rate, sampleFormat format, double startTime, double endTime);

enum ChannelName : int;
using ChannelNames = const ChannelName*;

EFFECTS_API
std::vector<MixerOptions::StageSpecification>
GetEffectStages(const WaveTrack& track);

EFFECTS_API
std::vector<MixerOptions::StageSpecification>
GetMasterEffectStages(const AudacityProject& project);

#endif
