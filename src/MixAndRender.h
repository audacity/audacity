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

class WaveTrack;
class WaveTrackFactory;

#include <memory>

/** @brief Mixes together all input tracks, applying any envelopes, amplitude
 * gain, panning, and real-time effects in the process.
 *
 * Takes one or more tracks as input; of all the WaveTrack s,
 * it mixes them together, applying any envelopes, amplitude gain, panning, and
 * real-time effects in the process.  The resulting pair of tracks (stereo) are
 * "rendered" and have no effects, gain, panning, or envelopes. Other sorts of
 * tracks are ignored.
 * If the start and end times passed are the same this is taken as meaning
 * no explicit time range to process, and the whole occupied length of the
 * input tracks is processed.
 * Channel group properties of the result are copied from the first input track.
 *
 * @param newTrackName used only when there is more than one input track (one
 * mono channel or a stereo pair); else the unique track's name is copied
 */
void AUDACITY_DLL_API MixAndRender(
   const TrackIterRange<const WaveTrack> &trackRange,
   const Mixer::WarpOptions &warpOptions,
   const wxString &newTrackName,
   WaveTrackFactory *factory,
   double rate, sampleFormat format,
   double startTime, double endTime,
   std::shared_ptr<WaveTrack> &uLeft,
   std::shared_ptr<WaveTrack> &uRight);

#endif

