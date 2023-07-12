/**********************************************************************

  Audacity: A Digital Audio Editor

  PasteOverPreservingClips.h

  Mitch Golden
  Vaughan Johnson

  Paul Licameli split from Equalization.h

***********************************************************************/
#ifndef __AUDACITY_PASTE_OVER_PRESERVING_CLIPS__
#define __AUDACITY_PASTE_OVER_PRESERVING_CLIPS__

#include <vector>

class sampleCount;
class WaveTrack;
class wxString;

using Gap = std::pair<double, double>;
using Gaps = std::vector<Gap>;

struct ClipData {
   //Find the bits of clips that need replacing
   Gaps clipStartEndTimes;
   //may be truncated due to a clip being partially selected
   Gaps clipRealStartEndTimes;
   //Used to restore clip names after pasting
   std::vector<wxString> clipNames;
};


//! Collect clip boundary and name information
/*!
 @param start beginning position to paste over in oldTrack
 @param len length to paste over in oldTrack
 @pre `oldTrack.IsLeader()`
 */
WAVE_TRACK_API ClipData CollectClipData(
   const WaveTrack &oldTrack, sampleCount start, sampleCount len);

//! Substitute new contents into existing track, preserving clip boundaries
/*!
 @param start beginning position to paste over in oldTrack
 @param len length to paste over in oldTrack
 @param newContents begins at offset 0
 @pre `oldTrack.IsLeader()`
 @pre `newContents.IsLeader()`
 @pre `oldTrack.NChannels() == newContents.NChannels()`
 */
WAVE_TRACK_API void PasteOverPreservingClips(const ClipData &data,
   WaveTrack &oldTrack, sampleCount start, sampleCount len,
   const WaveTrack &newContents);

#endif
