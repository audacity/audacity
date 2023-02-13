/**********************************************************************

  Audacity: A Digital Audio Editor

  PasteOverPreservingClips.h

  Mitch Golden
  Vaughan Johnson

  Paul Licameli split from Equalization.h

***********************************************************************/
#ifndef __AUDACITY_PASTE_OVER_PRESERVING_CLIPS__
#define __AUDACITY_PASTE_OVER_PRESERVING_CLIPS__

class sampleCount;
class WaveTrack;

//! Substitute new contents into existing track, preserving clip boundaries
/*!
 @param start beginning position to paste over in oldTrack
 @param len length to paste over in oldTrack
 @param newContents begins at offset 0
 */
WAVE_TRACK_API void PasteOverPreservingClips(
   WaveTrack &oldTrack, sampleCount start, sampleCount len,
   WaveTrack &newContents);

#endif
