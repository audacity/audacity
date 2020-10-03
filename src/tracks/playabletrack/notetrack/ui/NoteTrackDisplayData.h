/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NoteTrackDisplayData.h
  @brief Calculations supporting Note track display

  Paul Licameli split from NoteTrack.h

**********************************************************************/

#ifndef __AUDACITY_NOTE_TRACK_DISPLAY_DATA
#define __AUDACITY_NOTE_TRACK_DISPLAY_DATA

#include <wx/gdicmn.h>

class NoteTrack;

/// Data used to display a note track
class NoteTrackDisplayData {
private:
   const NoteTrack &mTrack;
   wxRect mRect;

   float mPitchHeight;
   // mBottom is the Y offset of pitch 0 (normally off screen)
   // Used so that mBottomNote is located at
   // mY + mHeight - (GetNoteMargin() + 1 + GetPitchHeight())
   int mBottom;
   int mMargin;

   enum { MinPitchHeight = 1, MaxPitchHeight = 25 };
public:
   NoteTrackDisplayData(const NoteTrack &track, const wxRect &r);

   void Zoom(int y, float multiplier, bool center);
   void ZoomTo(int start, int end);
   /// Zooms out a constant factor (subject to zoom limits)
   void ZoomOut(int y)
   { Zoom(y, 1.0f / ZoomStep, true); }
   /// Zooms in a constant factor (subject to zoom limits)
   void ZoomIn(int y)
   { Zoom(y, ZoomStep, true); }

   int GetPitchHeight(int factor) const;
   int GetNoteMargin() const { return mMargin; };
   int GetOctaveHeight() const { return GetPitchHeight(12) + 2; }
   // IPitchToY returns Y coordinate of top of pitch p
   int IPitchToY(int p) const;
   // compute the window coordinate of the bottom of an octave: This is
   // the bottom of the line separating B and C.
   int GetOctaveBottom(int oct) const {
      return IPitchToY(oct * 12) + GetPitchHeight(1) + 1;
   }
   // Y coordinate for given floating point pitch (rounded to int)
   int PitchToY(double p) const {
      return IPitchToY((int) (p + 0.5));
   }
   // Integer pitch corresponding to a Y coordinate
   int YToIPitch(int y) const;
   // map pitch class number (0-11) to pixel offset from bottom of octave
   // (the bottom of the black line between B and C) to the top of the
   // note. Note extra pixel separates B(11)/C(0) and E(4)/F(5).
   int GetNotePos(int p) const
   { return 1 + GetPitchHeight(p + 1) + (p > 4); }
   // get pixel offset to top of ith black key note
   int GetBlackPos(int i) const { return GetNotePos(i * 2 + 1 + (i > 1)); }
   // GetWhitePos tells where to draw lines between keys as an offset from
   // GetOctaveBottom. GetWhitePos(0) returns 1, which matches the location
   // of the line separating B and C
   int GetWhitePos(int i) const { return 1 + (i * GetOctaveHeight()) / 7; }

   static const float ZoomStep;
};

#endif
