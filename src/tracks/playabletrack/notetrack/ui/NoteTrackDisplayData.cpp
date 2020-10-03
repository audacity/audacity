/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NoteTrackDisplayData.cpp
  @brief Implements NoteTrackDisplayData

  Paul Licameli split from NoteTrack.cpp

**********************************************************************/

#include "NoteTrackDisplayData.h"
#include "NoteTrack.h"

NoteTrackDisplayData::NoteTrackDisplayData(
   const NoteTrack &track, const wxRect &rect)
   : mTrack{ track }
   , mRect{ rect }
{
   auto span = mTrack.GetTopNote() - mTrack.GetBottomNote() + 1; // + 1 to make sure it includes both

   mMargin = std::min((int) (rect.height / (float)(span)) / 2, rect.height / 4);

   // Count the number of dividers between B/C and E/F
   int numC = 0, numF = 0;
   auto botOctave = mTrack.GetBottomNote() / 12, botNote = mTrack.GetBottomNote() % 12;
   auto topOctave = mTrack.GetTopNote() / 12, topNote = mTrack.GetTopNote() % 12;
   if (topOctave == botOctave)
   {
      if (botNote == 0) numC = 1;
      if (topNote <= 5) numF = 1;
   }
   else
   {
      numC = topOctave - botOctave;
      numF = topOctave - botOctave - 1;
      if (botNote == 0) numC++;
      if (botNote <= 5) numF++;
      if (topOctave <= 5) numF++;
   }
   // Effective space, excluding the margins and the lines between some notes
   auto effectiveHeight = rect.height - (2 * (mMargin + 1)) - numC - numF;
   // Guaranteed that both the bottom and top notes will be visible
   // (assuming that the clamping below does not happen)
   mPitchHeight = effectiveHeight / ((float) span);

   if (mPitchHeight < MinPitchHeight)
      mPitchHeight = MinPitchHeight;
   if (mPitchHeight > MaxPitchHeight)
      mPitchHeight = MaxPitchHeight;

   mBottom = rect.y + rect.height - GetNoteMargin() - 1 - GetPitchHeight(1) +
            botOctave * GetOctaveHeight() + GetNotePos(botNote);
}

int NoteTrackDisplayData::IPitchToY(int p) const
{ return mBottom - (p / 12) * GetOctaveHeight() - GetNotePos(p % 12); }

int NoteTrackDisplayData::YToIPitch(int y) const
{
   y = mBottom - y; // pixels above pitch 0
   int octave = (y / GetOctaveHeight());
   y -= octave * GetOctaveHeight();
   // result is approximate because C and G are one pixel taller than
   // mPitchHeight.
   // Poke 1-13-18: However in practice this seems not to be an issue,
   // as long as we use mPitchHeight and not the rounded version
   return (y / mPitchHeight) + octave * 12;
}

void NoteTrackDisplayData::Zoom(int y, float multiplier, bool center)
{
   int clickedPitch = YToIPitch(y);
   int extent = mTrack.GetTopNote() - mTrack.GetBottomNote() + 1;
   int newExtent = (int) (extent / multiplier);
   float position;
   if (center) {
      // center the pitch that the user clicked on
      position = .5;
   } else {
      // align to keep the pitch that the user clicked on in the same place
      position = extent / (clickedPitch - mTrack.GetBottomNote());
   }
   int newBottomNote = clickedPitch - (newExtent * position);
   int newTopNote = clickedPitch + (newExtent * (1 - position));
   mTrack.SetNoteRange(newBottomNote, newTopNote);
}

void NoteTrackDisplayData::ZoomTo(int start, int end)
{
   wxRect trackRect(0, mRect.GetY(), 1, mRect.GetHeight());
   int pitch1 = YToIPitch(start);
   int pitch2 = YToIPitch(end);
   if (pitch1 == pitch2) {
      // Just zoom in instead of zooming to show only one note
      Zoom(start, 1, true);
      return;
   }
   // It's fine for this to be in either order
   mTrack.SetNoteRange(pitch1, pitch2);
}

int NoteTrackDisplayData::GetPitchHeight(int factor) const
{
   return std::max(1, (int)(factor * mPitchHeight));
   
}

const float NoteTrackDisplayData::ZoomStep = powf( 2.0f, 0.25f );
