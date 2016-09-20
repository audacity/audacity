/**********************************************************************

  Audacity: A Digital Audio Editor

  TrackArtist.cpp

  Dominic Mazzoni


*******************************************************************//*!

\class TrackArtist
\brief   This class handles the actual rendering of WaveTracks (both
  waveforms and spectra), NoteTracks, LabelTracks and TimeTracks.

  It's actually a little harder than it looks, because for
  waveforms at least it needs to cache the samples that are
  currently on-screen.

<b>How Audacity Redisplay Works \n
 Roger Dannenberg</b> \n
Oct 2010 \n

This is a brief guide to Audacity redisplay -- it may not be complete. It
is my attempt to understand the complicated graphics strategy.

One basic idea is that redrawing waveforms is rather slow, so Audacity
saves waveform images in bitmaps to make redrawing faster. In particular,
during audio playback (and recording), the vertical time indicator is
drawn over the waveform about 20 times per second. To avoid unnecessary
computation, the indicator is erased by copying a column of pixels from
a bitmap image of the waveform. Notice that this implies a two-stage
process: first, waveforms are drawn to the bitmp; then, the bitmap
(or pieces of it) are copied to the screen, perhaps along with other
graphics.

The bitmap is for the entire track panel, i.e. multiple tracks, and
includes things like the Gain and Pan slders to the left of the
waveform images.

The screen update uses a mixture of direct drawing and indirect paint
events. The "normal" way to update a graphical display is to call
the Refresh() method when something invalidates the screen. Later, the
system calls OnPaint(), which the application overrides to (re)draw the
screen. In wxWidgets, you can also draw directly to the screen without
calling Refresh() and without waiting for OnPaint() to be called.

I would expect there to be a 2-level invalidation scheme: Some changes
invalidate the bitmap, forcing a bitmap redraw *and* a screen redraw.
Other changes merely update the screen using pre-existing bitmaps. In
Audacity, the "2-level" invalidation works like this: Anything
that invalidates the bitmap calls TrackPanel::Refresh(), which
has an eraseBackground parameter. This flag says to redraw the
bitmap when OnPaint() is called. If eraseBackground is false, the
existing bitmap can be used for waveform images. Audacity also
draws directly to the screen to update the time indicator during
playback. To move the indicator, one column of pixels is drawn to
the screen to remove the indicator. Then the indicator is drawn at
a NEW time location.

The track panel consists of many components. The tree of calls that
update the bitmap looks like this:

\code
TrackPanel::DrawTracks(), calls
       TrackArtist::DrawTracks();
       TrackPanel::DrawEverythingElse();
               for each track,
                       TrackPanel::DrawOutside();
                               TrackPanel::DrawOutsideOfTrack();
                               TrackPanel::DrawBordersAroundTrack();
                               TrackPanel::DrawShadow();
                               TrackInfo::DrawCloseBox();
                               TrackInfo::DrawTitleBar();
                               TrackInfo::DrawMinimize();
                               TrackInfo::DrawBordersWithin();
                               various TrackInfo sliders and buttons
                       TrackArtist::DrawVRuler();
               TrackPanel::DrawZooming();
                       draws horizontal dashed lines during zoom-drag
               TrackPanel::HighlightFocusedTrack();
                       draws yellow highlight on selected track
               draw snap guidelines if any
\endcode

After drawing the bitmap and blitting the bitmap to the screen,
the following calls are (sometimes) made. To keep track of what has
been drawn on screen over the bitmap images,
\li \c mLastCursor is the position of the vertical line representing sel0,
        the selected time position
\li \c mLastIndicator is the position of the moving vertical line during
        playback

\code
TrackPanel::DoDrawIndicator();
        copy pixel column from bitmap to screen to erase indicator line
        TrackPanel::DoDrawCursor(); [if mLastCursor == mLastIndicator]
        TrackPanel::DisplaySelection();
        AdornedRulerPanel::DrawIndicator(); [not part of TrackPanel graphics]
        draw indicator on each track
TrackPanel::DoDrawCursor();
        draw cursor on each track  [at selectedRegion.t0()]
        AdornedRulerPanel::DrawCursor(); [not part of TrackPanel graphics]
        TrackPanel::DisplaySelection();
\endcode

To move the indicator, TrackPanel::OnTimer() calls the following, using
a drawing context (DC) for the screen. (Refresh is not called to create
an OnPaint event. Instead, drawing is direct to the screen.)
\code
TrackPanel::DrawIndicator();
        TrackPanel::DoDrawIndicator();
\endcode

Notice that TrackPanel::DrawZooming(), TrackPanel::HighlightFocusedTrack(),
and snap guidelines could be drawn directly to the screen rather than to
the bitmap, generally eliminating redraw work.

One problem is slider udpates. Sliders are in the left area of the track
panel. They are not wxWindows like wxSliders, but instead are just drawn
on the TrackPanel. When slider state changes, *all* tracks do a full
refresh, including recomputing the backing store. It would make more sense
to just invalidate the region containing the slider. However, doing that
would require either incrementally updating the bitmap (not currently done),
or maintaining the sliders and other track info on the screen and not in
the bitmap.

In my opinion, the bitmap should contain only the waveform, note, and
label images along with gray selection highlights. The track info
(sliders, buttons, title, etc.), track selection highlight, cursor, and
indicator should be drawn in the normal way, and clipping regions should
be used to avoid excessive copying of bitmaps (say, when sliders move),
or excessive redrawing of track info widgets (say, when scrolling occurs).
This is a fairly tricky code change since it requires careful specification
of what and where redraw should take place when any state changes. One
surprising finding is that NoteTrack display is slow compared to WaveTrack
display. Each note takes some time to gather attributes and select colors,
and while audio draws two amplitudes per horizontal pixels, large MIDI
scores can have more notes than horizontal pixels. This can make slider
changes very sluggish, but this can also be a problem with many
audio tracks.

*//*******************************************************************/

#include "Audacity.h"
#include "TrackArtist.h"
#include "AudacityApp.h"
#include "float_cast.h"

#include <math.h>
#include <float.h>
#include <limits>

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include <wx/brush.h>
#include <wx/colour.h>
#include <wx/dc.h>
#include <wx/dcmemory.h>
#include <wx/gdicmn.h>
#include <wx/graphics.h>
#include <wx/image.h>
#include <wx/pen.h>
#include <wx/log.h>
#include <wx/datetime.h>

#ifdef USE_MIDI
#include "NoteTrack.h"
#endif // USE_MIDI

#include "AColor.h"
#include "BlockFile.h"
#include "Envelope.h"
#include "NumberScale.h"
#include "WaveTrack.h"
#include "LabelTrack.h"
#include "TimeTrack.h"
#include "Prefs.h"
#include "prefs/GUISettings.h"
#include "prefs/SpectrogramSettings.h"
#include "prefs/WaveformSettings.h"
#include "Spectrum.h"
#include "ViewInfo.h"
#include "widgets/Ruler.h"
#include "Theme.h"
#include "AllThemeResources.h"
#include "Experimental.h"

#undef PROFILE_WAVEFORM
#ifdef PROFILE_WAVEFORM
   #ifdef __WXMSW__
      #include <time.h>
   #else
      #include <sys/time.h>
   #endif
double gWaveformTimeTotal = 0;
int gWaveformTimeCount = 0;

namespace {
   struct Profiler {
      Profiler()
      {
#   ifdef __WXMSW__
         _time64(&tv0);
#   else
         gettimeofday(&tv0, NULL);
#   endif
      }

      ~Profiler()
      {
#   ifdef __WXMSW__
         _time64(&tv1);
         double elapsed = _difftime64(tv1, tv0);
#   else
         gettimeofday(&tv1, NULL);
         double elapsed =
            (tv1.tv_sec + tv1.tv_usec*0.000001) -
            (tv0.tv_sec + tv0.tv_usec*0.000001);
#   endif
         gWaveformTimeTotal += elapsed;
         gWaveformTimeCount++;
         wxPrintf(wxT("Avg waveform drawing time: %f\n"),
            gWaveformTimeTotal / gWaveformTimeCount);
      }

#   ifdef __WXMSW__
      __time64_t tv0, tv1;
#else
      struct timeval tv0, tv1;
#endif
   };
}
#endif

#ifdef USE_MIDI
/*
const int octaveHeight = 62;
const int blackPos[5] = { 6, 16, 32, 42, 52 };
const int whitePos[7] = { 0, 9, 17, 26, 35, 44, 53 };
const int notePos[12] = { 1, 6, 11, 16, 21, 27,
                        32, 37, 42, 47, 52, 57 };

// map pitch number to window coordinate of the *top* of the note
// Note the "free" variable bottom, which is assumed to be a local
// variable set to the offset of pitch 0 relative to the window
#define IPITCH_TO_Y(t, p) (bottom - ((p) / 12) * octaveHeight - \
                          notePos[(p) % 12] - (t)->GetPitchHeight())

// GetBottom is called from a couple of places to compute the hypothetical
// coordinate of the bottom of pitch 0 in window coordinates. See
// IPITCH_TO_Y above, which computes coordinates relative to GetBottom()
// Note the -NOTE_MARGIN, which leaves a little margin to draw notes that
// are out of bounds. I'm not sure why the -2 is necessary.
int TrackArtist::GetBottom(NoteTrack *t, const wxRect &rect)
{
   int bottomNote = t->GetBottomNote();
   int bottom = rect.y + rect.height - 2 - t->GetNoteMargin() +
          ((bottomNote / 12) * octaveHeight + notePos[bottomNote % 12]);
   return bottom;

}
*/
#endif // USE_MIDI

TrackArtist::TrackArtist()
{
   mInsetLeft   = 0;
   mInsetTop    = 0;
   mInsetRight  = 0;
   mInsetBottom = 0;

   mdBrange = ENV_DB_RANGE;
   mShowClipping = false;
   UpdatePrefs();

   SetColours();
   vruler = std::make_unique<Ruler>();
}

TrackArtist::~TrackArtist()
{
}

void TrackArtist::SetColours()
{
   theTheme.SetBrushColour( blankBrush,      clrBlank );
   theTheme.SetBrushColour( unselectedBrush, clrUnselected);
   theTheme.SetBrushColour( selectedBrush,   clrSelected);
   theTheme.SetBrushColour( sampleBrush,     clrSample);
   theTheme.SetBrushColour( selsampleBrush,  clrSelSample);
   theTheme.SetBrushColour( dragsampleBrush, clrDragSample);
   theTheme.SetBrushColour( blankSelectedBrush, clrBlankSelected);

   theTheme.SetPenColour(   blankPen,        clrBlank);
   theTheme.SetPenColour(   unselectedPen,   clrUnselected);
   theTheme.SetPenColour(   selectedPen,     clrSelected);
   theTheme.SetPenColour(   samplePen,       clrSample);
   theTheme.SetPenColour(   selsamplePen,    clrSelSample);
   theTheme.SetPenColour(   muteSamplePen,   clrMuteSample);
   theTheme.SetPenColour(   odProgressDonePen, clrProgressDone);
   theTheme.SetPenColour(   odProgressNotYetPen, clrProgressNotYet);
   theTheme.SetPenColour(   rmsPen,          clrRms);
   theTheme.SetPenColour(   muteRmsPen,      clrMuteRms);
   theTheme.SetPenColour(   shadowPen,       clrShadow);
   theTheme.SetPenColour(   clippedPen,      clrClipped);
   theTheme.SetPenColour(   muteClippedPen,  clrMuteClipped);
   theTheme.SetPenColour(   blankSelectedPen,clrBlankSelected);
}

void TrackArtist::SetInset(int left, int top, int right, int bottom)
{
   mInsetLeft   = left;
   mInsetTop    = top;
   mInsetRight  = right;
   mInsetBottom = bottom;
}

void TrackArtist::DrawTracks(TrackList * tracks,
                             Track * start,
                             wxDC & dc,
                             wxRegion & reg,
                             wxRect & rect,
                             wxRect & clip,
                             const SelectedRegion &selectedRegion,
                             const ZoomInfo &zoomInfo,
                             bool drawEnvelope,
                             bool bigPoints,
                             bool drawSliders)
{
   wxRect trackRect = rect;
   wxRect stereoTrackRect;
   TrackListIterator iter(tracks);
   Track *t;

   bool hasSolo = false;
   for (t = iter.First(); t; t = iter.Next()) {
      if (t->GetSolo()) {
         hasSolo = true;
         break;
      }
   }

#if defined(DEBUG_CLIENT_AREA)
   // Change the +0 to +1 or +2 to see the bounding box
   mInsetLeft = 1+0; mInsetTop = 5+0; mInsetRight = 6+0; mInsetBottom = 2+0;

   // This just shows what the passed in rectangles enclose
   dc.SetPen(wxColour(*wxGREEN));
   dc.SetBrush(*wxTRANSPARENT_BRUSH);
   dc.DrawRectangle(rect);
   dc.SetPen(wxColour(*wxBLUE));
   dc.SetBrush(*wxTRANSPARENT_BRUSH);
   dc.DrawRectangle(clip);
#endif

   gPrefs->Read(wxT("/GUI/ShowTrackNameInWaveform"), &mbShowTrackNameInWaveform, false);

   t = iter.StartWith(start);
   while (t) {
      trackRect.y = t->GetY() - zoomInfo.vpos;
      trackRect.height = t->GetHeight();

      if (trackRect.y > clip.GetBottom() && !t->GetLinked()) {
         break;
      }

#if defined(DEBUG_CLIENT_AREA)
      // Filled rectangle to show the interior of the client area
      wxRect zr = trackRect;
      zr.x+=1; zr.y+=5; zr.width-=7; zr.height-=7;
      dc.SetPen(*wxCYAN_PEN);
      dc.SetBrush(*wxRED_BRUSH);
      dc.DrawRectangle(zr);
#endif

      stereoTrackRect = trackRect;

      // For various reasons, the code will break if we display one
      // of a stereo pair of tracks but not the other - for example,
      // if you try to edit the envelope of one track when its linked
      // pair is off the screen, then it won't be able to edit the
      // offscreen envelope.  So we compute the rect of the track and
      // its linked partner, and see if any part of that rect is on-screen.
      // If so, we draw both.  Otherwise, we can safely draw neither.

      Track *link = t->GetLink();
      if (link) {
         if (t->GetLinked()) {
            // If we're the first track
            stereoTrackRect.height += link->GetHeight();
         }
         else {
            // We're the second of two
            stereoTrackRect.y -= link->GetHeight();
            stereoTrackRect.height += link->GetHeight();
         }
      }
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      if(MONO_WAVE_PAN(t)){
         stereoTrackRect.height += t->GetHeight(true);
         t->SetVirtualStereo(false);
      }
#endif

      if (stereoTrackRect.Intersects(clip) && reg.Contains(stereoTrackRect)) {
         wxRect rr = trackRect;
         rr.x += mInsetLeft;
         rr.y += mInsetTop;
         rr.width -= (mInsetLeft + mInsetRight);
         rr.height -= (mInsetTop + mInsetBottom);
         DrawTrack(t, dc, rr,
                   selectedRegion, zoomInfo,
                   drawEnvelope, bigPoints, drawSliders, hasSolo);
      }

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      if(MONO_WAVE_PAN(t)){
         trackRect.y = t->GetY(true) - zoomInfo.vpos;
         trackRect.height = t->GetHeight(true);
         stereoTrackRect = trackRect;
         stereoTrackRect.y -= t->GetHeight();
         stereoTrackRect.height += t->GetHeight();
         t->SetVirtualStereo(true);
         if (stereoTrackRect.Intersects(clip) && reg.Contains(stereoTrackRect)) {
            wxRect rr = trackRect;
            rr.x += mInsetLeft;
            rr.y += mInsetTop;
            rr.width -= (mInsetLeft + mInsetRight);
            rr.height -= (mInsetTop + mInsetBottom);
            DrawTrack(t, dc, rr, zoomInfo,
                      drawEnvelope, bigPoints, drawSliders, hasSolo);
         }
      }
#endif

      t = iter.Next();
   }
}

void TrackArtist::DrawTrack(const Track * t,
                            wxDC & dc,
                            const wxRect & rect,
                            const SelectedRegion &selectedRegion,
                            const ZoomInfo &zoomInfo,
                            bool drawEnvelope,
                            bool bigPoints,
                            bool drawSliders,
                            bool hasSolo)
{
   switch (t->GetKind()) {
   case Track::Wave:
   {
      const WaveTrack* wt = static_cast<const WaveTrack*>(t);
      for (const auto &clip : wt->GetClips()) {
         clip->ClearDisplayRect();
      }

      bool muted = (hasSolo || t->GetMute()) && !t->GetSolo();

#if defined(__WXMAC__)
      wxAntialiasMode aamode = dc.GetGraphicsContext()->GetAntialiasMode();
      dc.GetGraphicsContext()->SetAntialiasMode(wxANTIALIAS_NONE);
#endif

      switch (wt->GetDisplay()) {
      case WaveTrack::Waveform:
         DrawWaveform(wt, dc, rect, selectedRegion, zoomInfo,
                      drawEnvelope,  bigPoints, drawSliders, muted);
         break;
      case WaveTrack::Spectrum:
         DrawSpectrum(wt, dc, rect, selectedRegion, zoomInfo);
         break;
      default:
         wxASSERT(false);
      }

#if defined(__WXMAC__)
      dc.GetGraphicsContext()->SetAntialiasMode(aamode);
#endif

      if (mbShowTrackNameInWaveform &&
          // Exclude right channel of stereo track 
          !(!wt->GetLinked() && wt->GetLink())) {
         wxFont labelFont(12, wxSWISS, wxNORMAL, wxNORMAL);
         dc.SetFont(labelFont);
         dc.SetTextForeground(wxColour(255, 255, 0));
         dc.DrawText (wt->GetName(), rect.x+10, rect.y);  // move right 10 pixels to avoid overwriting <- symbol
      }
      break;              // case Wave
   }
   #ifdef USE_MIDI
   case Track::Note:
   {
      bool muted = (hasSolo || t->GetMute()) && !t->GetSolo();
      DrawNoteTrack((NoteTrack *)t, dc, rect, selectedRegion, zoomInfo, muted);
      break;
   }
   #endif // USE_MIDI
   case Track::Label:
      DrawLabelTrack((LabelTrack *)t, dc, rect, selectedRegion, zoomInfo);
      break;
   case Track::Time:
      DrawTimeTrack((TimeTrack *)t, dc, rect, zoomInfo);
      break;
   }
}

void TrackArtist::DrawVRuler(const Track *t, wxDC * dc, wxRect & rect)
{
   int kind = t->GetKind();

   // Label and Time tracks do not have a vruler
   // But give it a beveled area
   if (kind == Track::Label) {
      wxRect bev = rect;
      bev.Inflate(-1, 0);
      bev.width += 1;
      AColor::BevelTrackInfo(*dc, true, bev);

      return;
   }

   // Time tracks
   if (kind == Track::Time) {
      wxRect bev = rect;
      bev.Inflate(-1, 0);
      bev.width += 1;
      AColor::BevelTrackInfo(*dc, true, bev);

      // Right align the ruler
      wxRect rr = rect;
      rr.width--;
      if (t->vrulerSize.GetWidth() < rect.GetWidth()) {
         int adj = rr.GetWidth() - t->vrulerSize.GetWidth();
         rr.x += adj;
         rr.width -= adj;
      }

      UpdateVRuler(t, rr);

      vruler->Draw(*dc);

      return;
   }

   // All waves have a ruler in the info panel
   // The ruler needs a bevelled surround.
   if (kind == Track::Wave) {
      wxRect bev = rect;
      bev.Inflate(-1, 0);
      bev.width += 1;
      AColor::BevelTrackInfo(*dc, true, bev);

      // Right align the ruler
      wxRect rr = rect;
      rr.width--;
      if (t->vrulerSize.GetWidth() < rect.GetWidth()) {
         int adj = rr.GetWidth() - t->vrulerSize.GetWidth();
         rr.x += adj;
         rr.width -= adj;
      }

      UpdateVRuler(t, rr);

      vruler->Draw(*dc);

      return;
   }

#ifdef USE_MIDI
   // The note track draws a vertical keyboard to label pitches
   if (kind == Track::Note) {
      UpdateVRuler(t, rect);

      dc->SetPen(*wxTRANSPARENT_PEN);
      dc->SetBrush(*wxWHITE_BRUSH);
      wxRect bev = rect;
      bev.x++;
      bev.width--;
      dc->DrawRectangle(bev);

      rect.y += 1;
      rect.height -= 1;

      //int bottom = GetBottom((NoteTrack *) t, rect);
      const NoteTrack *track = (NoteTrack *) t;
      track->PrepareIPitchToY(rect);

      wxPen hilitePen;
      hilitePen.SetColour(120, 120, 120);
      wxBrush blackKeyBrush;
      blackKeyBrush.SetColour(70, 70, 70);

      dc->SetBrush(blackKeyBrush);

      int fontSize = 10;
#ifdef __WXMSW__
      fontSize = 8;
#endif

      wxFont labelFont(fontSize, wxSWISS, wxNORMAL, wxNORMAL);
      dc->SetFont(labelFont);

      int octave = 0;
      int obottom = track->GetOctaveBottom(octave);
      int marg = track->GetNoteMargin();
      //IPITCH_TO_Y(octave * 12) + PITCH_HEIGHT + 1;
      while (obottom >= rect.y) {
         dc->SetPen(*wxBLACK_PEN);
         for (int white = 0; white < 7; white++) {
            int pos = track->GetWhitePos(white);
            if (obottom - pos > rect.y + marg + 1 &&
                // don't draw too close to margin line -- it's annoying
                obottom - pos < rect.y + rect.height - marg - 3)
               AColor::Line(*dc, rect.x, obottom - pos,
                            rect.x + rect.width, obottom - pos);
         }
         wxRect br = rect;
         br.height = track->GetPitchHeight();
         br.x++;
         br.width = 17;
         for (int black = 0; black < 5; black++) {
            br.y = obottom - track->GetBlackPos(black);
            if (br.y > rect.y + marg - 2 && br.y + br.height < rect.y + rect.height - marg) {
               dc->SetPen(hilitePen);
               dc->DrawRectangle(br);
               dc->SetPen(*wxBLACK_PEN);
               AColor::Line(*dc,
                            br.x + 1, br.y + br.height - 1,
                            br.x + br.width - 1, br.y + br.height - 1);
               AColor::Line(*dc,
                            br.x + br.width - 1, br.y + 1,
                            br.x + br.width - 1, br.y + br.height - 1);
            }
         }

         if (octave >= 1 && octave <= 10) {
            wxString s;
            // ISO standard: A440 is in the 4th octave, denoted
            // A4 <- the "4" should be a subscript.
            s.Printf(wxT("C%d"), octave - 1);
            wxCoord width, height;
            dc->GetTextExtent(s, &width, &height);
            if (obottom - height + 4 > rect.y &&
                obottom + 4 < rect.y + rect.height) {
               dc->SetTextForeground(wxColour(60, 60, 255));
               dc->DrawText(s, rect.x + rect.width - width,
                            obottom - height + 2);
            }
         }
         obottom = track->GetOctaveBottom(++octave);
      }
      // draw lines delineating the out-of-bounds margins
      dc->SetPen(*wxBLACK_PEN);
      // you would think the -1 offset here should be -2 to match the
      // adjustment to rect.y (see above), but -1 produces correct output
      AColor::Line(*dc, rect.x, rect.y + marg - 1, rect.x + rect.width, rect.y + marg - 1);
      // since the margin gives us the bottom of the line,
      // the extra -1 gets us to the top
      AColor::Line(*dc, rect.x, rect.y + rect.height - marg - 1,
                        rect.x + rect.width, rect.y + rect.height - marg - 1);

   }
#endif // USE_MIDI

}

void TrackArtist::UpdateVRuler(const Track *t, wxRect & rect)
{
   // Label tracks do not have a vruler
   if (t->GetKind() == Track::Label) {
      return;
   }

   // Time tracks
   if (t->GetKind() == Track::Time) {
      const TimeTrack *tt = (TimeTrack *)t;
      float min, max;
      min = tt->GetRangeLower() * 100.0;
      max = tt->GetRangeUpper() * 100.0;

      vruler->SetBounds(rect.x, rect.y, rect.x + rect.width, rect.y + rect.height-1);
      vruler->SetOrientation(wxVERTICAL);
      vruler->SetRange(max, min);
      vruler->SetFormat((tt->GetDisplayLog()) ? Ruler::RealLogFormat : Ruler::RealFormat);
      vruler->SetUnits(wxT(""));
      vruler->SetLabelEdges(false);
      vruler->SetLog(tt->GetDisplayLog());
   }

   // All waves have a ruler in the info panel
   // The ruler needs a bevelled surround.
   if (t->GetKind() == Track::Wave) {
      const WaveTrack *wt = static_cast<const WaveTrack*>(t);
      const float dBRange =
         wt->GetWaveformSettings().dBRange;

      const int display = wt->GetDisplay();

      if (display == WaveTrack::Waveform) {
         WaveformSettings::ScaleType scaleType =
            wt->GetWaveformSettings().scaleType;

         if (scaleType == WaveformSettings::stLinear) {
            // Waveform

            float min, max;
            wt->GetDisplayBounds(&min, &max);
            if (wt->GetLastScaleType() != scaleType &&
                wt->GetLastScaleType() != -1)
            {
               // do a translation into the linear space
               wt->SetLastScaleType();
               wt->SetLastdBRange();
               float sign = (min >= 0 ? 1 : -1);
               if (min != 0.) {
                  min = DB_TO_LINEAR(fabs(min) * dBRange - dBRange);
                  if (min < 0.0)
                     min = 0.0;
                  min *= sign;
               }
               sign = (max >= 0 ? 1 : -1);

               if (max != 0.) {
                  max = DB_TO_LINEAR(fabs(max) * dBRange - dBRange);
                  if (max < 0.0)
                     max = 0.0;
                  max *= sign;
               }
               wt->SetDisplayBounds(min, max);
            }

            vruler->SetBounds(rect.x, rect.y, rect.x + rect.width, rect.y + rect.height - 1);
            vruler->SetOrientation(wxVERTICAL);
            vruler->SetRange(max, min);
            vruler->SetFormat(Ruler::RealFormat);
            vruler->SetUnits(wxT(""));
            vruler->SetLabelEdges(false);
            vruler->SetLog(false);
         }
         else {
            wxASSERT(scaleType == WaveformSettings::stLogarithmic);
            scaleType = WaveformSettings::stLogarithmic;

            vruler->SetUnits(wxT(""));

            float min, max;
            wt->GetDisplayBounds(&min, &max);
            float lastdBRange;

            if (wt->GetLastScaleType() != scaleType &&
                wt->GetLastScaleType() != -1)
            {
               // do a translation into the dB space
               wt->SetLastScaleType();
               wt->SetLastdBRange();
               float sign = (min >= 0 ? 1 : -1);
               if (min != 0.) {
                  min = (LINEAR_TO_DB(fabs(min)) + dBRange) / dBRange;
                  if (min < 0.0)
                     min = 0.0;
                  min *= sign;
               }
               sign = (max >= 0 ? 1 : -1);

               if (max != 0.) {
                  max = (LINEAR_TO_DB(fabs(max)) + dBRange) / dBRange;
                  if (max < 0.0)
                     max = 0.0;
                  max *= sign;
               }
               wt->SetDisplayBounds(min, max);
            }
            else if (dBRange != (lastdBRange = wt->GetLastdBRange())) {
               wt->SetLastdBRange();
               // Remap the max of the scale
               const float sign = (max >= 0 ? 1 : -1);
               float newMax = max;
               if (max != 0.) {

// Ugh, duplicating from TrackPanel.cpp
#define ZOOMLIMIT 0.001f

                  const float extreme = LINEAR_TO_DB(2);
                  // recover dB value of max
                  const float dB = std::min(extreme, (float(fabs(max)) * lastdBRange - lastdBRange));
                  // find NEW scale position, but old max may get trimmed if the db limit rises
                  // Don't trim it to zero though, but leave max and limit distinct
                  newMax = sign * std::max(ZOOMLIMIT, (dBRange + dB) / dBRange);
                  // Adjust the min of the scale if we can,
                  // so the db Limit remains where it was on screen, but don't violate extremes
                  if (min != 0.)
                     min = std::max(-extreme, newMax * min / max);
               }

               wt->SetDisplayBounds(min, newMax);
            }

            if (max > 0) {
               int top = 0;
               float topval = 0;
               int bot = rect.height;
               float botval = -dBRange;

               if (min < 0) {
                  bot = top + (int)((max / (max - min))*(bot - top));
                  min = 0;
               }

               if (max > 1) {
                  top += (int)((max - 1) / (max - min) * (bot - top));
                  max = 1;
               }

               if (max < 1 && max > 0)
                  topval = -((1 - max) * dBRange);

               if (min > 0) {
                  botval = -((1 - min) * dBRange);
               }

               vruler->SetBounds(rect.x, rect.y + top, rect.x + rect.width, rect.y + bot - 1);
               vruler->SetOrientation(wxVERTICAL);
               vruler->SetRange(topval, botval);
            }
            else
               vruler->SetBounds(0.0, 0.0, 0.0, 0.0); // A.C.H I couldn't find a way to just disable it?
            vruler->SetFormat(Ruler::RealLogFormat);
            vruler->SetLabelEdges(true);
            vruler->SetLog(false);
         }
      }
      else {
         wxASSERT(display == WaveTrack::Spectrum);
         const SpectrogramSettings &settings = wt->GetSpectrogramSettings();
         float minFreq, maxFreq;
         wt->GetSpectrumBounds(&minFreq, &maxFreq);

         switch (settings.scaleType) {
         default:
            wxASSERT(false);
         case SpectrogramSettings::stLinear:
         {
            // Spectrum

            if (rect.height < 60)
               return;

            /*
            draw the ruler
            we will use Hz if maxFreq is < 2000, otherwise we represent kHz,
            and append to the numbers a "k"
            */
            vruler->SetBounds(rect.x, rect.y, rect.x + rect.width, rect.y + rect.height - 1);
            vruler->SetOrientation(wxVERTICAL);
            vruler->SetFormat(Ruler::RealFormat);
            vruler->SetLabelEdges(true);
            // use kHz in scale, if appropriate
            if (maxFreq >= 2000) {
               vruler->SetRange((maxFreq / 1000.), (minFreq / 1000.));
               vruler->SetUnits(wxT("k"));
            }
            else {
               // use Hz
               vruler->SetRange((int)(maxFreq), (int)(minFreq));
               vruler->SetUnits(wxT(""));
            }
            vruler->SetLog(false);
         }
         break;
         case SpectrogramSettings::stLogarithmic:
         case SpectrogramSettings::stMel:
         case SpectrogramSettings::stBark:
         case SpectrogramSettings::stErb:
         case SpectrogramSettings::stPeriod:
         {
            // SpectrumLog

            if (rect.height < 10)
               return;

            /*
            draw the ruler
            we will use Hz if maxFreq is < 2000, otherwise we represent kHz,
            and append to the numbers a "k"
            */
            vruler->SetBounds(rect.x, rect.y, rect.x + rect.width, rect.y + rect.height - 1);
            vruler->SetOrientation(wxVERTICAL);
            vruler->SetFormat(Ruler::IntFormat);
            vruler->SetLabelEdges(true);
            vruler->SetRange(maxFreq, minFreq);
            vruler->SetUnits(wxT(""));
            vruler->SetLog(true);
            NumberScale scale
               (wt->GetSpectrogramSettings().GetScale
                  (minFreq, maxFreq, wt->GetRate(), false).Reversal());
            vruler->SetNumberScale(&scale);
         }
         break;
         }
      }
   }

#ifdef USE_MIDI
   // The note track isn't drawing a ruler at all!
   // But it needs to!
   else if (t->GetKind() == Track::Note) {
      vruler->SetBounds(rect.x, rect.y, rect.x + 1, rect.y + rect.height-1);
      vruler->SetOrientation(wxVERTICAL);
   }
#endif // USE_MIDI

   vruler->GetMaxSize(&t->vrulerSize.x, &t->vrulerSize.y);
}

/// Takes a value between min and max and returns a value between
/// height and 0
/// \todo  Should this function move int GuiWaveTrack where it can
/// then use the zoomMin, zoomMax and height values without having
/// to have them passed in to it??
int GetWaveYPos(float value, float min, float max,
                int height, bool dB, bool outer,
                float dBr, bool clip)
{
   if (dB) {
      if (height == 0) {
         return 0;
      }

      float sign = (value >= 0 ? 1 : -1);

      if (value != 0.) {
         float db = LINEAR_TO_DB(fabs(value));
         value = (db + dBr) / dBr;
         if (!outer) {
            value -= 0.5;
         }
         if (value < 0.0) {
            value = 0.0;
         }
         value *= sign;
      }
   }
   else {
      if (!outer) {
         if (value >= 0.0) {
            value -= 0.5;
         }
         else {
            value += 0.5;
         }
      }
   }

   if (clip) {
      if (value < min) {
         value = min;
      }
      if (value > max) {
         value = max;
      }
   }

   value = (max - value) / (max - min);
   return (int) (value * (height - 1) + 0.5);
}

float FromDB(float value, double dBRange)
{
   if (value == 0)
      return 0;

   double sign = (value >= 0 ? 1 : -1);
   return DB_TO_LINEAR((fabs(value) * dBRange) - dBRange) * sign;
}

float ValueOfPixel(int yy, int height, bool offset,
   bool dB, double dBRange, float zoomMin, float zoomMax)
{
   wxASSERT(height > 0);
   // Map 0 to max and height - 1 (not height) to min
   float v =
      height == 1 ? (zoomMin + zoomMax) / 2 :
      zoomMax - (yy / (float)(height - 1)) * (zoomMax - zoomMin);
   if (offset) {
      if (v > 0.0)
         v += .5;
      else
         v -= .5;
   }

   if (dB)
      v = FromDB(v, dBRange);

   return v;
}

void TrackArtist::DrawNegativeOffsetTrackArrows(wxDC &dc, const wxRect &rect)
{
   // Draws two black arrows on the left side of the track to
   // indicate the user that the track has been time-shifted
   // to the left beyond t=0.0.

   dc.SetPen(*wxBLACK_PEN);
   AColor::Line(dc,
                rect.x + 2, rect.y + 6,
                rect.x + 8, rect.y + 6);
   AColor::Line(dc,
                rect.x + 2, rect.y + 6,
                rect.x + 6, rect.y + 2);
   AColor::Line(dc,
                rect.x + 2, rect.y + 6,
                rect.x + 6, rect.y + 10);
   AColor::Line(dc,
                rect.x + 2, rect.y + rect.height - 8,
                rect.x + 8, rect.y + rect.height - 8);
   AColor::Line(dc,
                rect.x + 2, rect.y + rect.height - 8,
                rect.x + 6, rect.y + rect.height - 4);
   AColor::Line(dc,
                rect.x + 2, rect.y + rect.height - 8,
                rect.x + 6, rect.y + rect.height - 12);
}

void TrackArtist::DrawWaveformBackground(wxDC &dc, int leftOffset, const wxRect &rect,
                                         const double env[],
                                         float zoomMin, float zoomMax,
                                         bool dB, float dBRange,
                                         double t0, double t1,
                                         const ZoomInfo &zoomInfo,
                                         bool drawEnvelope, bool bIsSyncLockSelected)
{

   // Visually (one vertical slice of the waveform background, on its side;
   // the "*" is the actual waveform background we're drawing
   //
   //1.0                              0.0                             -1.0
   // |--------------------------------|--------------------------------|
   //      ***************                           ***************
   //      |             |                           |             |
   //    maxtop        maxbot                      mintop        minbot

   int h = rect.height;
   int halfHeight = wxMax(h / 2, 1);
   int maxtop, lmaxtop = 0;
   int mintop, lmintop = 0;
   int maxbot, lmaxbot = 0;
   int minbot, lminbot = 0;
   bool sel, lsel = false;
   int xx, lx = 0;
   int l, w;

   dc.SetPen(*wxTRANSPARENT_PEN);
   dc.SetBrush(blankBrush);
   dc.DrawRectangle(rect);

   double time = zoomInfo.PositionToTime(0, -leftOffset), nextTime;
   for (xx = 0; xx < rect.width; ++xx, time = nextTime) {
      nextTime = zoomInfo.PositionToTime(xx + 1, -leftOffset);
      // First we compute the truncated shape of the waveform background.
      // If drawEnvelope is true, then we compute the lower border of the
      // envelope.

      maxtop = GetWaveYPos(env[xx], zoomMin, zoomMax,
                               h, dB, true, dBRange, true);
      maxbot = GetWaveYPos(env[xx], zoomMin, zoomMax,
                               h, dB, false, dBRange, true);

      mintop = GetWaveYPos(-env[xx], zoomMin, zoomMax,
                               h, dB, false, dBRange, true);
      minbot = GetWaveYPos(-env[xx], zoomMin, zoomMax,
                               h, dB, true, dBRange, true);

      // Make sure it's odd so that a that max and min mirror each other
      mintop +=1;
      minbot +=1;

      if (!drawEnvelope || maxbot > mintop) {
         maxbot = halfHeight;
         mintop = halfHeight;
      }

      // We don't draw selection color for sync-lock selected tracks.
      sel = (t0 <= time && nextTime < t1) && !bIsSyncLockSelected;

      if (lmaxtop == maxtop &&
          lmintop == mintop &&
          lmaxbot == maxbot &&
          lminbot == minbot &&
          lsel == sel) {
         continue;
      }

      dc.SetBrush(lsel ? selectedBrush : unselectedBrush);

      l = rect.x + lx;
      w = xx - lx;
      if (lmaxbot < lmintop - 1) {
         dc.DrawRectangle(l, rect.y + lmaxtop, w, lmaxbot - lmaxtop);
         dc.DrawRectangle(l, rect.y + lmintop, w, lminbot - lmintop);
      }
      else {
         dc.DrawRectangle(l, rect.y + lmaxtop, w, lminbot - lmaxtop);
      }

      lmaxtop = maxtop;
      lmintop = mintop;
      lmaxbot = maxbot;
      lminbot = minbot;
      lsel = sel;
      lx = xx;
   }

   dc.SetBrush(lsel ? selectedBrush : unselectedBrush);
   l = rect.x + lx;
   w = xx - lx;
   if (lmaxbot < lmintop - 1) {
      dc.DrawRectangle(l, rect.y + lmaxtop, w, lmaxbot - lmaxtop);
      dc.DrawRectangle(l, rect.y + lmintop, w, lminbot - lmintop);
   }
   else {
      dc.DrawRectangle(l, rect.y + lmaxtop, w, lminbot - lmaxtop);
   }

   // If sync-lock selected, draw in linked graphics.
   if (bIsSyncLockSelected && t0 < t1) {
      const int begin = std::max(0, std::min(rect.width, (int)(zoomInfo.TimeToPosition(t0, -leftOffset))));
      const int end = std::max(0, std::min(rect.width, (int)(zoomInfo.TimeToPosition(t1, -leftOffset))));
      DrawSyncLockTiles(&dc, wxRect(rect.x + begin, rect.y, end - 1 - begin, rect.height));
   }

   //OK, the display bounds are between min and max, which
   //is spread across rect.height.  Draw the line at the proper place.

   if (zoomMin < 0 && zoomMax > 0) {
      int half = (int)((zoomMax / (zoomMax - zoomMin)) * h);
      dc.SetPen(*wxBLACK_PEN);
      AColor::Line(dc, rect.x, rect.y + half, rect.x + rect.width, rect.y + half);
   }
}


void TrackArtist::DrawMinMaxRMS(wxDC &dc, const wxRect & rect, const double env[],
   float zoomMin, float zoomMax,
   bool dB, float dBRange,
   const float *min, const float *max, const float *rms, const int *bl,
   bool /* showProgress */, bool muted
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
   , const float gain
#endif
)
{
   // Display a line representing the
   // min and max of the samples in this region
   int lasth1 = std::numeric_limits<int>::max();
   int lasth2 = std::numeric_limits<int>::min();
   int h1;
   int h2;
   int *r1 = new int[rect.width];
   int *r2 = new int[rect.width];
   int *clipped = NULL;
   int clipcnt = 0;

   if (mShowClipping) {
      clipped = new int[rect.width];
   }

   long pixAnimOffset = (long)fabs((double)(wxDateTime::Now().GetTicks() * -10)) +
      wxDateTime::Now().GetMillisecond() / 100; //10 pixels a second

   bool drawStripes = true;
   bool drawWaveform = true;

   dc.SetPen(muted ? muteSamplePen : samplePen);
   for (int x0 = 0; x0 < rect.width; ++x0) {
      int xx = rect.x + x0;
      double v;
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
     //JWA: "gain" variable passed to function includes the pan value and is used below 4/14/13
      v = min[x0] * env[x0] * gain;
#else
      v = min[x0] * env[x0];
#endif
      if (clipped && mShowClipping && (v <= -MAX_AUDIO))
      {
         if (clipcnt == 0 || clipped[clipcnt - 1] != xx) {
            clipped[clipcnt++] = xx;
         }
      }
      h1 = GetWaveYPos(v, zoomMin, zoomMax,
                       rect.height, dB, true, dBRange, true);

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      v = max[x0] * env[x0] * gain;
#else
      v = max[x0] * env[x0];
#endif
      if (clipped && mShowClipping && (v >= MAX_AUDIO))
      {
         if (clipcnt == 0 || clipped[clipcnt - 1] != xx) {
            clipped[clipcnt++] = xx;
         }
      }
      h2 = GetWaveYPos(v, zoomMin, zoomMax,
                       rect.height, dB, true, dBRange, true);

      // JKC: This adjustment to h1 and h2 ensures that the drawn
      // waveform is continuous.
      if (x0 > 0) {
         if (h1 < lasth2) {
            h1 = lasth2 - 1;
         }
         if (h2 > lasth1) {
            h2 = lasth1 + 1;
         }
      }
      lasth1 = h1;
      lasth2 = h2;

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      r1[x0] = GetWaveYPos(-rms[x0] * env[x0]*gain, zoomMin, zoomMax,
                          rect.height, dB, true, dBRange, true);
      r2[x0] = GetWaveYPos(rms[xx0 * env[x0]*gain, zoomMin, zoomMax,
                          rect.height, dB, true, dBRange, true);
#else
      r1[x0] = GetWaveYPos(-rms[x0] * env[x0], zoomMin, zoomMax,
                          rect.height, dB, true, dBRange, true);
      r2[x0] = GetWaveYPos(rms[x0] * env[x0], zoomMin, zoomMax,
                          rect.height, dB, true, dBRange, true);
#endif
      // Make sure the rms isn't larger than the waveform min/max
      if (r1[x0] > h1 - 1) {
         r1[x0] = h1 - 1;
      }
      if (r2[x0] < h2 + 1) {
         r2[x0] = h2 + 1;
      }
      if (r2[x0] > r1[x0]) {
         r2[x0] = r1[x0];
      }

      if (bl[x0] <= -1) {
         if (drawStripes) {
            // TODO:unify with buffer drawing.
            dc.SetPen((bl[x0] % 2) ? muteSamplePen : samplePen);
            for (int yy = 0; yy < rect.height / 25 + 1; ++yy) {
               // we are drawing over the buffer, but I think DrawLine takes care of this.
               AColor::Line(dc,
                            xx,
                            rect.y + 25 * yy + (x0 /*+pixAnimOffset*/) % 25,
                            xx,
                            rect.y + 25 * yy + (x0 /*+pixAnimOffset*/) % 25 + 6); //take the min so we don't draw past the edge
            }
         }

         // draw a dummy waveform - some kind of sinusoid.  We want to animate it so the user knows it's a dummy.  Use the second's unit of a get time function.
         // Lets use a triangle wave for now since it's easier - I don't want to use sin() or make a wavetable just for this.
         if (drawWaveform) {
            int triX;
            dc.SetPen(samplePen);
            triX = fabs((double)((x0 + pixAnimOffset) % (2 * rect.height)) - rect.height) + rect.height;
            for (int yy = 0; yy < rect.height; ++yy) {
               if ((yy + triX) % rect.height == 0) {
                  dc.DrawPoint(xx, rect.y + yy);
               }
            }
         }

         // Restore the pen for remaining pixel columns!
         dc.SetPen(muted ? muteSamplePen : samplePen);
      }
      else {
         AColor::Line(dc, xx, rect.y + h2, xx, rect.y + h1);
      }
   }

   // Stroke rms over the min-max
   dc.SetPen(muted ? muteRmsPen : rmsPen);
   for (int x0 = 0; x0 < rect.width; ++x0) {
      int xx = rect.x + x0;
      if (bl[x0] <= -1) {
      }
      else if (r1[x0] != r2[x0]) {
         AColor::Line(dc, xx, rect.y + r2[x0], xx, rect.y + r1[x0]);
      }
   }

   // Draw the clipping lines
   if (clipcnt) {
      dc.SetPen(muted ? muteClippedPen : clippedPen);
      while (--clipcnt >= 0) {
         int xx = clipped[clipcnt];
         AColor::Line(dc, xx, rect.y, xx, rect.y + rect.height);
      }
   }

   if (mShowClipping) {
      delete[] clipped;
   }

   delete [] r1;
   delete [] r2;
}

void TrackArtist::DrawIndividualSamples(wxDC &dc, int leftOffset, const wxRect &rect,
                                        float zoomMin, float zoomMax,
                                        bool dB, float dBRange,
                                        const WaveClip *clip,
                                        const ZoomInfo &zoomInfo,
                                        bool bigPoints, bool showPoints, bool muted)
{
   const double toffset = clip->GetOffset();
   double rate = clip->GetRate();
   const double t0 = std::max(0.0, zoomInfo.PositionToTime(0, -leftOffset) - toffset);
   const auto s0 = sampleCount(floor(t0 * rate));
   const auto snSamples = clip->GetNumSamples();
   if (s0 > snSamples)
      return;

   const double t1 = zoomInfo.PositionToTime(rect.width - 1, -leftOffset) - toffset;
   const auto s1 = sampleCount(ceil(t1 * rate));

   // Assume size_t will not overflow, else we wouldn't be here drawing the
   // few individual samples
   auto slen = std::min(snSamples - s0, s1 - s0 + 1).as_size_t();

   if (slen <= 0)
      return;

   float *buffer = new float[slen];
   clip->GetSamples((samplePtr)buffer, floatSample, s0, slen);

   int *xpos = new int[slen];
   int *ypos = new int[slen];
   int *clipped = NULL;
   int clipcnt = 0;

   if (mShowClipping)
      clipped = new int[slen];

   dc.SetPen(muted ? muteSamplePen : samplePen);

   for (decltype(slen) s = 0; s < slen; s++) {
      const double time = toffset + (s + s0).as_double() / rate;
      const int xx = // An offset into the rectangle rect
         std::max(-10000, std::min(10000,
            (int)(zoomInfo.TimeToPosition(time, -leftOffset))));
      xpos[s] = xx;

      const double tt = buffer[s] * clip->GetEnvelope()->GetValue(time);

      if (clipped && mShowClipping && ((tt <= -MAX_AUDIO) || (tt >= MAX_AUDIO)))
         clipped[clipcnt++] = xx;
      ypos[s] =
         std::max(-1,
            std::min(rect.height,
               GetWaveYPos(tt, zoomMin, zoomMax,
                                  rect.height, dB, true, dBRange, false)));
   }

   // Draw lines
   for (decltype(slen) s = 0; s < slen - 1; s++) {
      AColor::Line(dc,
                   rect.x + xpos[s], rect.y + ypos[s],
                   rect.x + xpos[s + 1], rect.y + ypos[s + 1]);
   }

   if (showPoints)
   {
      // Draw points where spacing is enough
      const int tickSize = bigPoints ? 4 : 3;// Bigger ellipses when draggable.
      wxRect pr;
      pr.width = tickSize;
      pr.height = tickSize;
      //different colour when draggable.
      dc.SetBrush( bigPoints ? dragsampleBrush : sampleBrush);
      for (decltype(slen) s = 0; s < slen; s++) {
         if (ypos[s] >= 0 && ypos[s] < rect.height) {
            pr.x = rect.x + xpos[s] - tickSize/2;
            pr.y = rect.y + ypos[s] - tickSize/2;
            dc.DrawEllipse(pr);
         }
      }
   }

   // Draw clipping
   if (clipcnt) {
      dc.SetPen(muted ? muteClippedPen : clippedPen);
      while (--clipcnt >= 0) {
         auto s = clipped[clipcnt];
         AColor::Line(dc, rect.x + s, rect.y, rect.x + s, rect.y + rect.height);
      }
   }

   if (mShowClipping) {
      delete [] clipped;
   }

   delete[]buffer;
   delete[]xpos;
   delete[]ypos;
}

void TrackArtist::DrawEnvelope(wxDC &dc, const wxRect &rect, const double env[],
                               float zoomMin, float zoomMax,
                               bool dB, float dBRange)
{
   int h = rect.height;

   dc.SetPen(AColor::envelopePen);

   for (int x0 = 0; x0 < rect.width; ++x0) {
      int cenvTop = GetWaveYPos(env[x0], zoomMin, zoomMax,
                                h, dB, true, dBRange, true);

      int cenvBot = GetWaveYPos(-env[x0], zoomMin, zoomMax,
                                h, dB, true, dBRange, true);

      int envTop = GetWaveYPos(env[x0], zoomMin, zoomMax,
                               h, dB, true, dBRange, false);

      int envBot = GetWaveYPos(-env[x0], zoomMin, zoomMax,
                               h, dB, true, dBRange, false);

      // Make the collision at zero actually look solid
      if (cenvBot - cenvTop < 9) {
         int value = (int)((zoomMax / (zoomMax - zoomMin)) * h);
         cenvTop = value - 4;
         cenvBot = value + 4;
      }

      DrawEnvLine(dc, rect, x0, envTop, cenvTop, true);
      DrawEnvLine(dc, rect, x0, envBot, cenvBot, false);
   }
}

void TrackArtist::DrawEnvLine(wxDC &dc, const wxRect &rect, int x0, int y0, int cy, bool top)
{
   int xx = rect.x + x0;
   int yy = rect.y + cy;

   if (y0 < 0) {
      if (x0 % 4 != 3) {
         AColor::Line(dc, xx, yy, xx, yy + 3);
      }
   }
   else if (y0 > rect.height) {
      if (x0 % 4 != 3) {
         AColor::Line(dc, xx, yy - 3, xx, yy);
      }
   }
   else {
      if (top) {
         AColor::Line(dc, xx, yy, xx, yy + 3);
      }
      else {
         AColor::Line(dc, xx, yy - 3, xx, yy);
      }
   }
}

void TrackArtist::DrawWaveform(const WaveTrack *track,
                               wxDC & dc,
                               const wxRect & rect,
                               const SelectedRegion &selectedRegion,
                               const ZoomInfo &zoomInfo,
                               bool drawEnvelope,
                               bool bigPoints,
                               bool drawSliders,
                               bool muted)
{
   const bool dB = !track->GetWaveformSettings().isLinear();

   DrawBackgroundWithSelection(&dc, rect, track, blankSelectedBrush, blankBrush,
         selectedRegion, zoomInfo);

   for (const auto &clip: track->GetClips())
      DrawClipWaveform(track, clip.get(), dc, rect, selectedRegion, zoomInfo,
                       drawEnvelope, bigPoints,
                       dB, muted);

   // Update cache for locations, e.g. cutlines and merge points
   track->UpdateLocationsCache();

   for (const auto loc : track->GetCachedLocations()) {
      const int xx = zoomInfo.TimeToPosition(loc.pos);
      if (xx >= 0 && xx < rect.width) {
         dc.SetPen(*wxGREY_PEN);
         AColor::Line(dc, (int) (rect.x + xx - 1), rect.y, (int) (rect.x + xx - 1), rect.y + rect.height);
         if (loc.typ == WaveTrackLocation::locationCutLine) {
            dc.SetPen(*wxRED_PEN);
         }
         else {
            dc.SetPen(*wxBLACK_PEN);
         }
         AColor::Line(dc, (int) (rect.x + xx), rect.y, (int) (rect.x + xx), rect.y + rect.height);
         dc.SetPen(*wxGREY_PEN);
         AColor::Line(dc, (int) (rect.x + xx + 1), rect.y, (int) (rect.x + xx + 1), rect.y + rect.height);
      }
   }

   if (drawSliders) {
      DrawTimeSlider(dc, rect, true);  // directed right
      DrawTimeSlider(dc, rect, false); // directed left
   }
}


namespace {
struct ClipParameters
{
   // Do a bunch of calculations common to waveform and spectrum drawing.
   ClipParameters
      (bool spectrum, const WaveTrack *track, const WaveClip *clip, const wxRect &rect,
      const SelectedRegion &selectedRegion, const ZoomInfo &zoomInfo)
   {
      tOffset = clip->GetOffset();
      rate = clip->GetRate();

      h = zoomInfo.PositionToTime(0, 0
         , true
      );
      h1 = zoomInfo.PositionToTime(rect.width, 0
         , true
      );

      double sel0 = selectedRegion.t0();    //left selection bound
      double sel1 = selectedRegion.t1();    //right selection bound

      //If the track isn't selected, make the selection empty
      if (!track->GetSelected() &&
         (spectrum || !track->IsSyncLockSelected())) { // PRL: why was there a difference for spectrum?
         sel0 = sel1 = 0.0;
      }

      const double trackLen = clip->GetEndTime() - clip->GetStartTime();

      tpre = h - tOffset;                 // offset corrected time of
      //  left edge of display
      tpost = h1 - tOffset;               // offset corrected time of
      //  right edge of display

      const double sps = 1. / rate;            //seconds-per-sample

      // Determine whether we should show individual samples
      // or draw circular points as well
      averagePixelsPerSample = rect.width / (rate * (h1 - h));
      showIndividualSamples = averagePixelsPerSample > 0.5;

      // Calculate actual selection bounds so that t0 > 0 and t1 < the
      // end of the track
      t0 = (tpre >= 0.0 ? tpre : 0.0);
      t1 = (tpost < trackLen - sps * .99 ? tpost : trackLen - sps * .99);
      if (showIndividualSamples) {
         // adjustment so that the last circular point doesn't appear
         // to be hanging off the end
         t1 += 2. / (averagePixelsPerSample * rate);
      }

      // Make sure t1 (the right bound) is greater than 0
      if (t1 < 0.0) {
         t1 = 0.0;
      }

      // Make sure t1 is greater than t0
      if (t0 > t1) {
         t0 = t1;
      }

      // Use the WaveTrack method to show what is selected and 'should' be copied, pasted etc.
      ssel0 = std::max(sampleCount(0), spectrum
         ? sampleCount((sel0 - tOffset) * rate + .99) // PRL: why?
         : track->TimeToLongSamples(sel0 - tOffset)
      );
      ssel1 = std::max(sampleCount(0), spectrum
         ? sampleCount((sel1 - tOffset) * rate + .99) // PRL: why?
         : track->TimeToLongSamples(sel1 - tOffset)
      );

      //trim selection so that it only contains the actual samples
      if (ssel0 != ssel1 && ssel1 > (sampleCount)(0.5 + trackLen * rate)) {
         ssel1 = sampleCount( 0.5 + trackLen * rate );
      }

      // The variable "hiddenMid" will be the rectangle containing the
      // actual waveform, as opposed to any blank area before
      // or after the track, as it would appear without the fisheye.
      hiddenMid = rect;

      // If the left edge of the track is to the right of the left
      // edge of the display, then there's some unused area to the
      // left of the track.  Reduce the "hiddenMid"
      hiddenLeftOffset = 0;
      if (tpre < 0) {
         // Fix Bug #1296 caused by premature conversion to (int).
         wxInt64 time64 = zoomInfo.TimeToPosition(tOffset, 0 , true);
         if( time64 < 0 )
            time64 = 0;
         hiddenLeftOffset = (time64 < rect.width) ? (int)time64 : rect.width;

         hiddenMid.x += hiddenLeftOffset;
         hiddenMid.width -= hiddenLeftOffset;
      }

      // If the right edge of the track is to the left of the the right
      // edge of the display, then there's some unused area to the right
      // of the track.  Reduce the "hiddenMid" rect by the
      // size of the blank area.
      if (tpost > t1) {
         wxInt64 time64 = zoomInfo.TimeToPosition(tOffset+t1, 0 , true);
         if( time64 < 0 )
            time64 = 0;
         const int hiddenRightOffset = (time64 < rect.width) ? (int)time64 : rect.width;

         hiddenMid.width = std::max(0, hiddenRightOffset - hiddenLeftOffset);
      }
      // The variable "mid" will be the rectangle containing the
      // actual waveform, as distorted by the fisheye,
      // as opposed to any blank area before or after the track.
      mid = rect;

      // If the left edge of the track is to the right of the left
      // edge of the display, then there's some unused area to the
      // left of the track.  Reduce the "mid"
      leftOffset = 0;
      if (tpre < 0) {
         wxInt64 time64 = zoomInfo.TimeToPosition(tOffset, 0 , false);
         if( time64 < 0 )
            time64 = 0;
         leftOffset = (time64 < rect.width) ? (int)time64 : rect.width;

         mid.x += leftOffset;
         mid.width -= leftOffset;
      }

      // If the right edge of the track is to the left of the the right
      // edge of the display, then there's some unused area to the right
      // of the track.  Reduce the "mid" rect by the
      // size of the blank area.
      if (tpost > t1) {
         wxInt64 time64 = zoomInfo.TimeToPosition(tOffset+t1, 0 , false);
         if( time64 < 0 )
            time64 = 0;
         const int distortedRightOffset = (time64 < rect.width) ? (int)time64 : rect.width;

         mid.width = std::max(0, distortedRightOffset - leftOffset);
      }
   }

   double tOffset;
   double rate;
   double h; // absolute time of left edge of display
   double tpre; // offset corrected time of left edge of display
   double h1;
   double tpost; // offset corrected time of right edge of display

   // Calculate actual selection bounds so that t0 > 0 and t1 < the
   // end of the track
   double t0;
   double t1;

   double averagePixelsPerSample;
   bool showIndividualSamples;

   sampleCount ssel0;
   sampleCount ssel1;

   wxRect hiddenMid;
   int hiddenLeftOffset;

   wxRect mid;
   int leftOffset;
};
}

#ifdef __GNUC__
#define CONST
#else
#define CONST const
#endif

namespace {
struct WavePortion {
   wxRect rect;
   CONST double averageZoom;
   CONST bool inFisheye;
   WavePortion(int x, int y, int w, int h, double zoom, bool i)
      : rect(x, y, w, h), averageZoom(zoom), inFisheye(i)
   {}
};

void FindWavePortions
   (std::vector<WavePortion> &portions, const wxRect &rect, const ZoomInfo &zoomInfo,
    const ClipParameters &params)
{
   // If there is no fisheye, then only one rectangle has nonzero width.
   // If there is a fisheye, make rectangles for before and after
   // (except when they are squeezed to zero width), and at least one for inside
   // the fisheye.

   ZoomInfo::Intervals intervals;
   zoomInfo.FindIntervals(params.rate, intervals, rect.width, rect.x);
   ZoomInfo::Intervals::const_iterator it = intervals.begin(), end = intervals.end(), prev;
   wxASSERT(it != end && it->position == rect.x);
   const int rightmost = rect.x + rect.width;
   for (int left = rect.x; left < rightmost;) {
      while (it != end && it->position <= left)
         prev = it++;
      if (it == end)
         break;
      const int right = std::max(left, (int)(it->position));
      const int width = right - left;
      if (width > 0)
         portions.push_back(
            WavePortion(left, rect.y, width, rect.height,
                        prev->averageZoom, prev->inFisheye)
         );
      left = right;
   }
}
}

void TrackArtist::DrawClipWaveform(const WaveTrack *track,
                                   const WaveClip *clip,
                                   wxDC & dc,
                                   const wxRect & rect,
                                   const SelectedRegion &selectedRegion,
                                   const ZoomInfo &zoomInfo,
                                   bool drawEnvelope,
                                   bool bigPoints,
                                   bool dB,
                                   bool muted)
{
#ifdef PROFILE_WAVEFORM
   Profiler profiler;
#endif

   const ClipParameters params(false, track, clip, rect, selectedRegion, zoomInfo);
   const wxRect &hiddenMid = params.hiddenMid;
   // The "hiddenMid" rect contains the part of the display actually
   // containing the waveform, as it appears without the fisheye.  If it's empty, we're done.
   if (hiddenMid.width <= 0) {
      return;
   }

   const double &t0 = params.t0;
   const double &tOffset = params.tOffset;
   const double &h = params.h;
   const double &tpre = params.tpre;
   const double &tpost = params.tpost;
   const double &t1 = params.t1;
   const double &averagePixelsPerSample = params.averagePixelsPerSample;
   const double &rate = params.rate;
   double leftOffset = params.leftOffset;
   const wxRect &mid = params.mid;

   const float dBRange = track->GetWaveformSettings().dBRange;

   dc.SetPen(*wxTRANSPARENT_PEN);

   // If we get to this point, the clip is actually visible on the
   // screen, so remember the display rectangle.
   clip->SetDisplayRect(hiddenMid);

   // The bounds (controlled by vertical zooming; -1.0...1.0
   // by default)
   float zoomMin, zoomMax;
   track->GetDisplayBounds(&zoomMin, &zoomMax);

   std::vector<double> vEnv(mid.width);
   double *const env = &vEnv[0];
   clip->GetEnvelope()->GetValues(env, mid.width, leftOffset, zoomInfo);

   // Draw the background of the track, outlining the shape of
   // the envelope and using a colored pen for the selected
   // part of the waveform
   {
      double t0, t1;
      if (track->GetSelected() || track->IsSyncLockSelected()) {
         t0 = track->LongSamplesToTime(track->TimeToLongSamples(selectedRegion.t0())),
            t1 = track->LongSamplesToTime(track->TimeToLongSamples(selectedRegion.t1()));
      }
      else
         t0 = t1 = 0.0;
      DrawWaveformBackground(dc, leftOffset, mid,
         env,
         zoomMin, zoomMax,
         dB, dBRange,
         t0, t1, zoomInfo, drawEnvelope,
         !track->GetSelected());
   }

   WaveDisplay display(hiddenMid.width);
   bool isLoadingOD = false;//true if loading on demand block in sequence.

   const double pps =
      averagePixelsPerSample * rate;

   // For each portion separately, we will decide to draw
   // it as min/max/rms or as individual samples.
   std::vector<WavePortion> portions;
   FindWavePortions(portions, rect, zoomInfo, params);
   const unsigned nPortions = portions.size();

   // Require at least 1/2 pixel per sample for drawing individual samples.
   const double threshold1 = 0.5 * rate;
   // Require at least 3 pixels per sample for drawing the draggable points.
   const double threshold2 = 3 * rate;

   {
      bool showIndividualSamples = false;
      for (unsigned ii = 0; !showIndividualSamples && ii < nPortions; ++ii) {
         const WavePortion &portion = portions[ii];
         showIndividualSamples =
            !portion.inFisheye && portion.averageZoom > threshold1;
      }

      if (!showIndividualSamples) {
         // The WaveClip class handles the details of computing the shape
         // of the waveform.  The only way GetWaveDisplay will fail is if
         // there's a serious error, like some of the waveform data can't
         // be loaded.  So if the function returns false, we can just exit.

         // Note that we compute the full width display even if there is a
         // fisheye hiding part of it, because of the caching.  If the
         // fisheye moves over the background, there is then less to do when
         // redrawing.

         if (!clip->GetWaveDisplay(display,
            t0, pps, isLoadingOD))
            return;
      }
   }

   for (unsigned ii = 0; ii < nPortions; ++ii) {
      WavePortion &portion = portions[ii];
      const bool showIndividualSamples = portion.averageZoom > threshold1;
      const bool showPoints = portion.averageZoom > threshold2;
      wxRect& rect = portion.rect;
      rect.Intersect(mid);
      wxASSERT(rect.width >= 0);

      float *useMin = 0, *useMax = 0, *useRms = 0;
      int *useBl = 0;
      WaveDisplay fisheyeDisplay(rect.width);
      int skipped = 0, skippedLeft = 0, skippedRight = 0;
      if (portion.inFisheye) {
         if (!showIndividualSamples) {
            fisheyeDisplay.Allocate();
            const auto numSamples = clip->GetNumSamples();
            // Get wave display data for different magnification
            int jj = 0;
            for (; jj < rect.width; ++jj) {
               const double time =
                  zoomInfo.PositionToTime(jj, -leftOffset) - tOffset;
               const auto sample = (sampleCount)floor(time * rate + 0.5);
               if (sample < 0) {
                  ++rect.x;
                  ++skippedLeft;
                  continue;
               }
               if (sample >= numSamples)
                  break;
               fisheyeDisplay.where[jj - skippedLeft] = sample;
            }

            skippedRight = rect.width - jj;
            skipped = skippedRight + skippedLeft;
            rect.width -= skipped;

            // where needs a sentinel
            if (jj > 0)
               fisheyeDisplay.where[jj - skippedLeft] =
               1 + fisheyeDisplay.where[jj - skippedLeft - 1];
            fisheyeDisplay.width -= skipped;
            // Get a wave display for the fisheye, uncached.
            if (rect.width > 0)
               if (!clip->GetWaveDisplay(
                     fisheyeDisplay, t0, -1.0, // ignored
                     isLoadingOD))
                  continue; // serious error.  just don't draw??
            useMin = fisheyeDisplay.min;
            useMax = fisheyeDisplay.max;
            useRms = fisheyeDisplay.rms;
            useBl = fisheyeDisplay.bl;
         }
      }
      else {
         const int pos = leftOffset - params.hiddenLeftOffset;
         useMin = display.min + pos;
         useMax = display.max + pos;
         useRms = display.rms + pos;
         useBl = display.bl + pos;
      }

      leftOffset += skippedLeft;

      if (rect.width > 0) {
         if (!showIndividualSamples) {
            std::vector<double> vEnv2(rect.width);
            double *const env2 = &vEnv2[0];
            clip->GetEnvelope()->GetValues(env2, rect.width, leftOffset, zoomInfo);
            DrawMinMaxRMS(dc, rect, env2,
               zoomMin, zoomMax,
               dB, dBRange,
               useMin, useMax, useRms, useBl,
               isLoadingOD, muted
#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
               , track->GetChannelGain(track->GetChannel())
#endif
            );
         }
         else
            DrawIndividualSamples(dc, leftOffset, rect, zoomMin, zoomMax,
               dB, dBRange,
               clip, zoomInfo,
               bigPoints, showPoints, muted);
      }

      leftOffset += rect.width + skippedRight;
   }

   if (drawEnvelope) {
      DrawEnvelope(dc, mid, env, zoomMin, zoomMax, dB, dBRange);
      clip->GetEnvelope()->DrawPoints(dc, rect, zoomInfo, dB, dBRange, zoomMin, zoomMax);
   }

   // Draw arrows on the left side if the track extends to the left of the
   // beginning of time.  :)
   if (h == 0.0 && tOffset < 0.0) {
      DrawNegativeOffsetTrackArrows(dc, rect);
   }

   // Draw clip edges
   dc.SetPen(*wxGREY_PEN);
   if (tpre < 0) {
      AColor::Line(dc,
                   mid.x - 1, mid.y,
                   mid.x - 1, mid.y + rect.height);
   }
   if (tpost > t1) {
      AColor::Line(dc,
                   mid.x + mid.width, mid.y,
                   mid.x + mid.width, mid.y + rect.height);
   }
}


void TrackArtist::DrawTimeSlider(wxDC & dc,
                                 const wxRect & rect,
                                 bool rightwards)
{
   const int border = 3; // 3 pixels all round.
   const int width = 6; // width of the drag box.
   const int taper = 6; // how much the box tapers by.
   const int barSpacing = 4; // how far apart the bars are.
   const int barWidth = 3;
   const int xFlat = 3;

   //Enough space to draw in?
   if (rect.height <= ((taper+border + barSpacing) * 2)) {
      return;
   }
   if (rect.width <= (width * 2 + border * 3)) {
      return;
   }

   // The draggable box is tapered towards the direction you drag it.
   int leftTaper  = rightwards ? 0 : 6;
   int rightTaper = rightwards ? 6 : 0;

   int xLeft = rightwards ? (rect.x + border - 2)
                          : (rect.x + rect.width + 1 - (border + width));
   int yTop  = rect.y + border;
   int yBot  = rect.y + rect.height - border - 1;

   AColor::Light(&dc, false);
   AColor::Line(dc, xLeft,         yBot - leftTaper, xLeft,         yTop + leftTaper);
   AColor::Line(dc, xLeft,         yTop + leftTaper, xLeft + xFlat, yTop);
   AColor::Line(dc, xLeft + xFlat, yTop,             xLeft + width, yTop + rightTaper);

   AColor::Dark(&dc, false);
   AColor::Line(dc, xLeft + width,         yTop + rightTaper, xLeft + width,       yBot - rightTaper);
   AColor::Line(dc, xLeft + width,         yBot - rightTaper, xLeft + width-xFlat, yBot);
   AColor::Line(dc, xLeft + width - xFlat, yBot,              xLeft,               yBot - leftTaper);

   int firstBar = yTop + taper + taper / 2;
   int nBars    = (yBot - yTop - taper * 3) / barSpacing + 1;
   xLeft += (width - barWidth + 1) / 2;
   int yy;
   int i;

   AColor::Light(&dc, false);
   for (i = 0;i < nBars; i++) {
      yy = firstBar + barSpacing * i;
      AColor::Line(dc, xLeft, yy, xLeft + barWidth, yy);
   }
   AColor::Dark(&dc, false);
   for(i = 0;i < nBars; i++){
      yy = firstBar + barSpacing * i + 1;
      AColor::Line(dc, xLeft, yy, xLeft + barWidth, yy);
   }
}

void TrackArtist::DrawSpectrum(const WaveTrack *track,
                               wxDC & dc,
                               const wxRect & rect,
                               const SelectedRegion &selectedRegion,
                               const ZoomInfo &zoomInfo)
{
   DrawBackgroundWithSelection(&dc, rect, track, blankSelectedBrush, blankBrush,
         selectedRegion, zoomInfo);

   WaveTrackCache cache(track);
   for (const auto &clip: track->GetClips()) {
      DrawClipSpectrum(cache, clip.get(), dc, rect, selectedRegion, zoomInfo);
   }
}

static inline float findValue
(const float *spectrum, float bin0, float bin1, unsigned half,
 bool autocorrelation, int gain, int range)
{
   float value;


#if 0
   // Averaging method
   if ((int)(bin1) == (int)(bin0)) {
      value = spectrum[(int)(bin0)];
   } else {
      float binwidth= bin1 - bin0;
      value = spectrum[(int)(bin0)] * (1.f - bin0 + (int)bin0);

      bin0 = 1 + (int)(bin0);
      while (bin0 < (int)(bin1)) {
         value += spectrum[(int)(bin0)];
         bin0 += 1.0;
      }
      // Do not reference past end of freq array.
      if ((int)(bin1) >= (int)half) {
         bin1 -= 1.0;
      }

      value += spectrum[(int)(bin1)] * (bin1 - (int)(bin1));
      value /= binwidth;
   }
#else
   // Maximum method, and no apportionment of any single bins over multiple pixel rows
   // See Bug971
   int index, limitIndex;
   if (autocorrelation) {
      // bin = 2 * half / (half - 1 - array_index);
      // Solve for index
      index = std::max(0.0f, std::min(float(half - 1),
         (half - 1) - (2 * half) / (std::max(1.0f, bin0))
      ));
      limitIndex = std::max(0.0f, std::min(float(half - 1),
         (half - 1) - (2 * half) / (std::max(1.0f, bin1))
      ));
   }
   else {
      index = std::min<int>(half - 1, (int)(floor(0.5 + bin0)));
      limitIndex = std::min<int>(half, (int)(floor(0.5 + bin1)));
   }
   value = spectrum[index];
   while (++index < limitIndex)
      value = std::max(value, spectrum[index]);
#endif
   if (!autocorrelation) {
      // Last step converts dB to a 0.0-1.0 range
      value = (value + range + gain) / (double)range;
   }
   value = std::min(1.0f, std::max(0.0f, value));
   return value;
}


// Helper function to decide on which color set to use.
// dashCount counts both dashes and the spaces between them. 
inline
AColor::ColorGradientChoice ChooseColorSet( float bin0, float bin1, float selBinLo, 
   float selBinCenter, float selBinHi, int dashCount, bool isSpectral )
{
   if (!isSpectral)
      return  AColor::ColorGradientTimeSelected;
   if ((selBinCenter >= 0) && (bin0 <= selBinCenter) &&
       (selBinCenter < bin1))
      return AColor::ColorGradientEdge;
   if ((0 == dashCount % 2) &&
       (((selBinLo >= 0) && (bin0 <= selBinLo) && ( selBinLo < bin1))  ||
        ((selBinHi >= 0) && (bin0 <= selBinHi) && ( selBinHi < bin1))))
      return AColor::ColorGradientEdge;
   if ((selBinLo < 0 || selBinLo < bin1) && (selBinHi < 0 || selBinHi > bin0))
      return  AColor::ColorGradientTimeAndFrequencySelected;

      return  AColor::ColorGradientTimeSelected;
}


void TrackArtist::DrawClipSpectrum(WaveTrackCache &waveTrackCache,
                                   const WaveClip *clip,
                                   wxDC & dc,
                                   const wxRect & rect,
                                   const SelectedRegion &selectedRegion,
                                   const ZoomInfo &zoomInfo)
{
#ifdef PROFILE_WAVEFORM
   Profiler profiler;
#endif

   const WaveTrack *const track = waveTrackCache.GetTrack();
   const SpectrogramSettings &settings = track->GetSpectrogramSettings();
   const bool autocorrelation = (settings.algorithm == SpectrogramSettings::algPitchEAC);

   enum { DASH_LENGTH = 10 /* pixels */ };

   const ClipParameters params(true, track, clip, rect, selectedRegion, zoomInfo);
   const wxRect &hiddenMid = params.hiddenMid;
   // The "hiddenMid" rect contains the part of the display actually
   // containing the waveform, as it appears without the fisheye.  If it's empty, we're done.
   if (hiddenMid.width <= 0) {
      return;
   }

   const double &t0 = params.t0;
   const double &tOffset = params.tOffset;
   const auto &ssel0 = params.ssel0;
   const auto &ssel1 = params.ssel1;
   const double &averagePixelsPerSample = params.averagePixelsPerSample;
   const double &rate = params.rate;
   const double &hiddenLeftOffset = params.hiddenLeftOffset;
   const double &leftOffset = params.leftOffset;
   const wxRect &mid = params.mid;

   // If we get to this point, the clip is actually visible on the
   // screen, so remember the display rectangle.
   clip->SetDisplayRect(hiddenMid);

   double freqLo = SelectedRegion::UndefinedFrequency;
   double freqHi = SelectedRegion::UndefinedFrequency;
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   freqLo = selectedRegion.f0();
   freqHi = selectedRegion.f1();
#endif

   const bool &isGrayscale = settings.isGrayscale;
   const int &range = settings.range;
   const int &gain = settings.gain;

#ifdef EXPERIMENTAL_FIND_NOTES
   const bool &fftFindNotes = settings.fftFindNotes;
   const bool &findNotesMinA = settings.findNotesMinA;
   const bool &numberOfMaxima = settings.numberOfMaxima;
   const bool &findNotesQuantize = settings.findNotesQuantize;
#endif
#ifdef EXPERIMENTAL_FFT_Y_GRID
   const bool &fftYGrid = settings.fftYGrid;
#endif

   dc.SetPen(*wxTRANSPARENT_PEN);

   // We draw directly to a bit image in memory,
   // and then paint this directly to our offscreen
   // bitmap.  Note that this could be optimized even
   // more, but for now this is not bad.  -dmazzoni
   wxImage image((int)mid.width, (int)mid.height);
   if (!image.IsOk())
      return;
   unsigned char *data = image.GetData();

   const auto half = settings.GetFFTLength() / 2;
   const double binUnit = rate / (2 * half);
   const float *freq = 0;
   const sampleCount *where = 0;
   bool updated;
   {
      const double pps = averagePixelsPerSample * rate;
      updated = clip->GetSpectrogram(waveTrackCache, freq, where,
                                     (size_t)hiddenMid.width,
         t0, pps);
   }

   float minFreq, maxFreq;
   track->GetSpectrumBounds(&minFreq, &maxFreq);

   const SpectrogramSettings::ScaleType scaleType = settings.scaleType;

   // nearest frequency to each pixel row from number scale, for selecting
   // the desired fft bin(s) for display on that row
   float *bins = (float*)alloca(sizeof(*bins)*(hiddenMid.height + 1));
   {
       const NumberScale numberScale(settings.GetScale(minFreq, maxFreq, rate, true));

       NumberScale::Iterator it = numberScale.begin(mid.height);
       float nextBin = std::max(0.0f, std::min(float(half - 1), *it));

       int yy;
       for (yy = 0; yy < hiddenMid.height; ++yy) {
          bins[yy] = nextBin;
          nextBin = std::max(0.0f, std::min(float(half - 1), *++it));
       }
       bins[yy] = nextBin;
   }

#ifdef EXPERIMENTAL_FFT_Y_GRID
   const float
      log2 = logf(2.0f),
      scale2 = (lmax - lmin) / log2,
      lmin2 = lmin / log2;

   bool *yGrid;
   yGrid = new bool[mid.height];
   for (int yy = 0; yy < mid.height; ++yy) {
      float n = (float(yy) / mid.height*scale2 - lmin2) * 12;
      float n2 = (float(yy + 1) / mid.height*scale2 - lmin2) * 12;
      float f = float(minFreq) / (fftSkipPoints + 1)*powf(2.0f, n / 12.0f + lmin2);
      float f2 = float(minFreq) / (fftSkipPoints + 1)*powf(2.0f, n2 / 12.0f + lmin2);
      n = logf(f / 440) / log2 * 12;
      n2 = logf(f2 / 440) / log2 * 12;
      if (floor(n) < floor(n2))
         yGrid[yy] = true;
      else
         yGrid[yy] = false;
   }
#endif //EXPERIMENTAL_FFT_Y_GRID

   if (!updated && clip->mSpecPxCache->valid &&
      (clip->mSpecPxCache->len == hiddenMid.height * hiddenMid.width)
      && scaleType == clip->mSpecPxCache->scaleType
      && gain == clip->mSpecPxCache->gain
      && range == clip->mSpecPxCache->range
      && minFreq == clip->mSpecPxCache->minFreq
      && maxFreq == clip->mSpecPxCache->maxFreq
#ifdef EXPERIMENTAL_FFT_Y_GRID
   && fftYGrid==fftYGridOld
#endif //EXPERIMENTAL_FFT_Y_GRID
#ifdef EXPERIMENTAL_FIND_NOTES
   && fftFindNotes==fftFindNotesOld
   && findNotesMinA==findNotesMinAOld
   && numberOfMaxima==findNotesNOld
   && findNotesQuantize==findNotesQuantizeOld
#endif
   ) {
      // Wave clip's spectrum cache is up to date,
      // and so is the spectrum pixel cache
   }
   else {
      // Update the spectrum pixel cache
      clip->mSpecPxCache = std::make_unique<SpecPxCache>(hiddenMid.width * hiddenMid.height);
      clip->mSpecPxCache->valid = true;
      clip->mSpecPxCache->scaleType = scaleType;
      clip->mSpecPxCache->gain = gain;
      clip->mSpecPxCache->range = range;
      clip->mSpecPxCache->minFreq = minFreq;
      clip->mSpecPxCache->maxFreq = maxFreq;
#ifdef EXPERIMENTAL_FIND_NOTES
      fftFindNotesOld = fftFindNotes;
      findNotesMinAOld = findNotesMinA;
      findNotesNOld = numberOfMaxima;
      findNotesQuantizeOld = findNotesQuantize;
#endif

#ifdef EXPERIMENTAL_FIND_NOTES
         lmins = lmin,
         lmaxs = lmax
         ;
#endif //EXPERIMENTAL_FIND_NOTES

#ifdef EXPERIMENTAL_FIND_NOTES
      int maxima[128];
      float maxima0[128], maxima1[128];
      const float
         f2bin = half / (rate / 2.0f),
         bin2f = 1.0f / f2bin,
         minDistance = powf(2.0f, 2.0f / 12.0f),
         i0 = expf(lmin) / binUnit,
         i1 = expf(scale + lmin) / binUnit,
         minColor = 0.0f;
      const int maxTableSize = 1024;
      int *indexes = new int[maxTableSize];
#endif //EXPERIMENTAL_FIND_NOTES

#ifdef _OPENMP
#pragma omp parallel for
#endif
      for (int xx = 0; xx < hiddenMid.width; ++xx) {
         for (int yy = 0; yy < hiddenMid.height; ++yy) {
            const float bin     = bins[yy];
            const float nextBin = bins[yy+1];

            if (settings.scaleType != SpectrogramSettings::stLogarithmic) {
               const float value = findValue
                  (freq + half * xx, bin, nextBin, half, autocorrelation, gain, range);
               clip->mSpecPxCache->values[xx * hiddenMid.height + yy] = value;
            }
            else {
               // Do we need this legacy experiment still?
#ifdef EXPERIMENTAL_FIND_NOTES
               int maximas = 0;
               const int x0 = half * x;
               if (fftFindNotes) {
                  for (int i = maxTableSize - 1; i >= 0; i--)
                     indexes[i] = -1;

                  // Build a table of (most) values, put the index in it.
                  for (int i = (int)(i0); i < (int)(i1); i++) {
                     float freqi = freq[x0 + (int)(i)];
                     int value = (int)((freqi + gain + range) / range*(maxTableSize - 1));
                     if (value < 0)
                        value = 0;
                     if (value >= maxTableSize)
                        value = maxTableSize - 1;
                     indexes[value] = i;
                  }
                  // Build from the indices an array of maxima.
                  for (int i = maxTableSize - 1; i >= 0; i--) {
                     int index = indexes[i];
                     if (index >= 0) {
                        float freqi = freq[x0 + index];
                        if (freqi < findNotesMinA)
                           break;

                        bool ok = true;
                        for (int m = 0; m < maximas; m++) {
                           // Avoid to store very close maxima.
                           float maxm = maxima[m];
                           if (maxm / index < minDistance && index / maxm < minDistance) {
                              ok = false;
                              break;
                           }
                        }
                        if (ok) {
                           maxima[maximas++] = index;
                           if (maximas >= numberOfMaxima)
                              break;
                        }
                     }
                  }

// The f2pix helper macro converts a frequency into a pixel coordinate.
#define f2pix(f) (logf(f)-lmins)/(lmaxs-lmins)*hiddenMid.height

                  // Possibly quantize the maxima frequencies and create the pixel block limits.
                  for (int i = 0; i < maximas; i++) {
                     int index = maxima[i];
                     float f = float(index)*bin2f;
                     if (findNotesQuantize)
                     {
                        f = expf((int)(log(f / 440) / log2 * 12 - 0.5) / 12.0f*log2) * 440;
                        maxima[i] = f*f2bin;
                     }
                     float f0 = expf((log(f / 440) / log2 * 24 - 1) / 24.0f*log2) * 440;
                     maxima0[i] = f2pix(f0);
                     float f1 = expf((log(f / 440) / log2 * 24 + 1) / 24.0f*log2) * 440;
                     maxima1[i] = f2pix(f1);
                  }
               }
               int it = 0;
               int oldBin0 = -1;
               bool inMaximum = false;
#endif //EXPERIMENTAL_FIND_NOTES

               float value;

#ifdef EXPERIMENTAL_FIND_NOTES
               if (fftFindNotes) {
                  if (it < maximas) {
                     float i0 = maxima0[it];
                     if (yy >= i0)
                        inMaximum = true;

                     if (inMaximum) {
                        float i1 = maxima1[it];
                        if (yy + 1 <= i1) {
                           value = findValue(freq + x0, bin, nextBin, half, autocorrelation, gain, range);
                           if (value < findNotesMinA)
                              value = minColor;
                        }
                        else {
                           it++;
                           inMaximum = false;
                           value = minColor;
                        }
                     }
                     else {
                        value = minColor;
                     }
                  }
                  else
                     value = minColor;
               }
               else
#endif //EXPERIMENTAL_FIND_NOTES
               {
                  value = findValue
                     (freq + half * xx, bin, nextBin, half, autocorrelation, gain, range);
               }
               clip->mSpecPxCache->values[xx * hiddenMid.height + yy] = value;
            } // logF
         } // each yy
      } // each xx
   } // updating cache

   float selBinLo = freqLo / binUnit;
   float selBinHi = freqHi / binUnit;
   float selBinCenter =
      ((freqLo < 0 || freqHi < 0) ? -1 : sqrt(freqLo * freqHi)) / binUnit;

   const bool isSpectral = settings.SpectralSelectionEnabled();
   const bool hidden = (ZoomInfo::HIDDEN == zoomInfo.GetFisheyeState());
   const int begin = hidden
      ? 0
      : std::max(0, (int)(zoomInfo.GetFisheyeLeftBoundary(-leftOffset)));
   const int end = hidden
      ? 0
      : std::min(mid.width, (int)(zoomInfo.GetFisheyeRightBoundary(-leftOffset)));
   const size_t numPixels = std::max(0, end - begin);
   const size_t zeroPaddingFactor = autocorrelation ? 1 : settings.ZeroPaddingFactor();
   SpecCache specCache
      (numPixels, settings.algorithm, -1,
       t0, settings.windowType,
       settings.WindowSize(), zeroPaddingFactor, settings.frequencyGain);
   if (numPixels > 0) {
      for (int ii = begin; ii < end; ++ii) {
         const double time = zoomInfo.PositionToTime(ii, -leftOffset) - tOffset;
         specCache.where[ii - begin] = sampleCount(0.5 + rate * time);
      }
      specCache.Populate
         (settings, waveTrackCache,
          0, 0, numPixels,
          clip->GetNumSamples(),
          tOffset, rate,
          0 // FIXME: PRL -- make reassignment work with fisheye
       );
   }

   // build color gradient tables (not thread safe)
   if (!AColor::gradient_inited)
      AColor::PreComputeGradient();

   // left pixel column of the fisheye
   int fisheyeLeft = zoomInfo.GetFisheyeLeftBoundary(-leftOffset);

#ifdef _OPENMP
#pragma omp parallel for
#endif
   for (int xx = 0; xx < mid.width; ++xx) {

      int correctedX = xx + leftOffset - hiddenLeftOffset;

      // in fisheye mode the time scale has changed, so the row values aren't cached
      // in the loop above, and must be fetched from fft cache
      float* uncached;
      if (!zoomInfo.InFisheye(xx, -leftOffset)) {
          uncached = 0;
      }
      else {
          int specIndex = (xx - fisheyeLeft) * half;
          wxASSERT(specIndex >= 0 && specIndex < specCache.freq.size());
          uncached = &specCache.freq[specIndex];
      }

      // zoomInfo must be queried for each column since with fisheye enabled
      // time between columns is variable
      auto w0 = sampleCount(0.5 + rate *
                   (zoomInfo.PositionToTime(xx, -leftOffset) - tOffset));

      auto w1 = sampleCount(0.5 + rate *
                    (zoomInfo.PositionToTime(xx+1, -leftOffset) - tOffset));

      bool maybeSelected = ssel0 <= w0 && w1 < ssel1;

      for (int yy = 0; yy < hiddenMid.height; ++yy) {
         const float bin     = bins[yy];
         const float nextBin = bins[yy+1];

         // For spectral selection, determine what colour
         // set to use.  We use a darker selection if
         // in both spectral range and time range.

         AColor::ColorGradientChoice selected = AColor::ColorGradientUnselected;

         // If we are in the time selected range, then we may use a different color set.
         if (maybeSelected)
            selected =
               ChooseColorSet(bin, nextBin, selBinLo, selBinCenter, selBinHi,
                  (xx + leftOffset - hiddenLeftOffset) / DASH_LENGTH, isSpectral);

         const float value = uncached
            ? findValue(uncached, bin, nextBin, half, autocorrelation, gain, range)
            : clip->mSpecPxCache->values[correctedX * hiddenMid.height + yy];

         unsigned char rv, gv, bv;
         GetColorGradient(value, selected, isGrayscale, &rv, &gv, &bv);

#ifdef EXPERIMENTAL_FFT_Y_GRID
         if (fftYGrid && yGrid[yy]) {
            rv /= 1.1f;
            gv /= 1.1f;
            bv /= 1.1f;
         }
#endif //EXPERIMENTAL_FFT_Y_GRID

         int px = ((mid.height - 1 - yy) * mid.width + xx) * 3;
         data[px++] = rv;
         data[px++] = gv;
         data[px] = bv;
      } // each yy
   } // each xx

   wxBitmap converted = wxBitmap(image);

   wxMemoryDC memDC;

   memDC.SelectObject(converted);

   dc.Blit(mid.x, mid.y, mid.width, mid.height, &memDC, 0, 0, wxCOPY, FALSE);

#ifdef EXPERIMENTAL_FFT_Y_GRID
   delete[] yGrid;
#endif //EXPERIMENTAL_FFT_Y_GRID
}

#ifdef USE_MIDI
/*
Note: recall that Allegro attributes end in a type identifying letter.

In addition to standard notes, an Allegro_Note can denote a graphic.
A graphic is a note with a loud of zero (for quick testing) and an
attribute named "shapea" set to one of the following atoms:
    line
        from (time, pitch) to (time+dur, y1r), where y1r is an
          attribute
    rectangle
        from (time, pitch) to (time+dur, y1r), where y1r is an
          attribute
    triangle
        coordinates are (time, pitch), (x1r, y1r), (x2r, y2r)
        dur must be the max of x1r-time, x2r-time
    polygon
        coordinates are (time, pitch), (x1r, y1r), (x2r, y2r),
          (x3r, y3r), ... are coordinates (since we cannot represent
          arrays as attribute values, we just generate as many
          attribute names as we need)
        dur must be the max of xNr-time for all N
    oval
        similar to rectangle
        Note: this oval has horizontal and vertical axes only
    text
        drawn at (time, pitch)
        duration should be zero (text is clipped based on time and duration,
          NOT based on actual coordinates)

and optional attributes as follows:
    linecolori is 0x00rrggbb format color for line or text foreground
    fillcolori is 0x00rrggbb format color for fill or text background
    linethicki is line thickness in pixels, 0 for no line
    filll is true to fill rectangle or draw text background (default is false)
    fonta is one of ['roman', 'swiss', 'modern'] (font, otherwise use default)
    weighta may be 'bold' (font) (default is normal)
    sizei is font size (default is 8)
    justifys is a string containing two letters, a horizontal code and a
      vertical code. The horizontal code is as follows:
        l: the coordinate is to the left of the string (default)
        c: the coordinate is at the center of the string
        r: the coordinate is at the right of the string
      The vertical code is as follows:
        t: the coordinate is at the top of the string
        c: the coordinate is at the center of the string
        b: the coordinate is at the bottom of the string
        d: the coordinate is at the baseline of the string (default)
      Thus, -justifys:"lt" places the left top of the string at the point
        given by (pitch, time). The default value is "ld".
*/

/* Declare Static functions */
static const char *IsShape(Alg_note_ptr note);
static double LookupRealAttribute(Alg_note_ptr note, Alg_attribute attr, double def);
static long LookupIntAttribute(Alg_note_ptr note, Alg_attribute attr, long def);
static bool LookupLogicalAttribute(Alg_note_ptr note, Alg_attribute attr, bool def);
static const char *LookupStringAttribute(Alg_note_ptr note, Alg_attribute attr, const char *def);
static const char *LookupAtomAttribute(Alg_note_ptr note, Alg_attribute attr, char *def);
//static int PITCH_TO_Y(double p, int bottom);

// returns NULL if note is not a shape,
// returns atom (string) value of note if note is a shape
const char *IsShape(Alg_note_ptr note)
{
  Alg_parameters_ptr parameters = note->parameters;
  while (parameters) {
    if (strcmp(parameters->parm.attr_name(), "shapea") == 0) {
      return parameters->parm.a;
    }
    parameters = parameters->next;
  }
  return NULL;
}

// returns value of attr, or default if not found
double LookupRealAttribute(Alg_note_ptr note, Alg_attribute attr, double def)
{
  Alg_parameters_ptr parameters = note->parameters;
  while (parameters) {
    if (parameters->parm.attr_name() == attr + 1 &&
        parameters->parm.attr_type() == 'r') {
      return parameters->parm.r;
    }
    parameters = parameters->next;
  }
  return def;
}

// returns value of attr, or default if not found
long LookupIntAttribute(Alg_note_ptr note, Alg_attribute attr, long def)
{
  Alg_parameters_ptr parameters = note->parameters;
  while (parameters) {
    if (parameters->parm.attr_name() == attr + 1 &&
        parameters->parm.attr_type() == 'i') {
      return parameters->parm.i;
    }
    parameters = parameters->next;
  }
  return def;
}

// returns value of attr, or default if not found
bool LookupLogicalAttribute(Alg_note_ptr note, Alg_attribute attr, bool def)
{
  Alg_parameters_ptr parameters = note->parameters;
  while (parameters) {
    if (parameters->parm.attr_name() == attr + 1 &&
        parameters->parm.attr_type() == 'l') {
      return parameters->parm.l;
    }
    parameters = parameters->next;
  }
  return def;
}

// returns value of attr, or default if not found
const char *LookupStringAttribute(Alg_note_ptr note, Alg_attribute attr, const char *def)
{
  Alg_parameters_ptr parameters = note->parameters;
  while (parameters) {
    if (parameters->parm.attr_name() == attr + 1 &&
        parameters->parm.attr_type() == 's') {
      return parameters->parm.s;
    }
    parameters = parameters->next;
  }
  return def;
}

// returns value of attr, or default if not found
const char *LookupAtomAttribute(Alg_note_ptr note, Alg_attribute attr, char *def)
{
  Alg_parameters_ptr parameters = note->parameters;
  while (parameters) {
    if (parameters->parm.attr_name() == attr + 1 &&
        parameters->parm.attr_type() == 'a') {
      return parameters->parm.s;
    }
    parameters = parameters->next;
  }
  return def;
}

#define TIME_TO_X(t) (zoomInfo.TimeToPosition((t), rect.x))
#define X_TO_TIME(xx) (zoomInfo.PositionToTime((xx), rect.x))

// CLIP(x) changes x to lie between +/- CLIP_MAX due to graphics display problems
//  with very large coordinate values (this happens when you zoom in very far)
//  This will cause incorrect things to be displayed, but at these levels of zoom
//  you will only see a small fraction of the overall shape. Note that rectangles
//  and lines are clipped in a way that preserves correct graphics, so in
//  particular, line plots will be correct at any zoom (limited by floating point
//  precision).
#define CLIP_MAX 16000
#define CLIP(xx) { long c = (xx); if (c < -CLIP_MAX) c = -CLIP_MAX; \
                  if (c > CLIP_MAX) c = CLIP_MAX; (xx) = c; }

#define RED(i) ( unsigned char )( (((i) >> 16) & 0xff) )
#define GREEN(i) ( unsigned char )( (((i) >> 8) & 0xff) )
#define BLUE(i) ( unsigned char )( ((i) & 0xff) )

//#define PITCH_TO_Y(p) (rect.y + rect.height - (int)(pitchht * ((p) + 0.5 - pitch0) + 0.5))

/*
int PitchToY(double p, int bottom)
{
   int octave = (((int) (p + 0.5)) / 12);
   int n = ((int) (p + 0.5)) % 12;

   return IPITCH_TO_Y((int) (p + 0.5));
   // was: bottom - octave * octaveHeight - notePos[n] - 4;
}
*/

/* DrawNoteBackground is called by DrawNoteTrack twice: once to draw
   the unselected background, and once to draw the selected background.
   The selected background is the same except for the horizontal range
   and the colors. The background rectangle region is given by rect; the
   selected region is given by sel. The first time this is called,
   sel is equal to rect, and the entire region is drawn with unselected
   background colors.
 */
void TrackArtist::DrawNoteBackground(const NoteTrack *track, wxDC &dc,
                                     const wxRect &rect, const wxRect &sel,
                                     const ZoomInfo &zoomInfo,
                                     const wxBrush &wb, const wxPen &wp,
                                     const wxBrush &bb, const wxPen &bp,
                                     const wxPen &mp)
{
   dc.SetBrush(wb);
   dc.SetPen(wp);
   dc.DrawRectangle(sel); // fill rectangle with white keys background

   int left = TIME_TO_X(track->GetOffset());
   if (left < sel.x) left = sel.x; // clip on left

   int right = TIME_TO_X(track->GetOffset() + track->mSeq->get_real_dur());
   if (right > sel.x + sel.width) right = sel.x + sel.width; // clip on right

   // need overlap between MIDI data and the background region
   if (left >= right) return;

   dc.SetBrush(bb);
   int octave = 0;
   // obottom is the window coordinate of octave divider line
   int obottom = track->GetOctaveBottom(octave);
   // eOffset is for the line between E and F; there's another line
   // between B and C, hence the offset of 2 for two line thicknesses
   int eOffset = track->GetPitchHeight() * 5 + 2;
   while (obottom > rect.y + track->GetNoteMargin() + 3) {
      // draw a black line separating octaves if this octave botton is visible
      if (obottom < rect.y + rect.height - track->GetNoteMargin()) {
         dc.SetPen(*wxBLACK_PEN);
         // obottom - 1 because obottom is at the bottom of the line
         AColor::Line(dc, left, obottom - 1, right, obottom - 1);
      }
      dc.SetPen(bp);
      // draw a black-key stripe colored line separating E and F if visible
      if (obottom - eOffset > rect.y && obottom - eOffset < rect.y + rect.height) {
         AColor::Line(dc, left, obottom - eOffset,
                          right, obottom - eOffset);
      }

      // draw visible black key lines
      wxRect br;
      br.x = left;
      br.width = right - left;
      br.height = track->GetPitchHeight();
      for (int black = 0; black < 5; black++) {
         br.y = obottom - track->GetBlackPos(black);
         if (br.y > rect.y && br.y + br.height < rect.y + rect.height) {
            dc.DrawRectangle(br); // draw each black key background stripe
         }
      }
      obottom = track->GetOctaveBottom(++octave);
   }

   // draw bar lines
   Alg_seq_ptr seq = track->mSeq.get();
   // We assume that sliding a NoteTrack around slides the barlines
   // along with the notes. This means that when we write out a track
   // as Allegro or MIDI without the offset, we'll need to insert an
   // integer number of measures of silence, using tempo change to
   // match the duration to the offset.
   // Iterate over all time signatures to generate beat positions of
   // bar lines, map the beats to times, map the times to position,
   // and draw the bar lines that fall within the region of interest (sel)
   // seq->convert_to_beats();
   dc.SetPen(mp);
   Alg_time_sigs &sigs = seq->time_sig;
   int i = 0; // index into ts[]
   double next_bar_beat = 0.0;
   double beats_per_measure = 4.0;
   while (true) {
      if (i < sigs.length() && sigs[i].beat < next_bar_beat + ALG_EPS) {
         // NEW time signature takes effect
         Alg_time_sig &sig = sigs[i++];
         next_bar_beat = sig.beat;
         beats_per_measure = (sig.num * 4.0) / sig.den;
      }
      // map beat to time
      double t = seq->get_time_map()->beat_to_time(next_bar_beat);
      // map time to position
      int xx = TIME_TO_X(t + track->GetOffset());
      if (xx > right) break;
      AColor::Line(dc, xx, sel.y, xx, sel.y + sel.height);
      next_bar_beat += beats_per_measure;
   }
}

/* DrawNoteTrack:
Draws a piano-roll style display of sequence data with added
graphics. Since there may be notes outside of the display region,
reserve a half-note-height margin at the top and bottom of the
window and draw out-of-bounds notes here instead.
*/
void TrackArtist::DrawNoteTrack(const NoteTrack *track,
                                wxDC & dc,
                                const wxRect & rect,
                                const SelectedRegion &selectedRegion,
                                const ZoomInfo &zoomInfo,
                                bool muted)
{
   SonifyBeginNoteBackground();
   double sel0 = selectedRegion.t0();
   double sel1 = selectedRegion.t1();

   const double h = X_TO_TIME(rect.x);
   const double h1 = X_TO_TIME(rect.x + rect.width);

   Alg_seq_ptr seq = track->mSeq.get();
   if (!seq) {
      assert(track->mSerializationBuffer);
      // JKC: Previously this indirected via seq->, a NULL pointer.
      // This was actually OK, since unserialize is a static function.
      // Alg_seq:: is clearer.
      std::unique_ptr<Alg_track> alg_track{ Alg_seq::unserialize(track->mSerializationBuffer,
            track->mSerializationLength) };
      assert(alg_track->get_type() == 's');
      const_cast<NoteTrack*>(track)->mSeq.reset(seq = static_cast<Alg_seq*>(alg_track.release()));
      free(track->mSerializationBuffer);
      track->mSerializationBuffer = NULL;
   }
   assert(seq);
   int visibleChannels = track->mVisibleChannels;

   if (!track->GetSelected())
      sel0 = sel1 = 0.0;

   // reserve 1/2 note height at top and bottom of track for
   // out-of-bounds notes
   int numPitches = (rect.height) / track->GetPitchHeight();
   if (numPitches < 0) numPitches = 0; // cannot be negative

   // bottom is the hypothetical location of the bottom of pitch 0 relative to
   // the top of the clipping region rect: rect.height - PITCH_HEIGHT/2 is where the
   // bottomNote is displayed, and to that
   // we add the height of bottomNote from the position of pitch 0
   track->PrepareIPitchToY(rect);

   // Background comes in 6 colors:
   //   214, 214,214 -- unselected white keys
   //   192,192,192 -- unselected black keys
   //   170,170,170 -- unselected bar lines
   //   165,165,190 -- selected white keys
   //   148,148,170 -- selected black keys
   //   131,131,150 -- selected bar lines
   wxPen blackStripePen;
   blackStripePen.SetColour(192, 192, 192);
   wxBrush blackStripeBrush;
   blackStripeBrush.SetColour(192, 192, 192);
   wxPen barLinePen;
   barLinePen.SetColour(170, 170, 170);

   DrawNoteBackground(track, dc, rect, rect, zoomInfo, blankBrush, blankPen,
                      blackStripeBrush, blackStripePen, barLinePen);

   dc.SetClippingRegion(rect);

   // Draw the selection background
   // First, the white keys, as a single rectangle
   // In other words fill the selection area with selectedWhiteKeyPen
   wxRect selBG;
   selBG.y = rect.y;
   selBG.height = rect.height;
   selBG.x = TIME_TO_X(sel0);
   selBG.width = TIME_TO_X(sel1) - TIME_TO_X(sel0);

   wxPen selectedWhiteKeyPen;
   selectedWhiteKeyPen.SetColour(165, 165, 190);
   dc.SetPen(selectedWhiteKeyPen);

   wxBrush selectedWhiteKeyBrush;
   selectedWhiteKeyBrush.SetColour(165, 165, 190);
   // Then, the black keys and octave stripes, as smaller rectangles
   wxPen selectedBlackKeyPen;
   selectedBlackKeyPen.SetColour(148, 148, 170);
   wxBrush selectedBlackKeyBrush;
   selectedBlackKeyBrush.SetColour(148, 148, 170);
   wxPen selectedBarLinePen;
   selectedBarLinePen.SetColour(131, 131, 150);

   DrawNoteBackground(track, dc, rect, selBG, zoomInfo,
                      selectedWhiteKeyBrush, selectedWhiteKeyPen,
                      selectedBlackKeyBrush, selectedBlackKeyPen,
                      selectedBarLinePen);
   SonifyEndNoteBackground();
   SonifyBeginNoteForeground();
   int marg = track->GetNoteMargin();

   // NOTE: it would be better to put this in some global initialization
   // function rather than do lookups every time.
   Alg_attribute line = symbol_table.insert_string("line");
   Alg_attribute rectangle = symbol_table.insert_string("rectangle");
   Alg_attribute triangle = symbol_table.insert_string("triangle");
   Alg_attribute polygon = symbol_table.insert_string("polygon");
   Alg_attribute oval = symbol_table.insert_string("oval");
   Alg_attribute text = symbol_table.insert_string("text");
   Alg_attribute texts = symbol_table.insert_string("texts");
   Alg_attribute x1r = symbol_table.insert_string("x1r");
   Alg_attribute x2r = symbol_table.insert_string("x2r");
   Alg_attribute y1r = symbol_table.insert_string("y1r");
   Alg_attribute y2r = symbol_table.insert_string("y2r");
   Alg_attribute linecolori = symbol_table.insert_string("linecolori");
   Alg_attribute fillcolori = symbol_table.insert_string("fillcolori");
   Alg_attribute linethicki = symbol_table.insert_string("linethicki");
   Alg_attribute filll = symbol_table.insert_string("filll");
   Alg_attribute fonta = symbol_table.insert_string("fonta");
   Alg_attribute roman = symbol_table.insert_string("roman");
   Alg_attribute swiss = symbol_table.insert_string("swiss");
   Alg_attribute modern = symbol_table.insert_string("modern");
   Alg_attribute weighta = symbol_table.insert_string("weighta");
   Alg_attribute bold = symbol_table.insert_string("bold");
   Alg_attribute sizei = symbol_table.insert_string("sizei");
   Alg_attribute justifys = symbol_table.insert_string("justifys");

   // We want to draw in seconds, so we need to convert to seconds
   seq->convert_to_seconds();

   Alg_iterator iterator(seq, false);
   iterator.begin();
   //for every event
   Alg_event_ptr evt;
   while (0 != (evt = iterator.next())) {
      if (evt->get_type() == 'n') { // 'n' means a note
         Alg_note_ptr note = (Alg_note_ptr) evt;
         // if the note's channel is visible
         if (visibleChannels & (1 << (evt->chan & 15))) {
            double xx = note->time + track->GetOffset();
            double x1 = xx + note->dur;
            if (xx < h1 && x1 > h) { // omit if outside box
               const char *shape = NULL;
               if (note->loud > 0.0 || 0 == (shape = IsShape(note))) {
                  wxRect nr; // "note rectangle"
                  nr.y = track->PitchToY(note->pitch);
                  nr.height = track->GetPitchHeight();

                  nr.x = TIME_TO_X(xx);
                  nr.width = TIME_TO_X(x1) - nr.x;

                  if (nr.x + nr.width >= rect.x && nr.x < rect.x + rect.width) {
                     if (nr.x < rect.x) {
                        nr.width -= (rect.x - nr.x);
                        nr.x = rect.x;
                     }
                     if (nr.x + nr.width > rect.x + rect.width) // clip on right
                        nr.width = rect.x + rect.width - nr.x;

                     if (nr.y + nr.height < rect.y + marg + 3) {
                         // too high for window
                         nr.y = rect.y;
                         nr.height = marg;
                         dc.SetBrush(*wxBLACK_BRUSH);
                         dc.SetPen(*wxBLACK_PEN);
                         dc.DrawRectangle(nr);
                     } else if (nr.y >= rect.y + rect.height - marg - 1) {
                         // too low for window
                         nr.y = rect.y + rect.height - marg;
                         nr.height = marg;
                         dc.SetBrush(*wxBLACK_BRUSH);
                         dc.SetPen(*wxBLACK_PEN);
                         dc.DrawRectangle(nr);
                     } else {
                        if (nr.y + nr.height > rect.y + rect.height - marg)
                           nr.height = rect.y + rect.height - nr.y;
                        if (nr.y < rect.y + marg) {
                           int offset = rect.y + marg - nr.y;
                           nr.height -= offset;
                           nr.y += offset;
                        }
                        // nr.y += rect.y;
                        if (muted)
                           AColor::LightMIDIChannel(&dc, note->chan + 1);
                        else
                           AColor::MIDIChannel(&dc, note->chan + 1);
                        dc.DrawRectangle(nr);
                        if (track->GetPitchHeight() > 2) {
                           AColor::LightMIDIChannel(&dc, note->chan + 1);
                           AColor::Line(dc, nr.x, nr.y, nr.x + nr.width-2, nr.y);
                           AColor::Line(dc, nr.x, nr.y, nr.x, nr.y + nr.height-2);
                           AColor::DarkMIDIChannel(&dc, note->chan + 1);
                           AColor::Line(dc, nr.x+nr.width-1, nr.y,
                                 nr.x+nr.width-1, nr.y+nr.height-1);
                           AColor::Line(dc, nr.x, nr.y+nr.height-1,
                                 nr.x+nr.width-1, nr.y+nr.height-1);
                        }
//                        }
                     }
                  }
               } else if (shape) {
                  // draw a shape according to attributes
                  // add 0.5 to pitch because pitches are plotted with
                  // height = PITCH_HEIGHT; thus, the center is raised
                  // by PITCH_HEIGHT * 0.5
                  int yy = track->PitchToY(note->pitch);
                  long linecolor = LookupIntAttribute(note, linecolori, -1);
                  long linethick = LookupIntAttribute(note, linethicki, 1);
                  long fillcolor = -1;
                  long fillflag = 0;

                  // set default color to be that of channel
                  AColor::MIDIChannel(&dc, note->chan+1);
                  if (shape != text) {
                     if (linecolor != -1)
                        dc.SetPen(wxPen(wxColour(RED(linecolor),
                              GREEN(linecolor),
                              BLUE(linecolor)),
                              linethick, wxSOLID));
                  }
                  if (shape != line) {
                     fillcolor = LookupIntAttribute(note, fillcolori, -1);
                     fillflag = LookupLogicalAttribute(note, filll, false);

                     if (fillcolor != -1)
                        dc.SetBrush(wxBrush(wxColour(RED(fillcolor),
                              GREEN(fillcolor),
                              BLUE(fillcolor)),
                              wxSOLID));
                     if (!fillflag) dc.SetBrush(*wxTRANSPARENT_BRUSH);
                  }
                  int y1 = track->PitchToY(LookupRealAttribute(note, y1r, note->pitch));
                  if (shape == line) {
                     // extreme zooms caues problems under windows, so we have to do some
                     // clipping before calling display routine
                     if (xx < h) { // clip line on left
                        yy = (int)((yy + (y1 - yy) * (h - xx) / (x1 - xx)) + 0.5);
                        xx = h;
                     }
                     if (x1 > h1) { // clip line on right
                        y1 = (int)((yy + (y1 - yy) * (h1 - xx) / (x1 - xx)) + 0.5);
                        x1 = h1;
                     }
                     AColor::Line(dc, TIME_TO_X(xx), yy, TIME_TO_X(x1), y1);
                  } else if (shape == rectangle) {
                     if (xx < h) { // clip on left, leave 10 pixels to spare
                        xx = X_TO_TIME(rect.x - (linethick + 10));
                     }
                     if (x1 > h1) { // clip on right, leave 10 pixels to spare
                        xx = X_TO_TIME(rect.x + rect.width + linethick + 10);
                     }
                     dc.DrawRectangle(TIME_TO_X(xx), yy, TIME_TO_X(x1) - TIME_TO_X(xx), y1 - yy + 1);
                  } else if (shape == triangle) {
                     wxPoint points[3];
                     points[0].x = TIME_TO_X(xx);
                     CLIP(points[0].x);
                     points[0].y = yy;
                     points[1].x = TIME_TO_X(LookupRealAttribute(note, x1r, note->pitch));
                     CLIP(points[1].x);
                     points[1].y = y1;
                     points[2].x = TIME_TO_X(LookupRealAttribute(note, x2r, xx));
                     CLIP(points[2].x);
                     points[2].y = track->PitchToY(LookupRealAttribute(note, y2r, note->pitch));
                     dc.DrawPolygon(3, points);
                  } else if (shape == polygon) {
                     wxPoint points[20]; // upper bound of 20 sides
                     points[0].x = TIME_TO_X(xx);
                     CLIP(points[0].x);
                     points[0].y = yy;
                     points[1].x = TIME_TO_X(LookupRealAttribute(note, x1r, xx));
                     CLIP(points[1].x);
                     points[1].y = y1;
                     points[2].x = TIME_TO_X(LookupRealAttribute(note, x2r, xx));
                     CLIP(points[2].x);
                     points[2].y = track->PitchToY(LookupRealAttribute(note, y2r, note->pitch));
                     int n = 3;
                     while (n < 20) {
                        char name[8];
                        sprintf(name, "x%dr", n);
                        Alg_attribute attr = symbol_table.insert_string(name);
                        double xn = LookupRealAttribute(note, attr, -1000000.0);
                        if (xn == -1000000.0) break;
                        points[n].x = TIME_TO_X(xn);
                        CLIP(points[n].x);
                        sprintf(name, "y%dr", n - 1);
                        attr = symbol_table.insert_string(name);
                        double yn = LookupRealAttribute(note, attr, -1000000.0);
                        if (yn == -1000000.0) break;
                        points[n].y = track->PitchToY(yn);
                        n++;
                     }
                     dc.DrawPolygon(n, points);
                  } else if (shape == oval) {
                     int ix = TIME_TO_X(xx);
                     CLIP(ix);
                     int ix1 = TIME_TO_X(x1) - TIME_TO_X(xx);
                     if (ix1 > CLIP_MAX * 2) ix1 = CLIP_MAX * 2; // CLIP a width
                     dc.DrawEllipse(ix, yy, ix1, y1 - yy + 1);
                  } else if (shape == text) {
                     if (linecolor != -1)
                        dc.SetTextForeground(wxColour(RED(linecolor),
                              GREEN(linecolor),
                              BLUE(linecolor)));
                     // if no color specified, copy color from brush
                     else dc.SetTextForeground(dc.GetBrush().GetColour());

                     // This seems to have no effect, so I commented it out. -RBD
                     //if (fillcolor != -1)
                     //  dc.SetTextBackground(wxColour(RED(fillcolor),
                     //                                GREEN(fillcolor),
                     //                                BLUE(fillcolor)));
                     //// if no color specified, copy color from brush
                     //else dc.SetTextBackground(dc.GetPen().GetColour());

                     const char *font = LookupAtomAttribute(note, fonta, NULL);
                     const char *weight = LookupAtomAttribute(note, weighta, NULL);
                     int size = LookupIntAttribute(note, sizei, 8);
                     const char *justify = LookupStringAttribute(note, justifys, "ld");
                     wxFont wxfont;
                     wxfont.SetFamily(font == roman ? wxROMAN :
                        (font == swiss ? wxSWISS :
                           (font == modern ? wxMODERN : wxDEFAULT)));
                     wxfont.SetStyle(wxNORMAL);
                     wxfont.SetWeight(weight == bold ? wxBOLD : wxNORMAL);
                     wxfont.SetPointSize(size);
                     dc.SetFont(wxfont);

                     // now do justification
                     const char *s = LookupStringAttribute(note, texts, "");
                     wxCoord textWidth, textHeight;
                     dc.GetTextExtent(wxString::FromUTF8(s), &textWidth, &textHeight);
                     long hoffset = 0;
                     long voffset = -textHeight; // default should be baseline of text

                     if (strlen(justify) != 2) justify = "ld";

                     if (justify[0] == 'c') hoffset = -(textWidth/2);
                     else if (justify[0] == 'r') hoffset = -textWidth;

                     if (justify[1] == 't') voffset = 0;
                     else if (justify[1] == 'c') voffset = -(textHeight/2);
                     else if (justify[1] == 'b') voffset = -textHeight;
                     if (fillflag) {
                        // It should be possible to do this with background color,
                        // but maybe because of the transfer mode, no background is
                        // drawn. To fix this, just draw a rectangle:
                        dc.SetPen(wxPen(wxColour(RED(fillcolor),
                              GREEN(fillcolor),
                              BLUE(fillcolor)),
                              1, wxSOLID));
                        dc.DrawRectangle(TIME_TO_X(xx) + hoffset, yy + voffset,
                              textWidth, textHeight);
                     }
                     dc.DrawText(LAT1CTOWX(s), TIME_TO_X(xx) + hoffset, yy + voffset);
                  }
               }
            }
         }
      }
   }
   iterator.end();
   // draw black line between top/bottom margins and the track
   dc.SetPen(*wxBLACK_PEN);
   AColor::Line(dc, rect.x, rect.y + marg, rect.x + rect.width, rect.y + marg);
   AColor::Line(dc, rect.x, rect.y + rect.height - marg - 1, // subtract 1 to get
                rect.x + rect.width, rect.y + rect.height - marg - 1); // top of line

   if (h == 0.0 && track->GetOffset() < 0.0) {
      DrawNegativeOffsetTrackArrows(dc, rect);
   }

   dc.DestroyClippingRegion();
   SonifyEndNoteForeground();
}
#endif // USE_MIDI


void TrackArtist::DrawLabelTrack(const LabelTrack *track,
                                 wxDC & dc,
                                 const wxRect & rect,
                                 const SelectedRegion &selectedRegion,
                                 const ZoomInfo &zoomInfo)
{
   double sel0 = selectedRegion.t0();
   double sel1 = selectedRegion.t1();

   if (!track->GetSelected() && !track->IsSyncLockSelected())
      sel0 = sel1 = 0.0;

   track->Draw(dc, rect, SelectedRegion(sel0, sel1), zoomInfo);
}

void TrackArtist::DrawTimeTrack(const TimeTrack *track,
                                wxDC & dc,
                                const wxRect & rect,
                                const ZoomInfo &zoomInfo)
{
   track->Draw(dc, rect, zoomInfo);
   wxRect envRect = rect;
   envRect.height -= 2;
   double lower = track->GetRangeLower(), upper = track->GetRangeUpper();
   if(track->GetDisplayLog()) {
      // MB: silly way to undo the work of GetWaveYPos while still getting a logarithmic scale
      lower = LINEAR_TO_DB(std::max(1.0e-7, lower)) / mdBrange + 1.0;
      upper = LINEAR_TO_DB(std::max(1.0e-7, upper)) / mdBrange + 1.0;
   }
   track->GetEnvelope()->DrawPoints(dc, envRect, zoomInfo,
               track->GetDisplayLog(), mdBrange, lower, upper);
}

void TrackArtist::UpdatePrefs()
{
   mdBrange = gPrefs->Read(ENV_DB_KEY, mdBrange);
   mShowClipping = gPrefs->Read(wxT("/GUI/ShowClipping"), mShowClipping);
}

// Draws the sync-lock bitmap, tiled; always draws stationary relative to the DC
//
// AWD: now that the tiles don't link together, we're drawing a tilted grid, at
// two steps down for every one across. This creates a pattern that repeats in
// 5-step by 5-step boxes. Because we're only drawing in 5/25 possible positions
// we have a grid spacing somewhat smaller than the image dimensions. Thus we
// acheive lower density than with a square grid and eliminate edge cases where
// no tiles are displayed.
//
// The pattern draws in tiles at (0,0), (2,1), (4,2), (1,3), and (3,4) in each
// 5x5 box.
//
// There may be a better way to do this, or a more appealing pattern.
void TrackArtist::DrawSyncLockTiles(wxDC *dc, wxRect rect)
{
   wxBitmap syncLockBitmap(theTheme.Image(bmpSyncLockSelTile));

   // Grid spacing is a bit smaller than actual image size
   int gridW = syncLockBitmap.GetWidth() - 6;
   int gridH = syncLockBitmap.GetHeight() - 8;

   // Horizontal position within the grid, modulo its period
   int blockX = (rect.x / gridW) % 5;

   // Amount to offset drawing of first column
   int xOffset = rect.x % gridW;
   if (xOffset < 0) xOffset += gridW;

   // Check if we're missing an extra column to the left (this can happen
   // because the tiles are bigger than the grid spacing)
   bool extraCol = false;
   if (syncLockBitmap.GetWidth() - gridW > xOffset) {
      extraCol = true;
      xOffset += gridW;
      blockX = (blockX - 1) % 5;
   }
   // Make sure blockX is non-negative
   if (blockX < 0) blockX += 5;

   int xx = 0;
   while (xx < rect.width) {
      int width = syncLockBitmap.GetWidth() - xOffset;
      if (xx + width > rect.width)
         width = rect.width - xx;

      //
      // Draw each row in this column
      //

      // Vertical position in the grid, modulo its period
      int blockY = (rect.y / gridH) % 5;

      // Amount to offset drawing of first row
      int yOffset = rect.y % gridH;
      if (yOffset < 0) yOffset += gridH;

      // Check if we're missing an extra row on top (this can happen because
      // the tiles are bigger than the grid spacing)
      bool extraRow = false;
      if (syncLockBitmap.GetHeight() - gridH > yOffset) {
         extraRow = true;
         yOffset += gridH;
         blockY = (blockY - 1) % 5;
      }
      // Make sure blockY is non-negative
      if (blockY < 0) blockY += 5;

      int yy = 0;
      while (yy < rect.height)
      {
         int height = syncLockBitmap.GetHeight() - yOffset;
         if (yy + height > rect.height)
            height = rect.height - yy;

         // AWD: draw blocks according to our pattern
         if ((blockX == 0 && blockY == 0) || (blockX == 2 && blockY == 1) ||
             (blockX == 4 && blockY == 2) || (blockX == 1 && blockY == 3) ||
             (blockX == 3 && blockY == 4))
         {

            // Do we need to get a sub-bitmap?
            if (width != syncLockBitmap.GetWidth() || height != syncLockBitmap.GetHeight()) {
               wxBitmap subSyncLockBitmap =
                  syncLockBitmap.GetSubBitmap(wxRect(xOffset, yOffset, width, height));
               dc->DrawBitmap(subSyncLockBitmap, rect.x + xx, rect.y + yy, true);
            }
            else {
               dc->DrawBitmap(syncLockBitmap, rect.x + xx, rect.y + yy, true);
            }
         }

         // Updates for next row
         if (extraRow) {
            // Second offset row, still at y = 0; no more extra rows
            yOffset -= gridH;
            extraRow = false;
         }
         else {
            // Move on in y, no more offset rows
            yy += gridH - yOffset;
            yOffset = 0;
         }
         blockY = (blockY + 1) % 5;
      }

      // Updates for next column
      if (extraCol) {
         // Second offset column, still at x = 0; no more extra columns
         xOffset -= gridW;
         extraCol = false;
      }
      else {
         // Move on in x, no more offset rows
         xx += gridW - xOffset;
         xOffset = 0;
      }
      blockX = (blockX + 1) % 5;
   }
}

void TrackArtist::DrawBackgroundWithSelection(wxDC *dc, const wxRect &rect,
   const Track *track, wxBrush &selBrush, wxBrush &unselBrush,
   const SelectedRegion &selectedRegion, const ZoomInfo &zoomInfo)
{
   //MM: Draw background. We should optimize that a bit more.
   //AWD: "+ 1.5" and "+ 2.5" throughout match code in
   //AdornedRulerPanel::DoDrawSelection() and make selection line up with ruler.
   //I don't know if/why this is correct.

   const double sel0 = selectedRegion.t0();
   const double sel1 = selectedRegion.t1();

   dc->SetPen(*wxTRANSPARENT_PEN);
   if (track->GetSelected() || track->IsSyncLockSelected())
   {
      // Rectangles before, within, after the selction
      wxRect before = rect;
      wxRect within = rect;
      wxRect after = rect;

      before.width = (int)(zoomInfo.TimeToPosition(sel0) + 2);
      if (before.GetRight() > rect.GetRight()) {
         before.width = rect.width;
      }

      if (before.width > 0) {
         dc->SetBrush(unselBrush);
         dc->DrawRectangle(before);

         within.x = 1 + before.GetRight();
      }
      within.width = rect.x + (int)(zoomInfo.TimeToPosition(sel1) + 2) - within.x;

      if (within.GetRight() > rect.GetRight()) {
         within.width = 1 + rect.GetRight() - within.x;
      }

      if (within.width > 0) {
         if (track->GetSelected()) {
            dc->SetBrush(selBrush);
            dc->DrawRectangle(within);
         }
         else {
            // Per condition above, track must be sync-lock selected
            dc->SetBrush(unselBrush);
            dc->DrawRectangle(within);
            DrawSyncLockTiles(dc, within);
         }

         after.x = 1 + within.GetRight();
      }
      else {
         // `within` not drawn; start where it would have gone
         after.x = within.x;
      }

      after.width = 1 + rect.GetRight() - after.x;
      if (after.width > 0) {
         dc->SetBrush(unselBrush);
         dc->DrawRectangle(after);
      }
   }
   else
   {
      // Track not selected; just draw background
      dc->SetBrush(unselBrush);
      dc->DrawRectangle(rect);
   }
}

