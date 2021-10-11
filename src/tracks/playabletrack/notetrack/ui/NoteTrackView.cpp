/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackView.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "NoteTrackView.h"

#ifdef USE_MIDI
#include "../lib-src/header-substitutes/allegro.h"

#include "NoteTrackVRulerControls.h"
#include "../../../../NoteTrack.h"

#include "../../../../AColor.h"
#include "../../../../AllThemeResources.h"
#include "../../../../HitTestResult.h"
#include "../../../../Theme.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "ViewInfo.h"
#include "../../../ui/SelectHandle.h"
#include "StretchHandle.h"
#include "NoteTrackAffordanceControls.h"

#include <wx/dc.h>

NoteTrackView::NoteTrackView( const std::shared_ptr<Track> &pTrack )
   : CommonTrackView{ pTrack }
{
}

NoteTrackView::~NoteTrackView()
{
}

std::vector<UIHandlePtr> NoteTrackView::DetailedHitTest
(const TrackPanelMouseState &WXUNUSED(state),
 const AudacityProject *WXUNUSED(pProject), int, bool )
{
   // Eligible for stretch?
   UIHandlePtr result;
   std::vector<UIHandlePtr> results;
#ifdef USE_MIDI
#ifdef EXPERIMENTAL_MIDI_STRETCHING
   result = StretchHandle::HitTest(
      mStretchHandle, state, pProject, Pointer<NoteTrack>(this) );
   if (result)
      results.push_back(result);
#endif
#endif

   return results;
}

using DoGetNoteTrackView = DoGetView::Override< NoteTrack >;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(DoGetNoteTrackView) {
   return [](NoteTrack &track) {
      return std::make_shared<NoteTrackView>( track.SharedPointer() );
   };
}

std::shared_ptr<TrackVRulerControls> NoteTrackView::DoGetVRulerControls()
{
   return
      std::make_shared<NoteTrackVRulerControls>( shared_from_this() );
}

#define TIME_TO_X(t) (zoomInfo.TimeToPosition((t), rect.x))
#define X_TO_TIME(xx) (zoomInfo.PositionToTime((xx), rect.x))

std::shared_ptr<CommonTrackCell> NoteTrackView::GetAffordanceControls()
{
   if (mpAffordanceCellControl == nullptr)
   {
      mpAffordanceCellControl = std::make_shared<NoteTrackAffordanceControls>(DoFindTrack());
   }
   return mpAffordanceCellControl;
}

namespace {

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
void DrawNoteBackground(TrackPanelDrawingContext &context,
                                     const NoteTrack *track,
                                     const wxRect &rect, const wxRect &sel,
                                     const wxBrush &wb, const wxPen &wp,
                                     const wxBrush &bb, const wxPen &bp,
                                     const wxPen &mp)
{
   auto &dc = context.dc;
   const auto artist = TrackArtist::Get( context );
   const auto &zoomInfo = *artist->pZoomInfo;

   dc.SetBrush(wb);
   dc.SetPen(wp);
#ifndef EXPERIMENTAL_NOTETRACK_OVERLAY
   dc.DrawRectangle(sel); // fill rectangle with white keys background
#endif

   int left = TIME_TO_X(track->GetOffset());
   if (left < sel.x) left = sel.x; // clip on left

   int right = TIME_TO_X(track->GetOffset() + track->GetSeq().get_real_dur());
   if (right > sel.x + sel.width) right = sel.x + sel.width; // clip on right

   // need overlap between MIDI data and the background region
   if (left >= right) return;

   NoteTrackDisplayData data{ track, rect };
   dc.SetBrush(bb);
   int octave = 0;
   // obottom is the window coordinate of octave divider line
   int obottom = data.GetOctaveBottom(octave);
   // eOffset is for the line between E and F; there's another line
   // between B and C, hence the offset of 2 for two line thicknesses
   int eOffset = data.GetPitchHeight(5) + 2;
   while (obottom > rect.y + data.GetNoteMargin() + 3) {
      // draw a black line separating octaves if this octave bottom is visible
      if (obottom < rect.y + rect.height - data.GetNoteMargin()) {
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
      br.height = data.GetPitchHeight(1);
      for (int black = 0; black < 5; black++) {
         br.y = obottom - data.GetBlackPos(black);
         if (br.y > rect.y && br.y + br.height < rect.y + rect.height) {
            dc.DrawRectangle(br); // draw each black key background stripe
         }
      }
      obottom = data.GetOctaveBottom(++octave);
   }

   // draw bar lines
   Alg_seq_ptr seq = &track->GetSeq();
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
void DrawNoteTrack(TrackPanelDrawingContext &context,
                                const NoteTrack *track,
                                const wxRect & rect,
                                bool muted,
                                bool selected)
{
   auto &dc = context.dc;
   const auto artist = TrackArtist::Get( context );
   const auto &selectedRegion = *artist->pSelectedRegion;
   const auto &zoomInfo = *artist->pZoomInfo;

   SonifyBeginNoteBackground();
   double sel0 = selectedRegion.t0();
   double sel1 = selectedRegion.t1();

   const double h = X_TO_TIME(rect.x);
   const double h1 = X_TO_TIME(rect.x + rect.width);

   Alg_seq_ptr seq = &track->GetSeq();

   if (!track->GetSelected())
      sel0 = sel1 = 0.0;

   NoteTrackDisplayData data{ track, rect };

   // reserve 1/2 note height at top and bottom of track for
   // out-of-bounds notes
   int numPitches = (rect.height) / data.GetPitchHeight(1);
   if (numPitches < 0) numPitches = 0; // cannot be negative

   // Background comes in 4 colors, that are now themed.
   //   214, 214,214 -- unselected white keys
   //   192,192,192 -- black keys
   //   170,170,170 -- bar lines
   //   165,165,190 -- selected white keys

   wxPen blackStripePen;
   blackStripePen.SetColour(theTheme.Colour( clrMidiZebra));
   wxBrush blackStripeBrush;
   blackStripeBrush.SetColour(theTheme.Colour( clrMidiZebra));
   wxPen barLinePen;
   barLinePen.SetColour(theTheme.Colour( clrMidiLines));

   const auto &blankBrush = artist->blankBrush;
   const auto &blankPen = artist->blankPen;
   DrawNoteBackground(context, track, rect, rect, blankBrush, blankPen,
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
   selectedWhiteKeyBrush.SetColour(theTheme.Colour( clrSelected ));
   // Then, the black keys and octave stripes, as smaller rectangles
   wxPen selectedBlackKeyPen;
   selectedBlackKeyPen.SetColour(theTheme.Colour( clrMidiZebra));
   wxBrush selectedBlackKeyBrush;
   selectedBlackKeyBrush.SetColour(theTheme.Colour( clrMidiZebra));
   wxPen selectedBarLinePen;
   selectedBarLinePen.SetColour(theTheme.Colour( clrMidiLines));

   DrawNoteBackground(context, track, rect, selBG,
                      selectedWhiteKeyBrush, selectedWhiteKeyPen,
                      selectedBlackKeyBrush, selectedBlackKeyPen,
                      selectedBarLinePen);
   SonifyEndNoteBackground();
   SonifyBeginNoteForeground();
   int marg = data.GetNoteMargin();

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
         if (track->IsVisibleChan(evt->chan)) {
            double xx = note->time + track->GetOffset();
            double x1 = xx + note->dur;
            if (xx < h1 && x1 > h) { // omit if outside box
               const char *shape = NULL;
               if (note->loud > 0.0 || 0 == (shape = IsShape(note))) {
                  wxRect nr; // "note rectangle"
                  nr.y = data.PitchToY(note->pitch);
                  nr.height = data.GetPitchHeight(1);

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
                        if (data.GetPitchHeight(1) > 2) {
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
                  int yy = data.PitchToY(note->pitch);
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
                              linethick, wxPENSTYLE_SOLID));
                  }
                  if (shape != line) {
                     fillcolor = LookupIntAttribute(note, fillcolori, -1);
                     fillflag = LookupLogicalAttribute(note, filll, false);

                     if (fillcolor != -1)
                        dc.SetBrush(wxBrush(wxColour(RED(fillcolor),
                              GREEN(fillcolor),
                              BLUE(fillcolor)),
                              wxBRUSHSTYLE_SOLID));
                     if (!fillflag) dc.SetBrush(*wxTRANSPARENT_BRUSH);
                  }
                  int y1 = data.PitchToY(LookupRealAttribute(note, y1r, note->pitch));
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
                     points[2].y = data.PitchToY(LookupRealAttribute(note, y2r, note->pitch));
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
                     points[2].y = data.PitchToY(LookupRealAttribute(note, y2r, note->pitch));
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
                        points[n].y = data.PitchToY(yn);
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
                     wxfont.SetFamily(font == roman ? wxFONTFAMILY_ROMAN :
                        (font == swiss ? wxFONTFAMILY_SWISS :
                           (font == modern ? wxFONTFAMILY_MODERN : wxFONTFAMILY_DEFAULT)));
                     wxfont.SetStyle(wxFONTSTYLE_NORMAL);
                     wxfont.SetWeight(weight == bold ? wxFONTWEIGHT_BOLD : wxFONTWEIGHT_NORMAL);
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
                              1, wxPENSTYLE_SOLID));
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
      TrackArt::DrawNegativeOffsetTrackArrows( context, rect );
   }

   //draw clip edges
   {
      int left = TIME_TO_X(track->GetOffset());
      int right = TIME_TO_X(track->GetOffset() + track->GetSeq().get_real_dur());

      TrackArt::DrawClipEdges(dc, wxRect(left, rect.GetTop(), right - left + 1, rect.GetHeight()), selected);
   }

   dc.DestroyClippingRegion();
   SonifyEndNoteForeground();
}

}

void NoteTrackView::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassTracks ) {
      const auto nt = std::static_pointer_cast<const NoteTrack>(
         FindTrack()->SubstitutePendingChangedTrack());
      bool muted = false;
#ifdef EXPERIMENTAL_MIDI_OUT
      const auto artist = TrackArtist::Get( context );
      const auto hasSolo = artist->hasSolo;
      muted = (hasSolo || nt->GetMute()) && !nt->GetSolo();
#endif

#ifdef EXPERIMENTAL_NOTETRACK_OVERLAY
      TrackArt::DrawBackgroundWithSelection(context, rect, nt.get(), AColor::labelSelectedBrush, AColor::labelUnselectedBrush);
#endif
      bool selected{ false };
      if (auto affordance = std::dynamic_pointer_cast<NoteTrackAffordanceControls>(GetAffordanceControls()))
      {
         selected = affordance->IsSelected();
      }

      DrawNoteTrack(context, nt.get(), rect, muted, selected);
   }
   CommonTrackView::Draw( context, rect, iPass );
}
#endif
