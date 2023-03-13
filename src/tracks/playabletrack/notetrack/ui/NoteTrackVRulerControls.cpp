/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackVRulerControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/



#ifdef USE_MIDI
#include "NoteTrackVRulerControls.h"

#include "NoteTrackDisplayData.h"
#include "NoteTrackVZoomHandle.h"

#include "../../../../HitTestResult.h"
#include "NoteTrack.h"
#include "ProjectHistory.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelMouseEvent.h"

#include "AColor.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "../../../../widgets/LinearUpdater.h"
#include "../../../../widgets/RealFormat.h"
#include "../../../../widgets/Ruler.h"

#include <wx/dc.h>
#include <wx/event.h>

///////////////////////////////////////////////////////////////////////////////
NoteTrackVRulerControls::~NoteTrackVRulerControls()
{
}

std::vector<UIHandlePtr> NoteTrackVRulerControls::HitTest
(const TrackPanelMouseState &st,
 const AudacityProject *pProject)
{
   std::vector<UIHandlePtr> results;
   UIHandlePtr result;

   if ( st.state.GetX() <= st.rect.GetRight() - kGuard ) {
      auto track = std::static_pointer_cast<NoteTrack>(FindTrack());
      result = NoteTrackVZoomHandle::HitTest(
         mVZoomHandle, st.state, track, st.rect);
      if (result)
         results.push_back(result);
   }

   auto more = TrackVRulerControls::HitTest(st, pProject);
   std::copy(more.begin(), more.end(), std::back_inserter(results));

   return results;
}

unsigned NoteTrackVRulerControls::HandleWheelRotation
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   const wxMouseEvent &event = evt.event;

   if (!(event.ShiftDown() || event.CmdDown()))
      return RefreshNone;

   // Always stop propagation even if the ruler didn't change.  The ruler
   // is a narrow enough target.
   evt.event.Skip(false);

   const auto pTrack = FindTrack();
   if (!pTrack)
      return RefreshNone;

   auto steps = evt.steps;
   const auto nt = static_cast<NoteTrack*>(pTrack.get());

   if (event.CmdDown() && !event.ShiftDown()) {
      NoteTrackDisplayData data{ *nt, evt.rect };
      if (steps > 0)
         data.ZoomIn(evt.event.m_y);
      else
         data.ZoomOut(evt.event.m_y);
   } else if (!event.CmdDown() && event.ShiftDown()) {
      // Scroll some fixed number of notes, independent of zoom level or track height:
      static const int movement = 6; // 6 semitones is half an octave
      NoteTrackRange::Get(*nt).ShiftNoteRange((int) (steps * movement));
   } else {
      return RefreshNone;
   }

   ProjectHistory::Get( *pProject ).ModifyState(false);

   return RefreshCell | UpdateVRuler;
}

void NoteTrackVRulerControls::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect_, unsigned iPass )
{
   TrackVRulerControls::Draw( context, rect_, iPass );

   // Draw on a later pass like other vertical rulers,
   // although the bevel is done a little differently

   if ( iPass == TrackArtist::PassControls ) {
      // The note track draws a vertical keyboard to label pitches
      auto track = std::static_pointer_cast<NoteTrack>( FindTrack() );
      if ( !track )
         return;

      auto rect = rect_;
      --rect.width;
      --rect.height;

      bool highlight = false;
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
      highlight = rect.Contains(context.lastState.GetPosition());
#endif

      const auto artist = TrackArtist::Get( context );
      UpdateRuler(rect);

      auto dc = &context.dc;

      dc->SetPen(highlight ? AColor::uglyPen : *wxTRANSPARENT_PEN);
      dc->SetBrush(*wxWHITE_BRUSH);
      wxRect bev = rect;
      bev.x++;
      bev.width--;
      dc->DrawRectangle(bev);

      rect.y += 1;
      rect.height -= 1;

      NoteTrackDisplayData data{ *track, rect };

      wxPen hilitePen;
      hilitePen.SetColour(120, 120, 120);
      wxBrush blackKeyBrush;
      blackKeyBrush.SetColour(70, 70, 70);

      dc->SetBrush(blackKeyBrush);

      int fontSize = 10;
#ifdef __WXMSW__
      fontSize = 8;
#endif

      wxFont labelFont(fontSize, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL);
      dc->SetFont(labelFont);

      int octave = 0;
      int obottom = data.GetOctaveBottom(octave);
      int marg = data.GetNoteMargin();

      while (obottom >= rect.y) {
         dc->SetPen(*wxBLACK_PEN);
         for (int white = 0; white < 7; white++) {
            int pos = data.GetWhitePos(white);
            if (obottom - pos > rect.y + marg + 1 &&
                // don't draw too close to margin line -- it's annoying
                obottom - pos < rect.y + rect.height - marg - 3)
               AColor::Line(*dc, rect.x, obottom - pos,
                            rect.x + rect.width, obottom - pos);
         }
         wxRect br = rect;
         br.height = data.GetPitchHeight(1);
         br.x++;
         br.width = 17;
         for (int black = 0; black < 5; black++) {
            br.y = obottom - data.GetBlackPos(black);
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
         obottom = data.GetOctaveBottom(++octave);
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
}


void NoteTrackVRulerControls::UpdateRuler( const wxRect &rect )
{
   // The note track isn't drawing a ruler at all!
   // But it needs to!

   const auto nt = std::static_pointer_cast< NoteTrack >( FindTrack() );
   if (!nt)
      return;

   static Ruler ruler{
      LinearUpdater::Instance(), RealFormat::LinearInstance() };
   const auto vruler = &ruler;

   vruler->SetBounds(rect.x, rect.y, rect.x + 1, rect.y + rect.height-1);
   vruler->SetOrientation(wxVERTICAL);

   vruler->GetMaxSize( &nt->vrulerSize.first, &nt->vrulerSize.second );
}
#endif
