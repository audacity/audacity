/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackVZoomHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#ifdef USE_MIDI
#include "NoteTrackVZoomHandle.h"

#include "../../../ui/TrackVRulerControls.h"

#include "../../../../HitTestResult.h"
#include "../../../../NoteTrack.h"
#include "Project.h"
#include "../../../../ProjectHistory.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../widgets/PopupMenuTable.h"
#include "../../../../../images/Cursors.h"
#include "Prefs.h"

#include <wx/event.h>

namespace
{

   struct InitMenuData
   {
   public:
      AudacityProject &project;
      NoteTrack *pTrack;
      wxRect rect;
      unsigned result;
      int yy;
   };

   bool IsDragZooming(int zoomStart, int zoomEnd)
   {
      const int DragThreshold = 3;// Anything over 3 pixels is a drag, else a click.
      bool bVZoom;
      gPrefs->Read(wxT("/GUI/VerticalZooming"), &bVZoom, false);
      return bVZoom && (abs(zoomEnd - zoomStart) > DragThreshold);
   }

}

///////////////////////////////////////////////////////////////////////////////

NoteTrackVZoomHandle::NoteTrackVZoomHandle
(const std::shared_ptr<NoteTrack> &pTrack, const wxRect &rect, int y)
   : mpTrack{ pTrack } , mZoomStart(y), mZoomEnd(y), mRect(rect)
{
}

void NoteTrackVZoomHandle::Enter(bool, AudacityProject *)
{
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
   mChangeHighlight = RefreshCode::RefreshCell;
#endif
}

HitTestPreview NoteTrackVZoomHandle::HitPreview(const wxMouseState &state)
{
   static auto zoomInCursor =
      ::MakeCursor(wxCURSOR_MAGNIFIER, ZoomInCursorXpm, 19, 15);
   static auto zoomOutCursor =
      ::MakeCursor(wxCURSOR_MAGNIFIER, ZoomOutCursorXpm, 19, 15);
   static  wxCursor arrowCursor{ wxCURSOR_ARROW };

   bool bVZoom;
   gPrefs->Read(wxT("/GUI/VerticalZooming"), &bVZoom, false);
   bVZoom &= !state.RightIsDown();
   const auto message = bVZoom ? 
      XO("Click to vertically zoom in. Shift-click to zoom out. Drag to specify a zoom region.") :
      XO("Right-click for menu.");

   return {
      message,
      bVZoom ? (state.ShiftDown() ? &*zoomOutCursor : &*zoomInCursor) : &arrowCursor
      // , message
   };
}

UIHandlePtr NoteTrackVZoomHandle::HitTest
(std::weak_ptr<NoteTrackVZoomHandle> &holder,
 const wxMouseState &state,
 const std::shared_ptr<NoteTrack> &pTrack, const wxRect &rect)
{
   if (pTrack) {
      auto result = std::make_shared<NoteTrackVZoomHandle>(
         pTrack, rect, state.m_y);
      result = AssignUIHandlePtr(holder, result);
      return result;
   }
   return {};
}

NoteTrackVZoomHandle::~NoteTrackVZoomHandle()
{
}

bool NoteTrackVZoomHandle::HandlesRightClick()
{
   return true;
}

UIHandle::Result NoteTrackVZoomHandle::Click
(const TrackPanelMouseEvent &, AudacityProject *)
{
   // change note track to zoom like audio track
   //          mpTrack->StartVScroll();

   return RefreshCode::RefreshNone;
}

UIHandle::Result NoteTrackVZoomHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   auto pTrack = TrackList::Get( *pProject ).Lock(mpTrack);
   if (!pTrack)
      return Cancelled;

   const wxMouseEvent &event = evt.event;
   mZoomEnd = event.m_y;
   if (IsDragZooming(mZoomStart, mZoomEnd)) {
      // changed Note track to work like audio track
      //         pTrack->VScroll(mZoomStart, mZoomEnd);
      return RefreshAll;
   }
   return RefreshNone;
}

HitTestPreview NoteTrackVZoomHandle::Preview
(const TrackPanelMouseState &st, AudacityProject *)
{
   return HitPreview(st.state);
}

enum {
   OnZoomFitVerticalID = 20000,
   OnZoomResetID,
   OnZoomDiv2ID,
   OnZoomTimes2ID,
   OnZoomHalfWaveID,
   OnZoomInVerticalID,
   OnZoomOutVerticalID,

   OnZoomMaxID,

   OnUpOctaveID,
   OnDownOctaveID,
};
///////////////////////////////////////////////////////////////////////////////
// Table class

class NoteTrackVRulerMenuTable
  : public PopupMenuTable
  , private PrefsListener
{
   NoteTrackVRulerMenuTable()
      : PopupMenuTable{ "NoteTrackVRuler" }
   {};
   virtual ~NoteTrackVRulerMenuTable() {}
   DECLARE_POPUP_MENU(NoteTrackVRulerMenuTable);

public:
   static NoteTrackVRulerMenuTable &Instance();

protected:
   enum {
// Note that these can be with or without spectrum view which
// adds a constant.
//const int kZoom1to1 = 1;
//const int kZoomTimes2 = 2;
//const int kZoomDiv2 = 3;
//const int kZoomHalfWave = 4;
//const int kZoomInByDrag = 5;
      kZoomIn = 6,
      kZoomOut = 7,
      kZoomReset = 8,
      kZoomMax = 9,
      kUpOctave = 10,
      kDownOctave = 11,
   };

   InitMenuData *mpData {};
   void OnZoom( int iZoomCode );
// void OnZoomFitVertical(wxCommandEvent&){ OnZoom( kZoom1to1 );};
   void OnZoomReset(wxCommandEvent&){ OnZoom( kZoomReset );};
// void OnZoomDiv2Vertical(wxCommandEvent&){ OnZoom( kZoomDiv2 );};
// void OnZoomTimes2Vertical(wxCommandEvent&){ OnZoom( kZoomTimes2 );};
// void OnZoomHalfWave(wxCommandEvent&){ OnZoom( kZoomHalfWave );};
   void OnZoomInVertical(wxCommandEvent&){ OnZoom( kZoomIn );};
   void OnZoomOutVertical(wxCommandEvent&){ OnZoom( kZoomOut );};
   void OnZoomMax(wxCommandEvent&){ OnZoom( kZoomMax );};
   void OnUpOctave(wxCommandEvent&){ OnZoom( kUpOctave );};
   void OnDownOctave(wxCommandEvent&){ OnZoom( kDownOctave );};

private:
   void InitUserData(void *pUserData) override;

   void UpdatePrefs() override
   {
      // Because labels depend on advanced vertical zoom setting
      PopupMenuTable::Clear();
   }
};

NoteTrackVRulerMenuTable &NoteTrackVRulerMenuTable::Instance()
{
   static NoteTrackVRulerMenuTable instance;
   return instance;
}

void NoteTrackVRulerMenuTable::InitUserData(void *pUserData)
{
   mpData = static_cast<InitMenuData*>(pUserData);
}

void NoteTrackVRulerMenuTable::OnZoom( int iZoomCode ){
   switch( iZoomCode ){
   case kZoomReset:
      mpData->pTrack->ZoomAllNotes();
      break;
   case kZoomIn:
      mpData->pTrack->ZoomIn(mpData->rect, mpData->yy);
      break;
   case kZoomOut:
      mpData->pTrack->ZoomOut(mpData->rect, mpData->yy);
      break;
   case kZoomMax:
      mpData->pTrack->ZoomMaxExtent();
      break;
   case kUpOctave:
      mpData->pTrack->ShiftNoteRange(12);
      break;
   case kDownOctave:
      mpData->pTrack->ShiftNoteRange(-12);
      break;
   }
   AudacityProject *const project = &mpData->project;
   ProjectHistory::Get( *project ).ModifyState(false);
   using namespace RefreshCode;
   mpData->result = UpdateVRuler | RefreshAll;
}


BEGIN_POPUP_MENU(NoteTrackVRulerMenuTable)

   // Accelerators only if zooming enabled.
   bool bVZoom;
   gPrefs->Read(wxT("/GUI/VerticalZooming"), &bVZoom, false);

   BeginSection( "Zoom" );
      BeginSection( "Basic" );
         AppendItem( "Reset", OnZoomResetID,
            MakeLabel( XXO("Zoom Reset"), bVZoom, XXO("Shift-Right-Click")),
            POPUP_MENU_FN( OnZoomReset ) );
         AppendItem( "Max", OnZoomMaxID,        XXO("Max Zoom"), POPUP_MENU_FN( OnZoomMax ) );
      EndSection();

      BeginSection( "InOut" );
         AppendItem( "In", OnZoomInVerticalID,
            MakeLabel( XXO("Zoom In"), bVZoom, XXO("Left-Click/Left-Drag") ),
            POPUP_MENU_FN( OnZoomInVertical ) );
         AppendItem( "Out", OnZoomOutVerticalID,
            MakeLabel( XXO("Zoom Out"), bVZoom, XXO("Shift-Left-Click") ),
            POPUP_MENU_FN( OnZoomOutVertical ) );
      EndSection();
   EndSection();

   BeginSection( "Pan" );
      BeginSection( "Octaves" );
         AppendItem( "Up", OnUpOctaveID,   XXO("Up &Octave"),   POPUP_MENU_FN( OnUpOctave) );
         AppendItem( "Down", OnDownOctaveID, XXO("Down Octa&ve"), POPUP_MENU_FN( OnDownOctave ) );
      EndSection();
   EndSection();

END_POPUP_MENU()



UIHandle::Result NoteTrackVZoomHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject,
 wxWindow *pParent)
{
   using namespace RefreshCode;
   auto pTrack = TrackList::Get( *pProject ).Lock(mpTrack);
   if (!pTrack)
      return RefreshNone;

   const wxMouseEvent &event = evt.event;
   //const bool shiftDown = event.ShiftDown();
   const bool rightUp = event.RightUp();


   // Popup menu... 
   if (
       rightUp &&
       !(event.ShiftDown() || event.CmdDown()))
   {
      InitMenuData data {
         *pProject, pTrack.get(), mRect, RefreshNone, event.m_y
      };

      PopupMenuTable *const pTable =
          (PopupMenuTable *) &NoteTrackVRulerMenuTable::Instance();
      auto pMenu = PopupMenuTable::BuildMenu(pTable, &data);

      pMenu->Popup( *pParent, { event.m_x, event.m_y } );

      return data.result;
   }

   bool bVZoom;
   gPrefs->Read(wxT("/GUI/VerticalZooming"), &bVZoom, false);
   bVZoom &= event.GetId() != kCaptureLostEventId;
   if( !bVZoom )
      return RefreshAll;

   if (IsDragZooming(mZoomStart, mZoomEnd)) {
      pTrack->ZoomTo(evt.rect, mZoomStart, mZoomEnd);
   }
   else if (event.ShiftDown() || event.RightUp()) {
      if (event.ShiftDown() && event.RightUp()) {
         auto oldBotNote = pTrack->GetBottomNote();
         auto oldTopNote = pTrack->GetTopNote();
         // Zoom out to show all notes
         pTrack->ZoomAllNotes();
         if (pTrack->GetBottomNote() == oldBotNote &&
               pTrack->GetTopNote() == oldTopNote) {
            // However if we are already showing all notes, zoom out further
            pTrack->ZoomMaxExtent();
         }
      } else {
         // Zoom out
         pTrack->ZoomOut(evt.rect, mZoomEnd);
      }
   }
   else {
      pTrack->ZoomIn(evt.rect, mZoomEnd);
   }

   mZoomEnd = mZoomStart = 0;
   ProjectHistory::Get( *pProject ).ModifyState(false);

   return RefreshAll;
}

UIHandle::Result NoteTrackVZoomHandle::Cancel(AudacityProject *WXUNUSED(pProject))
{
   // Cancel is implemented!  And there is no initial state to restore,
   // so just return a code.
   return RefreshCode::RefreshAll;
}

void NoteTrackVZoomHandle::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassZooming ) {
      if (!mpTrack.lock()) //? TrackList::Lock()
         return;
      
      if ( IsDragZooming( mZoomStart, mZoomEnd ) )
         TrackVRulerControls::DrawZooming
            ( context, rect, mZoomStart, mZoomEnd );
   }
}

wxRect NoteTrackVZoomHandle::DrawingArea(
   TrackPanelDrawingContext &,
   const wxRect &rect, const wxRect &panelRect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassZooming )
      return TrackVRulerControls::ZoomingArea( rect, panelRect );
   else
      return rect;
}

#endif
