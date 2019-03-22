/**********************************************************************

Audacity: A Digital Audio Editor

NoteTrackVZoomHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h" // for USE_* macros
#ifdef USE_MIDI
#include "NoteTrackVZoomHandle.h"

#include "../../../../Experimental.h"

#include "NoteTrackVRulerControls.h"

#include "../../../../HitTestResult.h"
#include "../../../../NoteTrack.h"
#include "../../../../Project.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../widgets/PopupMenuTable.h"
#include "../../../../../images/Cursors.h"
#include "../../../../Prefs.h"

#include <wx/event.h>

namespace
{

   struct InitMenuData
   {
   public:
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

void NoteTrackVZoomHandle::Enter(bool)
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
      _("Click to vertically zoom in. Shift-click to zoom out. Drag to specify a zoom region.") :
      _("Right-click for menu.");

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
   auto pTrack = pProject->GetTracks()->Lock(mpTrack);
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
(const TrackPanelMouseState &st, const AudacityProject *)
{
   return HitPreview(st.state);
}

// Note that these can be with or without spectrum view which
// adds a constant.
//const int kZoom1to1 = 1;
//const int kZoomTimes2 = 2;
//const int kZoomDiv2 = 3;
//const int kZoomHalfWave = 4;
//const int kZoomInByDrag = 5;
const int kZoomIn = 6;
const int kZoomOut = 7;
const int kZoomReset = 8;

enum {
   OnZoomFitVerticalID = 20000,
   OnZoomResetID,
   OnZoomDiv2ID,
   OnZoomTimes2ID,
   OnZoomHalfWaveID,
   OnZoomInVerticalID,
   OnZoomOutVerticalID,

   // Reserve an ample block of ids for waveform scale types
   OnFirstWaveformScaleID,
   OnLastWaveformScaleID = OnFirstWaveformScaleID + 9,

   // Reserve an ample block of ids for spectrum scale types
   OnFirstSpectrumScaleID,
   OnLastSpectrumScaleID = OnFirstSpectrumScaleID + 19,
};
///////////////////////////////////////////////////////////////////////////////
// Table class

class NoteTrackVRulerMenuTable : public PopupMenuTable
{
   NoteTrackVRulerMenuTable(){};
   virtual ~NoteTrackVRulerMenuTable() {}
   DECLARE_POPUP_MENU(NoteTrackVRulerMenuTable);

public:
   static NoteTrackVRulerMenuTable &Instance();

protected:
   InitMenuData *mpData {};
   void OnZoom( int iZoomCode );
// void OnZoomFitVertical(wxCommandEvent&){ OnZoom( kZoom1to1 );};
   void OnZoomReset(wxCommandEvent&){ OnZoom( kZoomReset );};
// void OnZoomDiv2Vertical(wxCommandEvent&){ OnZoom( kZoomDiv2 );};
// void OnZoomTimes2Vertical(wxCommandEvent&){ OnZoom( kZoomTimes2 );};
// void OnZoomHalfWave(wxCommandEvent&){ OnZoom( kZoomHalfWave );};
   void OnZoomInVertical(wxCommandEvent&){ OnZoom( kZoomIn );};
   void OnZoomOutVertical(wxCommandEvent&){ OnZoom( kZoomOut );};

private:
   void DestroyMenu() override
   {
      mpData = nullptr;
   }

   virtual void InitMenu(Menu *pMenu, void *pUserData) override;

   void OnWaveformScaleType(wxCommandEvent &evt);
};

NoteTrackVRulerMenuTable &NoteTrackVRulerMenuTable::Instance()
{
   static NoteTrackVRulerMenuTable instance;
   return instance;
}

void NoteTrackVRulerMenuTable::InitMenu(Menu *WXUNUSED(pMenu), void *pUserData)
{
   mpData = static_cast<InitMenuData*>(pUserData);
}

void NoteTrackVRulerMenuTable::OnZoom( int iZoomCode ){
   switch( iZoomCode ){
   case kZoomReset:
      mpData->pTrack->SetBottomNote(0);
      mpData->pTrack->SetPitchHeight(mpData->rect.height, 1);
      break;
   case kZoomIn:
      mpData->pTrack->ZoomIn(mpData->rect, mpData->yy);
      break;
   case kZoomOut:
      mpData->pTrack->ZoomOut(mpData->rect, mpData->yy);
      break;

   }
}


BEGIN_POPUP_MENU(NoteTrackVRulerMenuTable)

   POPUP_MENU_ITEM(OnZoomResetID,      _("Zoom Reset\tShift-Right-Click"), OnZoomReset)

   POPUP_MENU_SEPARATOR()
   POPUP_MENU_ITEM(OnZoomInVerticalID,  _("Zoom In\tLeft-Click/Left-Drag"),  OnZoomInVertical)
   POPUP_MENU_ITEM(OnZoomOutVerticalID, _("Zoom Out\tShift-Left-Click"),     OnZoomOutVertical)

END_POPUP_MENU()



UIHandle::Result NoteTrackVZoomHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject,
 wxWindow *pParent)
{
   using namespace RefreshCode;
   auto pTrack = pProject->GetTracks()->Lock(mpTrack);
   if (!pTrack)
      return RefreshNone;

   const wxMouseEvent &event = evt.event;
   //const bool shiftDown = event.ShiftDown();
   const bool rightUp = event.RightUp();


   bool bVZoom;
   gPrefs->Read(wxT("/GUI/VerticalZooming"), &bVZoom, false);

   // Popup menu... 
   if (
       rightUp &&
       !(event.ShiftDown() || event.CmdDown()))
   {
      InitMenuData data {
         pTrack.get(), mRect, RefreshCode::RefreshNone, event.m_y
      };

      PopupMenuTable *const pTable =
          (PopupMenuTable *) &NoteTrackVRulerMenuTable::Instance();
      std::unique_ptr<PopupMenuTable::Menu>
         pMenu(PopupMenuTable::BuildMenu(pParent, pTable, &data));

      // Accelerators only if zooming enabled.
      if( !bVZoom )
      {
         wxMenuItemList & L = pMenu->GetMenuItems();
         // let's iterate over the list in STL syntax
         wxMenuItemList::iterator iter;
         for (iter = L.begin(); iter != L.end(); ++iter)
         {
             wxMenuItem *pItem = *iter;
             // Remove accelerator, if any.
             pItem->SetItemLabel( (pItem->GetItemLabel() + "\t" ).BeforeFirst('\t') );
         }
      }

      pParent->PopupMenu(pMenu.get(), event.m_x, event.m_y);

      return data.result;
   }

   bVZoom &= event.GetId() != kCaptureLostEventId;
   if( !bVZoom )
      return RefreshAll;

   if (IsDragZooming(mZoomStart, mZoomEnd)) {
      pTrack->ZoomTo(evt.rect, mZoomStart, mZoomEnd);
   }
   else if (event.ShiftDown() || event.RightUp()) {
      if (event.ShiftDown() && event.RightUp()) {
         // Zoom out completely
         pTrack->SetBottomNote(0);
         pTrack->SetPitchHeight(evt.rect.height, 1);
      } else {
         // Zoom out
         pTrack->ZoomOut(evt.rect, mZoomEnd);
      }
   }
   else {
      pTrack->ZoomIn(evt.rect, mZoomEnd);
   }

   mZoomEnd = mZoomStart = 0;
   pProject->ModifyState(true);

   return RefreshAll;
}

UIHandle::Result NoteTrackVZoomHandle::Cancel(AudacityProject *WXUNUSED(pProject))
{
   // Cancel is implemented!  And there is no initial state to restore,
   // so just return a code.
   return RefreshCode::RefreshAll;
}

void NoteTrackVZoomHandle::DrawExtras
(DrawingPass pass, wxDC * dc, const wxRegion &, const wxRect &panelRect)
{
   if (!mpTrack.lock()) //? TrackList::Lock()
      return;

   if ( pass == UIHandle::Cells &&
        IsDragZooming( mZoomStart, mZoomEnd ) )
      TrackVRulerControls::DrawZooming
         ( dc, mRect, panelRect, mZoomStart, mZoomEnd );
}

#endif
