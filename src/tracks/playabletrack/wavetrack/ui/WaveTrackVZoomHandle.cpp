/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackVZoomHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "WaveTrackVZoomHandle.h"

#include "../../../ui/TrackVRulerControls.h"

#include "../../../../HitTestResult.h"
#include "Project.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../WaveTrack.h"
#include "../../../../../images/Cursors.h"

bool WaveTrackVZoomHandle::IsDragZooming(int zoomStart, int zoomEnd)
{
   const int DragThreshold = 3;// Anything over 3 pixels is a drag, else a click.
   bool bVZoom;
   gPrefs->Read(wxT("/GUI/VerticalZooming"), &bVZoom, false);
   return bVZoom && (abs(zoomEnd - zoomStart) > DragThreshold);
}

///////////////////////////////////////////////////////////////////////////////
// Table class

void WaveTrackVRulerMenuTable::InitUserData(void *pUserData)
{
   mpData = static_cast<InitMenuData*>(pUserData);
}


void WaveTrackVRulerMenuTable::OnZoom(
   WaveTrackViewConstants::ZoomActions iZoomCode )
{
   mpData->doZoom(
      &mpData->project, mpData->pTrack,
      iZoomCode, mpData->rect, mpData->yy, mpData->yy, false
   );

   using namespace RefreshCode;
   mpData->result = UpdateVRuler | RefreshAll;
}

void WaveTrackVRulerMenuTable::UpdatePrefs()
{
   // Because labels depend on advanced vertical zoom setting
   PopupMenuTable::Clear();
}

///////////////////////////////////////////////////////////////////////////////

HitTestPreview WaveTrackVZoomHandle::HitPreview(const wxMouseState &state)
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

UIHandle::Result WaveTrackVZoomHandle::DoDrag(
   const TrackPanelMouseEvent &evt, AudacityProject *pProject,
   const int zoomStart, int &zoomEnd)
{
   using namespace RefreshCode;

   const wxMouseEvent &event = evt.event;
   if ( event.RightIsDown() )
      return RefreshNone;
   zoomEnd = event.m_y;
   if (IsDragZooming( zoomStart, zoomEnd ))
      return RefreshAll;
   return RefreshNone;
}

UIHandle::Result WaveTrackVZoomHandle::DoRelease(
   const TrackPanelMouseEvent &evt, AudacityProject *pProject,
   wxWindow *pParent, WaveTrack *pTrack, const wxRect &rect,
   DoZoomFunction doZoom, PopupMenuTable &table,
   int zoomStart, int zoomEnd )
{
   using namespace RefreshCode;
   if (!pTrack)
      return RefreshNone;

   const wxMouseEvent &event = evt.event;
   const bool shiftDown = event.ShiftDown();
   const bool rightUp = event.RightUp();


   // Popup menu...
   using namespace WaveTrackViewConstants;
   if (
       rightUp &&
       !(event.ShiftDown() || event.CmdDown()))
   {
      WaveTrackVRulerMenuTable::InitMenuData data {
         *pProject,
         pTrack, rect, RefreshCode::RefreshNone, event.m_y, doZoom };

      auto pMenu = PopupMenuTable::BuildMenu( &table, &data );
      pMenu->Popup( *pParent, { event.m_x, event.m_y } );

      return data.result;
   }
   else{
      bool bVZoom;
      gPrefs->Read(wxT("/GUI/VerticalZooming"), &bVZoom, false);
      // Ignore Capture Lost event 
      bVZoom &= event.GetId() != kCaptureLostEventId;
      // shiftDown | rightUp | ZoomKind
      //    T      |    T    | 1to1
      //    T      |    F    | Out
      //    F      |    -    | In
      if( bVZoom ) {
         if( shiftDown )
            zoomStart = zoomEnd;
         doZoom(pProject, pTrack,
            shiftDown
               ? (rightUp ? kZoom1to1 : kZoomOut)
               : kZoomIn,
            rect, zoomStart, zoomEnd, !shiftDown);
      }
   }

   return UpdateVRuler | RefreshAll;
}

void WaveTrackVZoomHandle::DoDraw(
   TrackPanelDrawingContext &context,
   const wxRect &rect, unsigned iPass, const int zoomStart, const int zoomEnd )
{
   if ( iPass == TrackArtist::PassZooming ) {
      if ( IsDragZooming( zoomStart, zoomEnd ) )
         TrackVRulerControls::DrawZooming
            ( context, rect, zoomStart, zoomEnd );
   }
}

wxRect WaveTrackVZoomHandle::DoDrawingArea(
   const wxRect &rect, const wxRect &panelRect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassZooming )
      return TrackVRulerControls::ZoomingArea( rect, panelRect );
   else
      return rect;
}
