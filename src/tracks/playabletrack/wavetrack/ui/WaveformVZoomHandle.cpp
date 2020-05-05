/**********************************************************************

Audacity: A Digital Audio Editor

WaveformVZoomHandle.cpp

Paul Licameli split from WaveTrackVZoomHandle.cpp

**********************************************************************/


#include "WaveformVZoomHandle.h"

#include "WaveTrackVZoomHandle.h"

#include "../../../../HitTestResult.h"
#include "NumberScale.h"
#include "Prefs.h"
#include "ProjectHistory.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../WaveTrack.h"
#include "../../../../prefs/WaveformSettings.h"

WaveformVZoomHandle::WaveformVZoomHandle(
   const std::shared_ptr<WaveTrack> &pTrack, const wxRect &rect, int y)
      : mpTrack{ pTrack } , mZoomStart(y), mZoomEnd(y), mRect(rect)
{
}

WaveformVZoomHandle::~WaveformVZoomHandle() = default;

void WaveformVZoomHandle::Enter( bool, AudacityProject* )
{
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
   mChangeHighlight = RefreshCode::RefreshCell;
#endif
}

bool WaveformVZoomHandle::HandlesRightClick()
{
   return true;
}

UIHandle::Result WaveformVZoomHandle::Click
(const TrackPanelMouseEvent &, AudacityProject *)
{
   return RefreshCode::RefreshNone;
}

UIHandle::Result WaveformVZoomHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   auto pTrack = TrackList::Get( *pProject ).Lock(mpTrack);
   if (!pTrack)
      return Cancelled;
   return WaveTrackVZoomHandle::DoDrag( evt, pProject, mZoomStart, mZoomEnd );
}

HitTestPreview WaveformVZoomHandle::Preview
(const TrackPanelMouseState &st, AudacityProject *)
{
   return WaveTrackVZoomHandle::HitPreview(st.state);
}

UIHandle::Result WaveformVZoomHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject,
 wxWindow *pParent)
{
   auto pTrack = TrackList::Get( *pProject ).Lock(mpTrack);
   return WaveTrackVZoomHandle::DoRelease(
      evt, pProject, pParent, pTrack.get(), mRect,
      DoZoom, WaveformVRulerMenuTable::Instance(),
      mZoomStart, mZoomEnd );
}

UIHandle::Result WaveformVZoomHandle::Cancel(AudacityProject*)
{
   // Cancel is implemented!  And there is no initial state to restore,
   // so just return a code.
   return RefreshCode::RefreshAll;
}

void WaveformVZoomHandle::Draw(
   TrackPanelDrawingContext &context,
   const wxRect &rect, unsigned iPass )
{
   if (!mpTrack.lock()) //? TrackList::Lock()
      return;
   return WaveTrackVZoomHandle::DoDraw(
      context, rect, iPass, mZoomStart, mZoomEnd );
}

wxRect WaveformVZoomHandle::DrawingArea(
   TrackPanelDrawingContext &,
   const wxRect &rect, const wxRect &panelRect, unsigned iPass )
{
   return WaveTrackVZoomHandle::DoDrawingArea( rect, panelRect, iPass );
}

// ZoomKind says how to zoom.
// If ZoomStart and ZoomEnd are not equal, this may override
// the zoomKind and cause a drag-zoom-in.
void WaveformVZoomHandle::DoZoom(
   AudacityProject *pProject,
   WaveTrack *pTrack,
   WaveTrackViewConstants::ZoomActions ZoomKind,
   const wxRect &rect, int zoomStart, int zoomEnd,
   bool fixedMousePoint)
{
   using namespace WaveTrackViewConstants;
   static const float ZOOMLIMIT = 0.001f;

   int height = rect.height;
   int ypos = rect.y;

   // Ensure start and end are in order (swap if not).
   if (zoomEnd < zoomStart)
      std::swap( zoomStart, zoomEnd );

   float min, max, minBand = 0;
   const double rate = pTrack->GetRate();
   const float halfrate = rate / 2;
   float maxFreq = 8000.0;

   bool bDragZoom = WaveTrackVZoomHandle::IsDragZooming(zoomStart, zoomEnd);

   // Possibly override the zoom kind.
   if( bDragZoom )
      ZoomKind = kZoomInByDrag;

   float top=2.0;
   float half=0.5;

   {
      pTrack->GetDisplayBounds(&min, &max);
      const WaveformSettings &waveSettings = pTrack->GetWaveformSettings();
      const bool linear = waveSettings.isLinear();
      if( !linear ){
         top = (LINEAR_TO_DB(2.0) + waveSettings.dBRange) / waveSettings.dBRange;
         half = (LINEAR_TO_DB(0.5) + waveSettings.dBRange) / waveSettings.dBRange;
      }
   }


   // Compute min and max.
   switch(ZoomKind)
   {
   default:
      // If we have covered all the cases, this won't happen.
      // In release builds Audacity will ignore the zoom.
      wxFAIL_MSG("Zooming Case not implemented by Audacity");
      break;
   case kZoomReset:
   case kZoom1to1:
      {
         // Zoom out full
         min = -1.0;
         max = 1.0;
      }
      break;
   case kZoomDiv2:
      {
         // Zoom out even more than full :-)
         // -2.0..+2.0 (or logarithmic equivalent)
         min = -top;
         max = top;
      }
      break;
   case kZoomTimes2:
      {
         // Zoom in to -0.5..+0.5
         min = -half;
         max = half;
      }
      break;
   case kZoomHalfWave:
      {
         // Zoom to show fractionally more than the top half of the wave.
         min = -0.01f;
         max = 1.0;
      }
      break;
   case kZoomInByDrag:
      {
         const float tmin = min, tmax = max;
         const float p1 = (zoomStart - ypos) / (float)height;
         const float p2 = (zoomEnd - ypos) / (float)height;
         max = (tmax * (1.0 - p1) + tmin * p1);
         min = (tmax * (1.0 - p2) + tmin * p2);

         // Waveform view - allow zooming down to a range of ZOOMLIMIT
         if (max - min < ZOOMLIMIT) {     // if user attempts to go smaller...
            float c = (min + max) / 2;    // ...set centre of view to centre of dragged area and top/bottom to ZOOMLIMIT/2 above/below
            min = c - ZOOMLIMIT / 2.0;
            max = c + ZOOMLIMIT / 2.0;
         }
      }
      break;
   case kZoomIn:
      {
         // Enforce maximum vertical zoom
         const float oldRange = max - min;
         const float l = std::max(ZOOMLIMIT, 0.5f * oldRange);
         const float ratio = l / (max - min);

         const float p1 = (zoomStart - ypos) / (float)height;
         float c = (max * (1.0 - p1) + min * p1);
         if (fixedMousePoint)
            min = c - ratio * (1.0f - p1) * oldRange,
            max = c + ratio * p1 * oldRange;
         else
            min = c - 0.5 * l,
            max = c + 0.5 * l;
      }
      break;
   case kZoomOut:
      {
         // Zoom out
         if (min <= -1.0 && max >= 1.0) {
            min = -top;
            max = top;
         }
         else {
            // limit to +/- 1 range unless already outside that range...
            float minRange = (min < -1) ? -top : -1.0;
            float maxRange = (max > 1) ? top : 1.0;
            // and enforce vertical zoom limits.
            const float p1 = (zoomStart - ypos) / (float)height;
            if (fixedMousePoint) {
               const float oldRange = max - min;
               const float c = (max * (1.0 - p1) + min * p1);
               min = std::min(maxRange - ZOOMLIMIT,
                  std::max(minRange, c - 2 * (1.0f - p1) * oldRange));
               max = std::max(minRange + ZOOMLIMIT,
                  std::min(maxRange, c + 2 * p1 * oldRange));
            }
            else {
               const float c = p1 * min + (1 - p1) * max;
               const float l = (max - min);
               min = std::min(maxRange - ZOOMLIMIT,
                              std::max(minRange, c - l));
               max = std::max(minRange + ZOOMLIMIT,
                              std::min(maxRange, c + l));
            }
         }
      }
      break;
   }

   // Now actually apply the zoom.
   for (auto channel : TrackList::Channels(pTrack))
      channel->SetDisplayBounds(min, max);

   zoomEnd = zoomStart = 0;
   if( pProject )
      ProjectHistory::Get( *pProject ).ModifyState(true);
}

///////////////////////////////////////////////////////////////////////////////
// Table class

PopupMenuTable &WaveformVRulerMenuTable::Instance()
{
   static WaveformVRulerMenuTable instance;
   return instance;
}

BEGIN_POPUP_MENU(WaveformVRulerMenuTable)
   // Accelerators only if zooming enabled.
   bool bVZoom;
   gPrefs->Read(wxT("/GUI/VerticalZooming"), &bVZoom, false);

   BeginSection( "Scales" );
   {
      const auto & names = WaveformSettings::GetScaleNames();
      for (int ii = 0, nn = names.size(); ii < nn; ++ii) {
         AppendRadioItem( names[ii].Internal(),
            OnFirstWaveformScaleID + ii, names[ii].Msgid(),
            POPUP_MENU_FN( OnWaveformScaleType ),
            [this, ii]() -> BasicMenu::Item::State {
               WaveTrack *const wt = mpData->pTrack;
               return { true,
                  ii == static_cast<int>(wt->GetWaveformSettings().scaleType)
               };
            }
          );
      }
   }
   EndSection();

   BeginSection( "Zoom" );
      BeginSection( "Basic" );
         AppendItem( "Reset", OnZoomFitVerticalID,
            MakeLabel( XXO("Zoom Reset"), bVZoom, XXO("Shift-Right-Click") ),
            POPUP_MENU_FN( OnZoomReset ) );
         AppendItem( "TimesHalf", OnZoomDiv2ID,        XXO("Zoom x1/2"),
            POPUP_MENU_FN( OnZoomDiv2Vertical ) );
         AppendItem( "TimesTwo", OnZoomTimes2ID,      XXO("Zoom x2"),                       POPUP_MENU_FN( OnZoomTimes2Vertical ) );

   #ifdef EXPERIMENTAL_HALF_WAVE
         AppendItem( "HalfWave", OnZoomHalfWaveID,    XXO("Half Wave"),                     POPUP_MENU_FN( OnZoomHalfWave ) );
   #endif
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

END_POPUP_MENU()

void WaveformVRulerMenuTable::OnWaveformScaleType(wxCommandEvent &evt)
{
   WaveTrack *const wt = mpData->pTrack;
   // Assume linked track is wave or null
   const WaveformSettings::ScaleType newScaleType =
      WaveformSettings::ScaleType(
         std::max(0,
            std::min((int)(WaveformSettings::stNumScaleTypes) - 1,
               evt.GetId() - OnFirstWaveformScaleID
      )));

   if (wt->GetWaveformSettings().scaleType != newScaleType) {
      for (auto channel : TrackList::Channels(wt)) {
         channel->GetWaveformSettings().scaleType = newScaleType;
      }

      AudacityProject *const project = &mpData->project;
      ProjectHistory::Get( *project ).ModifyState(true);

      using namespace RefreshCode;
      mpData->result = UpdateVRuler | RefreshAll;
   }
}
