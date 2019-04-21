/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackVZoomHandle.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../../Audacity.h"
#include "WaveTrackVZoomHandle.h"

#include "../../../../Experimental.h"

#include "WaveTrackVRulerControls.h"

#include "../../../../HitTestResult.h"
#include "../../../../NumberScale.h"
#include "../../../../prefs/SpectrogramSettings.h"
#include "../../../../prefs/WaveformSettings.h"
#include "../../../../Project.h"
#include "../../../../RefreshCode.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "../../../../WaveTrack.h"
#include "../../../../widgets/PopupMenuTable.h"
#include "../../../../../images/Cursors.h"
#include "../../../../Prefs.h"

namespace
{

struct InitMenuData
{
public:
   WaveTrack *pTrack;
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

WaveTrackVZoomHandle::WaveTrackVZoomHandle
(const std::shared_ptr<WaveTrack> &pTrack, const wxRect &rect, int y)
   : mpTrack{ pTrack } , mZoomStart(y), mZoomEnd(y), mRect(rect)
{
}

void WaveTrackVZoomHandle::Enter(bool)
{
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
   mChangeHighlight = RefreshCode::RefreshCell;
#endif
}

// ZoomKind says how to zoom.
// If ZoomStart and ZoomEnd are not equal, this may override
// the zoomKind and cause a drag-zoom-in.
void WaveTrackVZoomHandle::DoZoom
   (AudacityProject *pProject,
    WaveTrack *pTrack, bool allChannels, int ZoomKind,
    const wxRect &rect, int zoomStart, int zoomEnd,
    bool fixedMousePoint)
{
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
   const SpectrogramSettings &specSettings = pTrack->GetSpectrogramSettings();
   NumberScale scale;
   const bool spectral = (pTrack->GetDisplay() == WaveTrack::Spectrum);
   const bool spectrumLinear = spectral &&
      (pTrack->GetSpectrogramSettings().scaleType == SpectrogramSettings::stLinear);


   bool bDragZoom = IsDragZooming(zoomStart, zoomEnd);
   // Add 100 if spectral to separate the kinds of zoom.
   const int kSpectral = 100;

   // Possibly override the zoom kind.
   if( bDragZoom )
      ZoomKind = kZoomInByDrag;

   // If we are actually zooming a spectrum rather than a wave.
   ZoomKind += spectral ? kSpectral:0;

   float top=2.0;
   float half=0.5;

   if (spectral) {
      pTrack->GetSpectrumBounds(&min, &max);
      scale = (specSettings.GetScale(min, max));
      const auto fftLength = specSettings.GetFFTLength();
      const float binSize = rate / fftLength;
      maxFreq = gPrefs->Read(wxT("/Spectrum/MaxFreq"), 8000L);
      // JKC:  Following discussions of Bug 1208 I'm allowing zooming in
      // down to one bin.
      //      const int minBins =
      //         std::min(10, fftLength / 2); //minimum 10 freq bins, unless there are less
      const int minBins = 1;
      minBand = minBins * binSize;
   }
   else{
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

   // VZooming on spectral we don't implement the other zoom presets.
   // They are also not in the menu.
   case kZoomReset + kSpectral:
      {
         // Zoom out to normal level.
         min = spectrumLinear ? 0.0f : 1.0f;
         max = maxFreq;
      }
      break;
   case kZoom1to1 + kSpectral:
   case kZoomDiv2 + kSpectral:
   case kZoomTimes2 + kSpectral:
   case kZoomHalfWave + kSpectral:
      {
         // Zoom out full
         min = spectrumLinear ? 0.0f : 1.0f;
         max = halfrate;
      }
      break;
   case kZoomInByDrag + kSpectral:
      {
         double xmin = 1 - (zoomEnd - ypos) / (float)height;
         double xmax = 1 - (zoomStart - ypos) / (float)height;
         const float middle = (xmin + xmax) / 2;
         const float middleValue = scale.PositionToValue(middle);

         min = std::max(spectrumLinear ? 0.0f : 1.0f,
            std::min(middleValue - minBand / 2,
            scale.PositionToValue(xmin)
            ));
         max = std::min(halfrate,
            std::max(middleValue + minBand / 2,
            scale.PositionToValue(xmax)
            ));
      }
      break;
   case kZoomIn + kSpectral:
      {
         // Center the zoom-in at the click
         const float p1 = (zoomStart - ypos) / (float)height;
         const float middle = 1.0f - p1;
         const float middleValue = scale.PositionToValue(middle);

         if (fixedMousePoint) {
            min = std::max(spectrumLinear ? 0.0f : 1.0f,
               std::min(middleValue - minBand * middle,
               scale.PositionToValue(0.5f * middle)
            ));
            max = std::min(halfrate,
               std::max(middleValue + minBand * p1,
               scale.PositionToValue(middle + 0.5f * p1)
            ));
         }
         else {
            min = std::max(spectrumLinear ? 0.0f : 1.0f,
               std::min(middleValue - minBand / 2,
               scale.PositionToValue(middle - 0.25f)
            ));
            max = std::min(halfrate,
               std::max(middleValue + minBand / 2,
               scale.PositionToValue(middle + 0.25f)
            ));
         }
      }
      break;
   case kZoomOut + kSpectral:
      {
         // Zoom out
         const float p1 = (zoomStart - ypos) / (float)height;
         // (Used to zoom out centered at midline, ignoring the click, if linear view.
         //  I think it is better to be consistent.  PRL)
         // Center zoom-out at the midline
         const float middle = // spectrumLinear ? 0.5f :
            1.0f - p1;

         if (fixedMousePoint) {
            min = std::max(spectrumLinear ? 0.0f : 1.0f, scale.PositionToValue(-middle));
            max = std::min(halfrate, scale.PositionToValue(1.0f + p1));
         }
         else {
            min = std::max(spectrumLinear ? 0.0f : 1.0f, scale.PositionToValue(middle - 1.0f));
            max = std::min(halfrate, scale.PositionToValue(middle + 1.0f));
         }
      }
      break;
   }

   // Now actually apply the zoom.
   for (auto channel : TrackList::Channels(pTrack)) {
      if (!allChannels && channel != pTrack)
         continue;
      if (spectral)
         channel->SetSpectrumBounds(min, max);
      else
         channel->SetDisplayBounds(min, max);
   }

   zoomEnd = zoomStart = 0;
   if( pProject )
      pProject->ModifyState(true);
}

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

class WaveTrackVRulerMenuTable : public PopupMenuTable
{
protected:
   WaveTrackVRulerMenuTable() {}

   void InitMenu(Menu *pMenu, void *pUserData) override;

private:
   void DestroyMenu() override
   {
      mpData = nullptr;
   }

protected:
   InitMenuData *mpData {};

   void OnZoom( int iZoomCode );
   void OnZoomFitVertical(wxCommandEvent&){ OnZoom( kZoom1to1 );};
   void OnZoomReset(wxCommandEvent&){ OnZoom( kZoomReset );};
   void OnZoomDiv2Vertical(wxCommandEvent&){ OnZoom( kZoomDiv2 );};
   void OnZoomTimes2Vertical(wxCommandEvent&){ OnZoom( kZoomTimes2 );};
   void OnZoomHalfWave(wxCommandEvent&){ OnZoom( kZoomHalfWave );};
   void OnZoomInVertical(wxCommandEvent&){ OnZoom( kZoomIn );};
   void OnZoomOutVertical(wxCommandEvent&){ OnZoom( kZoomOut );};
};

void WaveTrackVRulerMenuTable::InitMenu(Menu *, void *pUserData)
{
   mpData = static_cast<InitMenuData*>(pUserData);
}


void WaveTrackVRulerMenuTable::OnZoom( int iZoomCode )
{
   WaveTrackVZoomHandle::DoZoom
      (::GetActiveProject(), mpData->pTrack, true,
       iZoomCode, mpData->rect, mpData->yy, mpData->yy, false);

   using namespace RefreshCode;
   mpData->result = UpdateVRuler | RefreshAll;
}

///////////////////////////////////////////////////////////////////////////////
// Table class

class WaveformVRulerMenuTable : public WaveTrackVRulerMenuTable
{
   WaveformVRulerMenuTable() : WaveTrackVRulerMenuTable() {}
   virtual ~WaveformVRulerMenuTable() {}
   DECLARE_POPUP_MENU(WaveformVRulerMenuTable);

public:
   static WaveformVRulerMenuTable &Instance();

private:
   virtual void InitMenu(Menu *pMenu, void *pUserData) override;

   void OnWaveformScaleType(wxCommandEvent &evt);
};

WaveformVRulerMenuTable &WaveformVRulerMenuTable::Instance()
{
   static WaveformVRulerMenuTable instance;
   return instance;
}

void WaveformVRulerMenuTable::InitMenu(Menu *pMenu, void *pUserData)
{
   WaveTrackVRulerMenuTable::InitMenu(pMenu, pUserData);

// DB setting is already on track drop down.
#if 0 
   WaveTrack *const wt = mpData->pTrack;
   const int id =
      OnFirstWaveformScaleID + (int)(wt->GetWaveformSettings().scaleType);
   pMenu->Check(id, true);
#endif
}

BEGIN_POPUP_MENU(WaveformVRulerMenuTable)

   POPUP_MENU_ITEM(OnZoomFitVerticalID, _("Zoom Reset\tShift-Right-Click"), OnZoomReset)
   POPUP_MENU_ITEM(OnZoomDiv2ID,        _("Zoom x1/2"),                     OnZoomDiv2Vertical)
   POPUP_MENU_ITEM(OnZoomTimes2ID,      _("Zoom x2"),                       OnZoomTimes2Vertical)

#ifdef EXPERIMENTAL_HALF_WAVE
   POPUP_MENU_ITEM(OnZoomHalfWaveID,    _("Half Wave"),                     OnZoomHalfWave)
#endif

   POPUP_MENU_SEPARATOR()
   POPUP_MENU_ITEM(OnZoomInVerticalID,  _("Zoom In\tLeft-Click/Left-Drag"),  OnZoomInVertical)
   POPUP_MENU_ITEM(OnZoomOutVerticalID, _("Zoom Out\tShift-Left-Click"),     OnZoomOutVertical)
// The log and linear options are already available as waveform db.
// So don't repeat them here.
#if 0
   POPUP_MENU_SEPARATOR()
   {
      const auto & names = WaveformSettings::GetScaleNames();
      for (int ii = 0, nn = names.size(); ii < nn; ++ii) {
         POPUP_MENU_RADIO_ITEM(OnFirstWaveformScaleID + ii, names[ii],
            OnWaveformScaleType);
      }
   }
#endif
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
         channel->GetIndependentWaveformSettings().scaleType = newScaleType;
      }

      ::GetActiveProject()->ModifyState(true);

      using namespace RefreshCode;
      mpData->result = UpdateVRuler | RefreshAll;
   }
}

///////////////////////////////////////////////////////////////////////////////
// Table class

class SpectrumVRulerMenuTable : public WaveTrackVRulerMenuTable
{
   SpectrumVRulerMenuTable() : WaveTrackVRulerMenuTable() {}
   virtual ~SpectrumVRulerMenuTable() {}
   DECLARE_POPUP_MENU(SpectrumVRulerMenuTable);

public:
   static SpectrumVRulerMenuTable &Instance();

private:
   void InitMenu(Menu *pMenu, void *pUserData) override;

   void OnSpectrumScaleType(wxCommandEvent &evt);
};

SpectrumVRulerMenuTable &SpectrumVRulerMenuTable::Instance()
{
   static SpectrumVRulerMenuTable instance;
   return instance;
}

void SpectrumVRulerMenuTable::InitMenu(Menu *pMenu, void *pUserData)
{
   WaveTrackVRulerMenuTable::InitMenu(pMenu, pUserData);

   WaveTrack *const wt = mpData->pTrack;
   const int id =
      OnFirstSpectrumScaleID + (int)(wt->GetSpectrogramSettings().scaleType);
   pMenu->Check(id, true);
}

BEGIN_POPUP_MENU(SpectrumVRulerMenuTable)

   {
      const auto & names = SpectrogramSettings::GetScaleNames();
      for (int ii = 0, nn = names.size(); ii < nn; ++ii) {
         POPUP_MENU_RADIO_ITEM(OnFirstSpectrumScaleID + ii, names[ii],
            OnSpectrumScaleType);
      }
   }

POPUP_MENU_SEPARATOR()
   POPUP_MENU_ITEM(OnZoomResetID,         _("Zoom Reset"),                     OnZoomReset)
   POPUP_MENU_ITEM(OnZoomFitVerticalID,   _("Zoom to Fit\tShift-Right-Click"), OnZoomFitVertical)
   POPUP_MENU_ITEM(OnZoomInVerticalID,    _("Zoom In\tLeft-Click/Left-Drag"),  OnZoomInVertical)
   POPUP_MENU_ITEM(OnZoomOutVerticalID,   _("Zoom Out\tShift-Left-Click"),     OnZoomOutVertical)
END_POPUP_MENU()

void SpectrumVRulerMenuTable::OnSpectrumScaleType(wxCommandEvent &evt)
{
   WaveTrack *const wt = mpData->pTrack;

   const SpectrogramSettings::ScaleType newScaleType =
      SpectrogramSettings::ScaleType(
         std::max(0,
            std::min((int)(SpectrogramSettings::stNumScaleTypes) - 1,
               evt.GetId() - OnFirstSpectrumScaleID
      )));
   if (wt->GetSpectrogramSettings().scaleType != newScaleType) {
      for (auto channel : TrackList::Channels(wt))
         channel->GetIndependentSpectrogramSettings().scaleType = newScaleType;

      ::GetActiveProject()->ModifyState(true);

      using namespace RefreshCode;
      mpData->result = UpdateVRuler | RefreshAll;
   }
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
      _("Click to vertically zoom in. Shift-click to zoom out. Drag to specify a zoom region.") :
      _("Right-click for menu.");

   return {
      message,
      bVZoom ? (state.ShiftDown() ? &*zoomOutCursor : &*zoomInCursor) : &arrowCursor
      // , message
   };
}

WaveTrackVZoomHandle::~WaveTrackVZoomHandle()
{
}

UIHandle::Result WaveTrackVZoomHandle::Click
(const TrackPanelMouseEvent &, AudacityProject *)
{
   return RefreshCode::RefreshNone;
}

UIHandle::Result WaveTrackVZoomHandle::Drag
(const TrackPanelMouseEvent &evt, AudacityProject *pProject)
{
   using namespace RefreshCode;
   auto pTrack = pProject->GetTracks()->Lock(mpTrack);
   if (!pTrack)
      return Cancelled;

   const wxMouseEvent &event = evt.event;
   if ( event.RightIsDown() )
      return RefreshNone;
   mZoomEnd = event.m_y;
   if (IsDragZooming(mZoomStart, mZoomEnd))
      return RefreshAll;
   return RefreshNone;
}

HitTestPreview WaveTrackVZoomHandle::Preview
(const TrackPanelMouseState &st, const AudacityProject *)
{
   return HitPreview(st.state);
}

UIHandle::Result WaveTrackVZoomHandle::Release
(const TrackPanelMouseEvent &evt, AudacityProject *pProject,
 wxWindow *pParent)
{
   using namespace RefreshCode;
   auto pTrack = pProject->GetTracks()->Lock(mpTrack);
   if (!pTrack)
      return RefreshNone;

   const wxMouseEvent &event = evt.event;
   const bool shiftDown = event.ShiftDown();
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
         (pTrack->GetDisplay() == WaveTrack::Spectrum)
         ? (PopupMenuTable *) &SpectrumVRulerMenuTable::Instance()
         : (PopupMenuTable *) &WaveformVRulerMenuTable::Instance();
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
   else{
      // Ignore Capture Lost event 
      bVZoom &= event.GetId() != kCaptureLostEventId;
      // shiftDown | rightUp | ZoomKind
      //    T      |    T    | 1to1
      //    T      |    F    | Out
      //    F      |    -    | In
      if( bVZoom ){
         if( shiftDown )
            mZoomStart=mZoomEnd;
         DoZoom(pProject, pTrack.get(), true,
                shiftDown ? (rightUp ? kZoom1to1 : kZoomOut)  : kZoomIn,
            mRect, mZoomStart, mZoomEnd, !shiftDown);
      }
   }

   return UpdateVRuler | RefreshAll;
}

UIHandle::Result WaveTrackVZoomHandle::Cancel(AudacityProject*)
{
   // Cancel is implemented!  And there is no initial state to restore,
   // so just return a code.
   return RefreshCode::RefreshAll;
}

void WaveTrackVZoomHandle::DrawExtras
(DrawingPass pass, wxDC * dc, const wxRegion &, const wxRect &panelRect)
{
   if (!mpTrack.lock()) // TrackList::Lock()?
      return;

   if ( pass == UIHandle::Cells &&
        IsDragZooming( mZoomStart, mZoomEnd ) )
      TrackVRulerControls::DrawZooming
         ( dc, mRect, panelRect, mZoomStart, mZoomEnd );
}

