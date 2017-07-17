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
   return (abs(zoomEnd - zoomStart) > DragThreshold);
}

}

WaveTrackVZoomHandle::WaveTrackVZoomHandle
(const std::shared_ptr<WaveTrack> &pTrack, const wxRect &rect, int y)
   : mZoomStart(y), mZoomEnd(y), mRect(rect)
   , mpTrack{ pTrack }
{
}

void WaveTrackVZoomHandle::Enter(bool)
{
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
   mChangeHighlight = RefreshCode::RefreshCell;
#endif
}

void WaveTrackVZoomHandle::DoZoom
   (AudacityProject *pProject,
    WaveTrack *pTrack, bool shiftDown, bool rightUp,
    const wxRect &rect, int zoomStart, int zoomEnd,
    bool fixedMousePoint)
{
   static const float ZOOMLIMIT = 0.001f;

   TrackList *const tracks = pProject->GetTracks();

   // Assume linked track is wave or null
   const auto partner = static_cast<WaveTrack *>(pTrack->GetLink());
   int height = rect.height;
   int ypos = rect.y;

   // Ensure start and end are in order (swap if not).
   if (zoomEnd < zoomStart)
      std::swap( zoomStart, zoomEnd );

   float min, max, c, minBand = 0;
   const double rate = pTrack->GetRate();
   const float halfrate = rate / 2;
   const SpectrogramSettings &settings = pTrack->GetSpectrogramSettings();
   NumberScale scale;
   const bool spectral = (pTrack->GetDisplay() == WaveTrack::Spectrum);
   const bool spectrumLinear = spectral &&
      (pTrack->GetSpectrogramSettings().scaleType == SpectrogramSettings::stLinear);

   if (spectral) {
      pTrack->GetSpectrumBounds(&min, &max);
      scale = (settings.GetScale(min, max));
      const auto fftLength = settings.GetFFTLength();
      const float binSize = rate / fftLength;

      // JKC:  Following discussions of Bug 1208 I'm allowing zooming in
      // down to one bin.
      //      const int minBins =
      //         std::min(10, fftLength / 2); //minimum 10 freq bins, unless there are less
      const int minBins = 1;
      minBand = minBins * binSize;
   }
   else
      pTrack->GetDisplayBounds(&min, &max);

   if (IsDragZooming(zoomStart, zoomEnd)) {
      // Drag Zoom
      const float tmin = min, tmax = max;

      if (spectral) {
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
      else {
         const float p1 = (zoomStart - ypos) / (float)height;
         const float p2 = (zoomEnd - ypos) / (float)height;
         max = (tmax * (1.0 - p1) + tmin * p1);
         min = (tmax * (1.0 - p2) + tmin * p2);

         // Waveform view - allow zooming down to a range of ZOOMLIMIT
         if (max - min < ZOOMLIMIT) {     // if user attempts to go smaller...
            c = (min + max) / 2;           // ...set centre of view to centre of dragged area and top/bottom to ZOOMLIMIT/2 above/below
            min = c - ZOOMLIMIT / 2.0;
            max = c + ZOOMLIMIT / 2.0;
         }
      }
   }
   else if (shiftDown || rightUp) {
      // Zoom OUT
      if (spectral) {
         if (shiftDown && rightUp) {
            // Zoom out full
            min = spectrumLinear ? 0.0f : 1.0f;
            max = halfrate;
         }
         else {
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
      }
      else {
         // Zoom out to -1.0...1.0 first, then, and only
         // then, if they click again, allow one more
         // zoom out.
         if (shiftDown && rightUp) {
            // Zoom out full
            min = -1.0;
            max = 1.0;
         }
         else {
            // Zoom out
            const WaveformSettings &settings = pTrack->GetWaveformSettings();
            const bool linear = settings.isLinear();
            const float top = linear
               ? 2.0
               : (LINEAR_TO_DB(2.0) + settings.dBRange) / settings.dBRange;
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
      }
   }
   else {
      // Zoom IN
      if (spectral) {
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
      else {
         // Zoom in centered on cursor
         if (min < -1.0 || max > 1.0) {
            min = -1.0;
            max = 1.0;
         }
         else {
            // Enforce maximum vertical zoom
            const float oldRange = max - min;
            const float l = std::max(ZOOMLIMIT, 0.5f * oldRange);
            const float ratio = l / (max - min);

            const float p1 = (zoomStart - ypos) / (float)height;
            const float c = (max * (1.0 - p1) + min * p1);
            if (fixedMousePoint)
               min = c - ratio * (1.0f - p1) * oldRange,
               max = c + ratio * p1 * oldRange;
            else
               min = c - 0.5 * l,
               max = c + 0.5 * l;
         }
      }
   }

   if (spectral) {
      pTrack->SetSpectrumBounds(min, max);
      if (partner)
         partner->SetSpectrumBounds(min, max);
   }
   else {
      pTrack->SetDisplayBounds(min, max);
      if (partner)
         partner->SetDisplayBounds(min, max);
   }

   zoomEnd = zoomStart = 0;
   pProject->ModifyState(true);
}

enum {
   OnZoomInVerticalID = 20000,
   OnZoomOutVerticalID,
   OnZoomFitVerticalID,

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

   void OnZoomInVertical(wxCommandEvent&);
   void OnZoomOutVertical(wxCommandEvent&);
   void OnZoomFitVertical(wxCommandEvent&);
};

void WaveTrackVRulerMenuTable::InitMenu(Menu *, void *pUserData)
{
   mpData = static_cast<InitMenuData*>(pUserData);
}

void WaveTrackVRulerMenuTable::OnZoomInVertical(wxCommandEvent &)
{
   WaveTrackVZoomHandle::DoZoom
      (::GetActiveProject(), mpData->pTrack, false, false, mpData->rect, mpData->yy, mpData->yy, false);

   using namespace RefreshCode;
   mpData->result = UpdateVRuler | RefreshAll;
}

void WaveTrackVRulerMenuTable::OnZoomOutVertical(wxCommandEvent &)
{
   WaveTrackVZoomHandle::DoZoom
      (::GetActiveProject(), mpData->pTrack, true, false, mpData->rect, mpData->yy, mpData->yy, false);

   using namespace RefreshCode;
   mpData->result = UpdateVRuler | RefreshAll;
}

void WaveTrackVRulerMenuTable::OnZoomFitVertical(wxCommandEvent &)
{
   WaveTrackVZoomHandle::DoZoom
      (::GetActiveProject(), mpData->pTrack, true, true, mpData->rect, mpData->yy, mpData->yy, false);

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
   virtual void InitMenu(Menu *pMenu, void *pUserData);

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

   WaveTrack *const wt = mpData->pTrack;
   const int id =
      OnFirstWaveformScaleID + (int)(wt->GetWaveformSettings().scaleType);
   pMenu->Check(id, true);
}

BEGIN_POPUP_MENU(WaveformVRulerMenuTable)

   {
      const wxArrayString & names = WaveformSettings::GetScaleNames();
      for (int ii = 0, nn = names.size(); ii < nn; ++ii) {
         POPUP_MENU_RADIO_ITEM(OnFirstWaveformScaleID + ii, names[ii],
            OnWaveformScaleType);
      }
   }

   POPUP_MENU_SEPARATOR()
   POPUP_MENU_ITEM(OnZoomInVerticalID,  _("Zoom In\tLeft-Click/Left-Drag"),  OnZoomInVertical)
   POPUP_MENU_ITEM(OnZoomOutVerticalID, _("Zoom Out\tShift-Left-Click"),     OnZoomOutVertical)
   POPUP_MENU_ITEM(OnZoomFitVerticalID, _("Zoom to Fit\tShift-Right-Click"), OnZoomFitVertical)
END_POPUP_MENU()

void WaveformVRulerMenuTable::OnWaveformScaleType(wxCommandEvent &evt)
{
   WaveTrack *const wt = mpData->pTrack;
   // Assume linked track is wave or null
   const auto partner = static_cast<WaveTrack*>(wt->GetLink());
   const WaveformSettings::ScaleType newScaleType =
      WaveformSettings::ScaleType(
         std::max(0,
            std::min((int)(WaveformSettings::stNumScaleTypes) - 1,
               evt.GetId() - OnFirstWaveformScaleID
      )));
   if (wt->GetWaveformSettings().scaleType != newScaleType) {
      wt->GetIndependentWaveformSettings().scaleType = newScaleType;
      if (partner)
         partner->GetIndependentWaveformSettings().scaleType = newScaleType;

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
   void InitMenu(Menu *pMenu, void *pUserData);

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
      const wxArrayString & names = SpectrogramSettings::GetScaleNames();
      for (int ii = 0, nn = names.size(); ii < nn; ++ii) {
         POPUP_MENU_RADIO_ITEM(OnFirstSpectrumScaleID + ii, names[ii],
            OnSpectrumScaleType);
      }
   }

   POPUP_MENU_SEPARATOR()
   POPUP_MENU_ITEM(OnZoomInVerticalID,  _("Zoom In\tLeft-Click/Left-Drag"),  OnZoomInVertical)
   POPUP_MENU_ITEM(OnZoomOutVerticalID, _("Zoom Out\tShift-Left-Click"),     OnZoomOutVertical)
   POPUP_MENU_ITEM(OnZoomFitVerticalID, _("Zoom to Fit\tShift-Right-Click"), OnZoomFitVertical)
END_POPUP_MENU()

void SpectrumVRulerMenuTable::OnSpectrumScaleType(wxCommandEvent &evt)
{
   WaveTrack *const wt = mpData->pTrack;
   // Assume linked track is wave or null
   const auto partner = static_cast<WaveTrack*>(wt->GetLink());
   const SpectrogramSettings::ScaleType newScaleType =
      SpectrogramSettings::ScaleType(
         std::max(0,
            std::min((int)(SpectrogramSettings::stNumScaleTypes) - 1,
               evt.GetId() - OnFirstSpectrumScaleID
      )));
   if (wt->GetSpectrogramSettings().scaleType != newScaleType) {
      wt->GetIndependentSpectrogramSettings().scaleType = newScaleType;
      if (partner)
         partner->GetIndependentSpectrogramSettings().scaleType = newScaleType;

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
   const auto message =
_("Click to vertically zoom in. Shift-click to zoom out. Drag to specify a zoom region.");
   return {
      message,
      (state.ShiftDown() ? &*zoomOutCursor : &*zoomInCursor)
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

   // Popup menu... disabled
   if (false &&
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

      pParent->PopupMenu(pMenu.get(), event.m_x, event.m_y);

      return data.result;
   }
   else
      DoZoom(pProject, pTrack.get(), shiftDown, rightUp,
         mRect, mZoomStart, mZoomEnd, false);

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

