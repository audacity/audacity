/**********************************************************************

Sneedacity: A Digital Audio Editor

SpectrumVZoomHandle.h

Paul Licameli split from WaveTrackVZoomHandle.h

**********************************************************************/

#ifndef __SNEEDACITY_SPECTRUM_VZOOM_HANDLE__
#define __SNEEDACITY_SPECTRUM_VZOOM_HANDLE__

#include "../../../../UIHandle.h" // to inherit
#include "WaveTrackViewConstants.h"

class WaveTrack;

class SpectrumVZoomHandle final : public UIHandle
{
   SpectrumVZoomHandle(const SpectrumVZoomHandle&);

public:
   explicit SpectrumVZoomHandle
      (const std::shared_ptr<WaveTrack> &pTrack, const wxRect &rect, int y);

   SpectrumVZoomHandle &operator=(const SpectrumVZoomHandle&) = default;

   static void DoZoom(
      SneedacityProject *pProject, WaveTrack *pTrack,
      WaveTrackViewConstants::ZoomActions ZoomKind,
      const wxRect &rect, int zoomStart, int zoomEnd,
      bool fixedMousePoint);

   ~SpectrumVZoomHandle() override;

   std::shared_ptr<WaveTrack> GetTrack() const { return mpTrack.lock(); }

   void Enter(bool forward, SneedacityProject*) override;

   Result Click
      (const TrackPanelMouseEvent &event, SneedacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, SneedacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseState &state, SneedacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, SneedacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(SneedacityProject *pProject) override;

private:

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;

   wxRect DrawingArea(
      TrackPanelDrawingContext &,
      const wxRect &rect, const wxRect &panelRect, unsigned iPass ) override;

   std::weak_ptr<WaveTrack> mpTrack;

   int mZoomStart{}, mZoomEnd{};
   wxRect mRect{};
};

#include "WaveTrackVZoomHandle.h" // to inherit

class SpectrumVRulerMenuTable : public WaveTrackVRulerMenuTable
{
   SpectrumVRulerMenuTable()
      : WaveTrackVRulerMenuTable{ "SpectrumVRuler" }
   {}
   virtual ~SpectrumVRulerMenuTable() {}
   DECLARE_POPUP_MENU(SpectrumVRulerMenuTable);

public:
   static PopupMenuTable &Instance();

private:
   void OnSpectrumScaleType(wxCommandEvent &evt);
};

#endif
