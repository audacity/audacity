/**********************************************************************

Audacity: A Digital Audio Editor

WaveTrackVZoomHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_WAVE_TRACK_VZOOM_HANDLE__
#define __AUDACITY_WAVE_TRACK_VZOOM_HANDLE__

class wxMouseState;
class PopupMenuTable;
class WaveTrack;
#include "WaveTrackViewConstants.h"
#include "../../../../UIHandle.h"


class WaveTrackVZoomHandle : public UIHandle
{
   WaveTrackVZoomHandle(const WaveTrackVZoomHandle&);
   static HitTestPreview HitPreview(const wxMouseState &state);

public:
   explicit WaveTrackVZoomHandle
      (const std::shared_ptr<WaveTrack> &pTrack, const wxRect &rect, int y);

   WaveTrackVZoomHandle &operator=(const WaveTrackVZoomHandle&) = default;

   static bool IsDragZooming(int zoomStart, int zoomEnd);

   using DoZoomFunction = void (*)( AudacityProject *pProject,
       WaveTrack *pTrack,
       WaveTrackViewConstants::ZoomActions ZoomKind,
       const wxRect &rect, int zoomStart, int zoomEnd,
       bool fixedMousePoint);

   virtual ~WaveTrackVZoomHandle();

   std::shared_ptr<WaveTrack> GetTrack() const { return mpTrack.lock(); }

   void Enter(bool forward) override;

   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;
   static Result DoDrag(
      const TrackPanelMouseEvent &event, AudacityProject *pProject,
      int zoomStart, int &zoomEnd );

   HitTestPreview Preview
      (const TrackPanelMouseState &state, const AudacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;
   static Result DoRelease(
      const TrackPanelMouseEvent &event, AudacityProject *pProject,
      wxWindow *pParent, WaveTrack *pTrack, const wxRect &mRect,
      DoZoomFunction doZoom, PopupMenuTable &table,
      int zoomStart, int zoomEnd );

   Result Cancel(AudacityProject *pProject) override;

private:

   // TrackPanelDrawable implementation
   void Draw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass ) override;
   static void DoDraw(
      TrackPanelDrawingContext &context,
      const wxRect &rect, unsigned iPass, int zoomStart, int zoomEnd );

   wxRect DrawingArea(
      const wxRect &rect, const wxRect &panelRect, unsigned iPass ) override;
   static wxRect DoDrawingArea(
      const wxRect &rect, const wxRect &panelRect, unsigned iPass );

   std::weak_ptr<WaveTrack> mpTrack;

   int mZoomStart{}, mZoomEnd{};
   wxRect mRect{};

   friend class SpectrumVZoomHandle;
   friend class WaveformVZoomHandle;
};

#include "../../../../widgets/PopupMenuTable.h" // to inherit

class WaveTrackVRulerMenuTable : public PopupMenuTable
{
public:
   struct InitMenuData
   {
   public:
      WaveTrack *pTrack;
      wxRect rect;
      unsigned result;
      int yy;
      WaveTrackVZoomHandle::DoZoomFunction doZoom;
   };

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

   void OnZoom( WaveTrackViewConstants::ZoomActions iZoomCode );
   void OnZoomFitVertical(wxCommandEvent&)
      { OnZoom( WaveTrackViewConstants::kZoom1to1 );};
   void OnZoomReset(wxCommandEvent&)
      { OnZoom( WaveTrackViewConstants::kZoomReset );};
   void OnZoomDiv2Vertical(wxCommandEvent&)
      { OnZoom( WaveTrackViewConstants::kZoomDiv2 );};
   void OnZoomTimes2Vertical(wxCommandEvent&)
      { OnZoom( WaveTrackViewConstants::kZoomTimes2 );};
   void OnZoomHalfWave(wxCommandEvent&)
      { OnZoom( WaveTrackViewConstants::kZoomHalfWave );};
   void OnZoomInVertical(wxCommandEvent&)
      { OnZoom( WaveTrackViewConstants::kZoomIn );};
   void OnZoomOutVertical(wxCommandEvent&)
      { OnZoom( WaveTrackViewConstants::kZoomOut );};
};

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

   OnZoomMaxID,

   OnUpOctaveID,
   OnDownOctaveID,
};

#endif
