/**********************************************************************

Audacity: A Digital Audio Editor

SelectHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_SELECT_HANDLE__
#define __AUDACITY_SELECT_HANDLE__

#include "../../UIHandle.h"
#include "../../SelectedRegion.h"

#include "../../MemoryX.h"
#include <vector>

#include <wx/event.h>
#include <wx/gdicmn.h>

struct HitTestResult;
class SelectionStateChanger;
class SnapManager;
class SpectrumAnalyst;
class Track;
class ViewInfo;
class WaveTrack;

class SelectHandle : public wxEvtHandler, public UIHandle
{
   SelectHandle();
   SelectHandle(const SelectHandle&);
   SelectHandle &operator=(const SelectHandle&);

public:
   static SelectHandle& Instance();

   // This always hits, but details of the hit vary with mouse position and
   // key state.
   static HitTestResult HitTest
      (const TrackPanelMouseState &state, const AudacityProject *pProject,
       const std::shared_ptr<Track> &pTrack);

   virtual ~SelectHandle();

   bool IsClicked() const;

   virtual Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject);

   virtual Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject);

   virtual HitTestPreview Preview
      (const TrackPanelMouseState &state, const AudacityProject *pProject);

   virtual Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent);

   virtual Result Cancel(AudacityProject*);

   virtual void DrawExtras
      (DrawingPass pass,
      wxDC * dc, const wxRegion &updateRegion, const wxRect &panelRect);

   // Receives timer event notifications, to implement auto-scroll
   void OnTimer(wxCommandEvent &event);

private:
   void Connect(AudacityProject *pProject);
   void Disconnect();

   void StartSelection
      (AudacityProject *pProject, int mouseXCoordinate, int trackLeftEdge);
   void AdjustSelection
      (AudacityProject *pProject,
       ViewInfo &viewInfo, int mouseXCoordinate, int trackLeftEdge,
       Track *pTrack);

   void StartFreqSelection
      (ViewInfo &viewInfo, int mouseYCoordinate, int trackTopEdge,
      int trackHeight, Track *pTrack);
   void AdjustFreqSelection
      (const WaveTrack *wt,
       ViewInfo &viewInfo, int mouseYCoordinate, int trackTopEdge,
       int trackHeight);

   void HandleCenterFrequencyClick
      (const ViewInfo &viewInfo, bool shiftDown,
       const WaveTrack *pTrack, double value);
   static void StartSnappingFreqSelection
      (SpectrumAnalyst &analyst,
       const ViewInfo &viewInfo, const WaveTrack *pTrack);
   void MoveSnappingFreqSelection
      (AudacityProject *pProject, ViewInfo &viewInfo, int mouseYCoordinate,
       int trackTopEdge,
       int trackHeight, Track *pTrack);
public:
   // This is needed to implement a command assignable to keystrokes
   static void SnapCenterOnce
      (SpectrumAnalyst &analyst,
       ViewInfo &viewInfo, const WaveTrack *pTrack, bool up);
private:
   //void ResetFreqSelectionPin
   //   (const ViewInfo &viewInfo, double hintFrequency, bool logF);


   std::weak_ptr<Track> mpTrack;
   wxRect mRect{};
   SelectedRegion mInitialSelection{};

   // Handles snapping the selection boundaries or track boundaries to
   // line up with existing tracks or labels.  mSnapLeft and mSnapRight
   // are the horizontal index of pixels to display user feedback
   // guidelines so the user knows when such snapping is taking place.
   std::unique_ptr<SnapManager> mSnapManager;
   wxInt64 mSnapLeft{ -1 };
   wxInt64 mSnapRight{ -1 };

   bool mSelStartValid{};
   double mSelStart{ 0.0 };

   int mSelectionBoundary{ 0 };

   enum eFreqSelMode {
      FREQ_SEL_INVALID,

      FREQ_SEL_SNAPPING_CENTER,
      FREQ_SEL_PINNED_CENTER,
      FREQ_SEL_DRAG_CENTER,

      FREQ_SEL_FREE,
      FREQ_SEL_TOP_FREE,
      FREQ_SEL_BOTTOM_FREE,
   }  mFreqSelMode{ FREQ_SEL_INVALID };
   std::weak_ptr<const WaveTrack> mFreqSelTrack;
   // Following holds:
   // the center for FREQ_SEL_PINNED_CENTER,
   // the ratio of top to center (== center to bottom) for FREQ_SEL_DRAG_CENTER,
   // a frequency boundary for FREQ_SEL_FREE, FREQ_SEL_TOP_FREE, or
   // FREQ_SEL_BOTTOM_FREE,
   // and is ignored otherwise.
   double mFreqSelPin{ -1.0 };
   std::unique_ptr<SpectrumAnalyst> mFrequencySnapper;

   int mMostRecentX{ -1 }, mMostRecentY{ -1 };

   bool mAutoScrolling{};

   AudacityProject *mConnectedProject{};

   std::unique_ptr<SelectionStateChanger> mSelectionStateChanger;
};
#endif
