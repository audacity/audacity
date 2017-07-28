/**********************************************************************

Audacity: A Digital Audio Editor

SelectHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_SELECT_HANDLE__
#define __AUDACITY_SELECT_HANDLE__

#include "../../UIHandle.h"
#include "../../SelectedRegion.h"
#include "../../Snap.h"

#include "../../MemoryX.h"
#include <vector>

#include <wx/gdicmn.h>

class SelectionStateChanger;
class SnapManager;
class SpectrumAnalyst;
class Track;
class TrackList;
class ViewInfo;
class WaveTrack;
class wxMouseState;

class SelectHandle : public UIHandle
{
   SelectHandle(const SelectHandle&);

public:
   explicit SelectHandle
      (const std::shared_ptr<Track> &pTrack, bool useSnap,
       const TrackList &trackList,
       const TrackPanelMouseState &st, const ViewInfo &viewInfo);

   // This always hits, but details of the hit vary with mouse position and
   // key state.
   static UIHandlePtr HitTest
      (std::weak_ptr<SelectHandle> &holder,
       const TrackPanelMouseState &state, const AudacityProject *pProject,
       const std::shared_ptr<Track> &pTrack);

   SelectHandle &operator=(const SelectHandle&) = default;
   
   virtual ~SelectHandle();

   bool IsClicked() const;

   void SetUseSnap(bool use);
   void Enter(bool forward) override;

   bool HasSnap() const;
   bool HasEscape() const override;

   bool Escape() override;

   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseState &state, const AudacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(AudacityProject*) override;

   void DrawExtras
      (DrawingPass pass,
      wxDC * dc, const wxRegion &updateRegion, const wxRect &panelRect)
      override;

   static UIHandle::Result NeedChangeHighlight
      (const SelectHandle &oldState,
       const SelectHandle &newState);

private:
   void Connect(AudacityProject *pProject);

   void StartSelection(AudacityProject *pProject);
   void AdjustSelection
      (AudacityProject *pProject,
       ViewInfo &viewInfo, int mouseXCoordinate, int trackLeftEdge,
       Track *pTrack);
   void AssignSelection(ViewInfo &viewInfo, double selend, Track *pTrack);

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

   std::shared_ptr<SnapManager> mSnapManager;
   SnapResults mSnapStart, mSnapEnd;
   bool mUseSnap{ true };

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
   std::shared_ptr<SpectrumAnalyst> mFrequencySnapper;

   int mMostRecentX{ -1 }, mMostRecentY{ -1 };

   bool mAutoScrolling{};

   std::shared_ptr<SelectionStateChanger> mSelectionStateChanger;

   class TimerHandler;
   friend TimerHandler;
   std::shared_ptr<TimerHandler> mTimerHandler;
};
#endif
