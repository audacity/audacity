/**********************************************************************

Audacity: A Digital Audio Editor

TrackSelectHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TRACK_SELECT_HANDLE__
#define __AUDACITY_TRACK_SELECT_HANDLE__

#include "../../UIHandle.h"

class wxMouseEvent;
struct HitTestResult;
class Track;

class TrackSelectHandle final : public UIHandle
{
   TrackSelectHandle();
   TrackSelectHandle(const TrackSelectHandle&) = delete;
   TrackSelectHandle &operator=(const TrackSelectHandle&) = delete;
   static TrackSelectHandle& Instance();
   static HitTestPreview HitPreview(unsigned trackCount);

public:
   static HitTestResult HitAnywhere(unsigned trackCount);

   virtual ~TrackSelectHandle();

   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   HitTestPreview Preview
      (const TrackPanelMouseEvent &event, const AudacityProject *pProject)
      override;

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(AudacityProject *) override;

   bool StopsOnKeystroke() override { return true; }

private:
   Track *mpTrack {};

   // JH: if the user is dragging a track, at what y
   //   coordinate should the dragging track move up or down?
   int mMoveUpThreshold {};
   int mMoveDownThreshold {};
   int mRearrangeCount {};

   void CalculateRearrangingThresholds(const wxMouseEvent & event);
};

#endif
