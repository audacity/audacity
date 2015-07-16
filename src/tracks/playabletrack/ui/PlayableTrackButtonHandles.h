/**********************************************************************

Audacity: A Digital Audio Editor

PlayableTrackButtonHandles.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_PLAYABLE_TRACK_BUTTON_HANDLES__
#define __AUDACITY_PLAYABLE_TRACK_BUTTON_HANDLES__

#include "../../ui/ButtonHandle.h"

struct HitTestResult;

class MuteButtonHandle final : public ButtonHandle
{
   MuteButtonHandle(const MuteButtonHandle&) = delete;
   MuteButtonHandle &operator=(const MuteButtonHandle&) = delete;

   MuteButtonHandle();
   virtual ~MuteButtonHandle();
   static MuteButtonHandle& Instance();

protected:
   Result CommitChanges
      (const wxMouseEvent &event, AudacityProject *pProject, wxWindow *pParent)
      override;

   bool StopsOnKeystroke () override { return true; }

public:
   static HitTestResult HitTest
      (const wxMouseEvent &event, const wxRect &rect,
       const AudacityProject *pProject, const Track *pTrack);
};

////////////////////////////////////////////////////////////////////////////////

class SoloButtonHandle final : public ButtonHandle
{
   SoloButtonHandle(const SoloButtonHandle&) = delete;
   SoloButtonHandle &operator=(const SoloButtonHandle&) = delete;

   SoloButtonHandle();
   virtual ~SoloButtonHandle();
   static SoloButtonHandle& Instance();

protected:
   Result CommitChanges
      (const wxMouseEvent &event, AudacityProject *pProject, wxWindow *pParent)
      override;

   bool StopsOnKeystroke () override { return true; }

public:
   static HitTestResult HitTest
      (const wxMouseEvent &event, const wxRect &rect,
       const AudacityProject *pProject, const Track *pTrack);
};

#endif
