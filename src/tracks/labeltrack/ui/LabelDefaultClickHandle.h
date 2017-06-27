/**********************************************************************

Audacity: A Digital Audio Editor

LabelDefaultClickHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_LABEL_DEFAULT_CLICK_HANDLE__
#define __AUDACITY_LABEL_DEFAULT_CLICK_HANDLE__

#include "../../../UIHandle.h"
#include "../../../MemoryX.h"

class wxMouseEvent;
struct HitTestResult;
class LabelTrack;

// Used as a base class.
// Adds some behavior to clicks.
class LabelDefaultClickHandle /* not final */ : public UIHandle
{
   LabelDefaultClickHandle(const LabelDefaultClickHandle&) = delete;
   LabelDefaultClickHandle &operator=(const LabelDefaultClickHandle&) = delete;

public:
   LabelDefaultClickHandle();
   virtual ~LabelDefaultClickHandle();

   Result Click
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, AudacityProject *pProject) override;

   // does not override Preview()

   Result Release
      (const TrackPanelMouseEvent &event, AudacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(AudacityProject *pProject) override;

private:
   struct LabelState;
   std::unique_ptr< LabelState > mLabelState;
   void SaveState( AudacityProject *pProject );
   void RestoreState( AudacityProject *pProject );
};

#endif
