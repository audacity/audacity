/**********************************************************************

Sneedacity: A Digital Audio Editor

LabelDefaultClickHandle.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __SNEEDACITY_LABEL_DEFAULT_CLICK_HANDLE__
#define __SNEEDACITY_LABEL_DEFAULT_CLICK_HANDLE__

#include "../../../UIHandle.h"

class wxMouseEvent;
class LabelTrack;

// Used as a base class.
// Adds some behavior to clicks.
class LabelDefaultClickHandle /* not final */ : public UIHandle
{
public:
   LabelDefaultClickHandle();
   virtual ~LabelDefaultClickHandle();

   LabelDefaultClickHandle &operator=
      (const LabelDefaultClickHandle&) = default;
   
   Result Click
      (const TrackPanelMouseEvent &event, SneedacityProject *pProject) override;

   Result Drag
      (const TrackPanelMouseEvent &event, SneedacityProject *pProject) override;

   // does not override Preview()

   Result Release
      (const TrackPanelMouseEvent &event, SneedacityProject *pProject,
       wxWindow *pParent) override;

   Result Cancel(SneedacityProject *pProject) override;

private:
   struct LabelState;
   std::shared_ptr< LabelState > mLabelState;
   void SaveState( SneedacityProject *pProject );
   void RestoreState( SneedacityProject *pProject );
};

#endif
