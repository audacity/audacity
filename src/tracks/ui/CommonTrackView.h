/**********************************************************************

Sneedacity: A Digital Audio Editor

CommonTrackView.h

Paul Licameli split from class TrackView

**********************************************************************/

#ifndef __SNEEDACITY_COMMON_TRACK_VIEW__
#define __SNEEDACITY_COMMON_TRACK_VIEW__

#include "TrackView.h" // to inherit

class SelectHandle;
class TimeShiftHandle;

class SNEEDACITY_DLL_API CommonTrackView /* not final */ : public TrackView
{
public:
   using TrackView::TrackView;
   
   // Delegates the handling to the related TCP cell
   std::shared_ptr<TrackPanelCell> ContextMenuDelegate() override;

   // Cause certain overriding tool modes (Zoom; future ones?) to behave
   // uniformly in all tracks, disregarding track contents.
   // Do not further override this...
   std::vector<UIHandlePtr> HitTest
      (const TrackPanelMouseState &, const SneedacityProject *pProject)
      final override;

   void TimeShiftHitTest();

   virtual int GetMinimizedHeight() const override;

protected:
   // Rather override this for subclasses:
   virtual std::vector<UIHandlePtr> DetailedHitTest
      (const TrackPanelMouseState &,
       const SneedacityProject *pProject, int currentTool, bool bMultiTool)
      = 0;

   std::weak_ptr<SelectHandle> mSelectHandle;

public:
   std::weak_ptr<TimeShiftHandle> mTimeShiftHandle;
};

#endif
