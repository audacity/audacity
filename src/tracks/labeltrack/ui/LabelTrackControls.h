/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_LABEL_TRACK_CONTROLS__
#define __AUDACITY_LABEL_TRACK_CONTROLS__

#include "../../ui/TrackControls.h"

class LabelTrackControls final : public TrackControls
{
   LabelTrackControls();
   LabelTrackControls(const LabelTrackControls&) = delete;
   LabelTrackControls &operator=(const LabelTrackControls&) = delete;

public:
   static LabelTrackControls &Instance();
   ~LabelTrackControls();

   HitTestResult HitTest
      (const TrackPanelMouseEvent &event,
       const AudacityProject *pProject) override;
};

#endif
