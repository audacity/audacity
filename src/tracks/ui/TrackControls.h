/**********************************************************************

Audacity: A Digital Audio Editor

TrackControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TRACK_CONTROLS__
#define __AUDACITY_TRACK_CONTROLS__

#include "CommonTrackPanelCell.h"

class Track;

class TrackControls /* not final */ : public CommonTrackPanelCell
{
public:
   TrackControls() : mpTrack(NULL) {}

   virtual ~TrackControls() = 0;

   Track *GetTrack() const { return mpTrack; }

protected:
   // An override is supplied for derived classes to call through but it is
   // still marked pure virtual
   virtual HitTestResult HitTest
      (const TrackPanelMouseEvent &event,
       const AudacityProject *) override = 0;

   Track *FindTrack() override;

   friend class Track;
   Track *mpTrack;
};

#endif
