/**********************************************************************

Audacity: A Digital Audio Editor

BackgroundCell.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_BACKGROUND_CELL__
#define __AUDACITY_BACKGROUND_CELL__

#include "CommonTrackPanelCell.h"

class AudacityProject;

class BackgroundCell final : public CommonTrackPanelCell
{
public:
   BackgroundCell(AudacityProject *pProject)
      : mpProject(pProject)
   {}

   virtual ~BackgroundCell();

protected:
   HitTestResult HitTest
      (const TrackPanelMouseState &state,
      const AudacityProject *pProject) override;

   std::shared_ptr<Track> FindTrack() override;

private:
   AudacityProject *mpProject;
};

#endif
