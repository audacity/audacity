/**********************************************************************

Audacity: A Digital Audio Editor

CommonTrackPanelCell.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_COMMON_TRACK_PANEL_CELL__
#define __AUDACITY_COMMON_TRACK_PANEL_CELL__

#include "../../TrackPanelCell.h"
#include <stdlib.h>

class Track;

class AUDACITY_DLL_API CommonTrackPanelCell /* not final */
   : public TrackPanelCell
{
public:
   CommonTrackPanelCell() {}

   virtual ~CommonTrackPanelCell() = 0;

   virtual Track *FindTrack() = 0;

protected:
};

#endif
