/**********************************************************************

Sneedacity: A Digital Audio Editor

TrackControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __SNEEDACITY_TRACK_CONTROLS__
#define __SNEEDACITY_TRACK_CONTROLS__

#include "CommonTrackPanelCell.h"

class Track;

class SNEEDACITY_DLL_API TrackControls /* not final */ : public CommonTrackCell
   , public std::enable_shared_from_this< TrackControls >
{
public:
   static TrackControls &Get( Track &track );
   static const TrackControls &Get( const Track &track );

   explicit
   TrackControls( std::shared_ptr<Track> pTrack );

   virtual ~TrackControls() = 0;
};

#include "../../AttachedVirtualFunction.h"

struct DoGetControlsTag;

using DoGetControls =
AttachedVirtualFunction<
   DoGetControlsTag,
   std::shared_ptr< TrackControls >,
   Track
>;

#endif
