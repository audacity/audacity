/**********************************************************************

Audacity: A Digital Audio Editor

TrackControls.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TRACK_CONTROLS__
#define __AUDACITY_TRACK_CONTROLS__

#include "CommonTrackPanelCell.h"
#include "TrackInfo.h"

class Track;

class AUDACITY_DLL_API TrackControls /* not final */ : public CommonTrackCell, public std::enable_shared_from_this< TrackControls >
{
public:
    static TrackControls& Get(Track& track);
    static const TrackControls& Get(const Track& track);

    explicit
    TrackControls(std::shared_ptr<Track> pTrack);

    virtual ~TrackControls() = 0;

    // This decides what details to draw in the track control panel, besides
    // those at the bottom
    virtual const TrackInfo::TCPLines& GetTCPLines() const = 0;
};

#include "AttachedVirtualFunction.h"

struct DoGetControlsTag;

using DoGetControls
    =AttachedVirtualFunction<
          DoGetControlsTag,
          std::shared_ptr< TrackControls >,
          Track
          >;
DECLARE_EXPORTED_ATTACHED_VIRTUAL(AUDACITY_DLL_API, DoGetControls);

#endif
