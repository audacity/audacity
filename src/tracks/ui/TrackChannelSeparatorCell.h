/**********************************************************************

Audacity: A Digital Audio Editor

TrackChannelSeparatorCell.h

Vitaly Sverchinsky

**********************************************************************/


#pragma once

#include "CommonTrackPanelCell.h"

// Base class for separators used between channels of the track,
// see TrackView class for details.
class AUDACITY_DLL_API TrackChannelSeparatorCell 
    : public CommonTrackPanelCell
{
public:
    virtual int GetHeight() = 0;
};
