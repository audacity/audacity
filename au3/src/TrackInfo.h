/**********************************************************************

Audacity: A Digital Audio Editor

TrackInfo.h

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#ifndef __AUDACITY_TRACK_INFO__
#define __AUDACITY_TRACK_INFO__

#include <vector>

class wxDC;
class wxRect;
class wxWindow;
class Track;
struct TrackPanelDrawingContext;
class UIHandle;

namespace TrackInfo {
struct TCPLine {
    enum : unsigned {
        // The sequence is not significant, just keep bits distinct
        kItemBarButtons       = 1 << 0,
        kItemStatusInfo1      = 1 << 1,
        kItemMute             = 1 << 2,
        kItemSolo             = 1 << 3,
        kItemVolume           = 1 << 4,
        kItemPan              = 1 << 5,
        kItemVelocity         = 1 << 6,
        kItemMidiControlsRect = 1 << 7,
        kItemSyncLock         = 1 << 9,
        kItemStatusInfo2      = 1 << 10,
        kItemEffects          = 1 << 11,

        kHighestBottomItem = kItemSyncLock,
    };

    using DrawFunction = void (*)(
        TrackPanelDrawingContext& context,
        const wxRect& rect,
        const Track* maybeNULL
        );

    unsigned items;   // a bitwise OR of values of the enum above
    int height;
    int extraSpace;
    DrawFunction drawFunction;
};

using TCPLines = std::vector< TCPLine >;

// return y value and height
AUDACITY_DLL_API
std::pair< int, int > CalcItemY(const TCPLines& lines, unsigned iItem);

AUDACITY_DLL_API
void SetTrackInfoFont(wxDC* dc);
}

#endif
