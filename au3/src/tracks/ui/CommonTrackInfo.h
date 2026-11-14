/**********************************************************************

Audacity: A Digital Audio Editor

CommonTrackInfo.h

Paul Licameli split from TrackInfo.h

**********************************************************************/
#ifndef __AUDACITY_COMMON_TRACK_INFO__
#define __AUDACITY_COMMON_TRACK_INFO__

#include "TrackInfo.h"

class Channel;
class wxPoint;

namespace CommonTrackInfo {
using TCPLine = TrackInfo::TCPLine;
using TCPLines = TrackInfo::TCPLines;

constexpr static auto Margin = 4;
constexpr static auto ToolButtonSize = 16;

AUDACITY_DLL_API
unsigned MinimumTrackHeight();

AUDACITY_DLL_API
unsigned DefaultTrackHeight(const TCPLines& topLines);

AUDACITY_DLL_API
void DrawItems(TrackPanelDrawingContext& context, const wxRect& rect, const Track& track);

AUDACITY_DLL_API
void DrawItems(TrackPanelDrawingContext& context, const wxRect& rect, const Track* pTrack, const std::vector<TCPLine>& topLines,
               const std::vector<TCPLine>& bottomLines);

AUDACITY_DLL_API
void DrawCloseButton(TrackPanelDrawingContext& context, const wxRect& bev, const Channel* pChannel, UIHandle* target);

AUDACITY_DLL_API
void CloseTitleDrawFunction(TrackPanelDrawingContext& context, const wxRect& rect, const Track* pTrack);

AUDACITY_DLL_API
void SyncLockDrawFunction(TrackPanelDrawingContext& context, const wxRect& rect, const Track* pTrack);

AUDACITY_DLL_API
void GetCloseBoxHorizontalBounds(const wxRect& rect, wxRect& dest);
AUDACITY_DLL_API
void GetCloseBoxRect(const wxRect& rect, wxRect& dest);

AUDACITY_DLL_API
void GetTrackTitleHorizontalBounds(const wxRect& rect, wxRect& dest);
AUDACITY_DLL_API
void GetTrackTitleRect(const wxRect& rect, wxRect& dest);

AUDACITY_DLL_API
void GetSliderHorizontalBounds(const wxRect& rect, wxRect& dest);

AUDACITY_DLL_API
void GetTrackMenuButtonBounds(const wxRect& rect, wxRect& dest);
AUDACITY_DLL_API
void GetTrackMenuButtonRect(const wxRect& rect, wxRect& dest);

AUDACITY_DLL_API
void GetMinimizeHorizontalBounds(const wxRect& rect, wxRect& dest);
AUDACITY_DLL_API
void GetMinimizeRect(const wxRect& rect, wxRect& dest);

AUDACITY_DLL_API
void GetSyncLockHorizontalBounds(const wxRect& rect, wxRect& dest);
AUDACITY_DLL_API
void GetSyncLockIconRect(const wxRect& rect, wxRect& dest);

AUDACITY_DLL_API
bool HideTopItem(const wxRect& rect, const wxRect& subRect, int allowance = 0);

// To help subclasses define GetTCPLines
AUDACITY_DLL_API
const TCPLines& StaticTCPLines();
}

#endif
