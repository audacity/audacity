/**********************************************************************

Audacity: A Digital Audio Editor

CommonTrackInfo.cpp

Paul Licameli split from TrackInfo.cpp

********************************************************************//*!

\namespace CommonTrackInfo
\brief
  Functions for drawing the track control panel, which is shown to the side
  of a track
  It has the menus, pan and gain controls displayed in it.
  So "Info" is somewhat a misnomer. Should possibly be "TrackControls".

  It maintains global slider widget instances that are reparented and
  repositioned as needed for drawing and interaction with the user,
  interoperating with the custom panel subdivision implemented in CellularPanel
  and avoiding wxWidgets sizers

  If we'd instead coded it as a wxWindow, we would have an instance
  of this class for each track displayed.

**********************************************************************/
#include "CommonTrackInfo.h"

#include <wx/dc.h>
#include <wx/dcmemory.h>

#include "AColor.h"
#include "AllThemeResources.h"
#include "SyncLock.h"
#include "Theme.h"
#include "Track.h"
#include "TrackArt.h"
#include "TrackControls.h"
#include "TrackPanelDrawingContext.h"
#include "UIHandle.h"
#include "ViewInfo.h"
#include "tracks/ui/ChannelView.h"

#define RANGE(array) std::begin(array), std::end(array)
using TCPLine = TrackInfo::TCPLine;
using TCPLines = TrackInfo::TCPLines;

static const TCPLines& commonTrackTCPLines()
{
    static const TCPLines theLines{
        { TCPLine::kItemBarButtons, kTrackInfoTitleHeight, kTrackInfoTitleExtra,
          &CommonTrackInfo::CloseTitleDrawFunction },
    };
    return theLines;
}

const TCPLines& CommonTrackInfo::StaticTCPLines()
{
    return commonTrackTCPLines();
}

namespace {
constexpr auto Padding = 2;

int totalTCPLines(const TCPLines& lines, bool omitLastExtra)
{
    int total = 0;
    int lastExtra = 0;
    for ( const auto line : lines ) {
        lastExtra = line.extraSpace;
        total += line.height + lastExtra;
    }
    if (omitLastExtra) {
        total -= lastExtra;
    }
    return total;
}
}

namespace {
// Items for the bottom of the panel, listed bottom-upwards
// As also with the top items, the extra space is below the item
const TrackInfo::TCPLine defaultCommonTrackTCPBottomLines[] = {
    // The '0' avoids impinging on bottom line of TCP
    // Use -1 if you do want to do so.
    { TCPLine::kItemSyncLock, kTrackInfoBtnSize, 0,
      &CommonTrackInfo::SyncLockDrawFunction },
};
TCPLines commonTrackTCPBottomLines{ RANGE(defaultCommonTrackTCPBottomLines) };

// return y value and height
std::pair< int, int > CalcBottomItemY
    (const TCPLines& lines, unsigned iItem, int height)
{
    int y = height;
    auto pLines = lines.begin();
    while (pLines != lines.end()
           && 0 == (pLines->items & iItem)) {
        y -= pLines->height + pLines->extraSpace;
        ++pLines;
    }
    if (pLines != lines.end()) {
        y -= (pLines->height + pLines->extraSpace);
    }
    return { y, pLines->height };
}
}

unsigned CommonTrackInfo::MinimumTrackHeight()
{
    unsigned height = Margin * 2;
    if (!commonTrackTCPLines().empty()) {
        height += commonTrackTCPLines().front().height;
    }
    if (!commonTrackTCPBottomLines.empty()) {
        height += commonTrackTCPBottomLines.front().height;
    }
    // + 1 prevents the top item from disappearing for want of enough space,
    // according to the rules in HideTopItem.
    return height + kVerticalPadding + 1;
}

bool CommonTrackInfo::HideTopItem(const wxRect& rect, const wxRect& subRect,
                                  int allowance)
{
    auto limit = CalcBottomItemY
                     (commonTrackTCPBottomLines, TCPLine::kHighestBottomItem, rect.height).first;
    // Return true if the rectangle is even touching the limit
    // without an overlap.  That was the behavior as of 2.1.3.
    return subRect.y + subRect.height - allowance >= rect.y + limit;
}

void CommonTrackInfo::DrawItems
    (TrackPanelDrawingContext& context,
    const wxRect& rect, const Track& track)
{
    auto& trackControl = TrackControls::Get(track);
    const auto& topLines = trackControl.GetTCPLines();
    const auto& bottomLines = commonTrackTCPBottomLines;
    DrawItems
        (context, rect, &track, topLines, bottomLines);
}

void CommonTrackInfo::DrawItems
    (TrackPanelDrawingContext& context,
    const wxRect& rect_, const Track* pTrack,
    const std::vector<TCPLine>& topLines, const std::vector<TCPLine>& bottomLines)
{
    const auto rect = wxRect(rect_).Deflate(Margin);
    auto dc = &context.dc;
    TrackInfo::SetTrackInfoFont(dc);
    dc->SetTextForeground(theTheme.Colour(clrTrackPanelText));

    {
        int yy = 0;
        for ( const auto& line : topLines ) {
            wxRect itemRect{
                rect.x, rect.y + yy,
                rect.width, line.height
            };
            if (!CommonTrackInfo::HideTopItem(rect, itemRect)
                && line.drawFunction) {
                line.drawFunction(context, itemRect, pTrack);
            }
            yy += line.height + line.extraSpace;
        }
    }
    {
        int yy = rect.height;
        for ( const auto& line : bottomLines ) {
            yy -= line.height + line.extraSpace;
            if (line.drawFunction) {
                wxRect itemRect{
                    rect.x, rect.y + yy,
                    rect.width, line.height
                };
                line.drawFunction(context, itemRect, pTrack);
            }
        }
    }
}

void CommonTrackInfo::DrawCloseButton(
    TrackPanelDrawingContext& context, const wxRect& bev,
    const Channel* pChannel, UIHandle* target)
{
    auto dc = &context.dc;
    auto pTrack = pChannel
                  ? dynamic_cast<const Track*>(&pChannel->GetChannelGroup())
                  : nullptr;
    bool selected = pTrack ? pTrack->GetSelected() : true;
    bool hit = target && target->FindTrack().get() == pTrack;
    bool captured = hit && target->IsDragging();
    bool down = captured && bev.Contains(context.lastState.GetPosition());

    wxMemoryDC memDC;

    if (selected) {
        memDC.SelectObject(theTheme.Bitmap(bmpCloseHover));
    } else if (down) {
        memDC.SelectObject(theTheme.Bitmap(bmpCloseDown));
    } else {
        memDC.SelectObject(theTheme.Bitmap(bmpCloseNormal));
    }

    AColor::Bevel2(*dc, !down, bev, selected, hit);

    dc->Blit(bev.GetLeft(), bev.GetTop(), bev.width, bev.height, &memDC, 2, 2);
}

namespace {
void DrawToolButtonBackground(TrackPanelDrawingContext& context, const wxRect& rect, bool captured)
{
    const auto hovered = rect.Contains(context.lastState.GetPosition());
    if (captured && hovered) {
        AColor::DrawFrame(context.dc, rect, theTheme.Bitmap(bmpHiliteButtonSmall), 11);
    } else if (hovered) {
        AColor::DrawFrame(context.dc, rect, theTheme.Bitmap(bmpHiliteUpButtonSmall), 11);
    }
}
}

void CommonTrackInfo::CloseTitleDrawFunction
    (TrackPanelDrawingContext& context,
    const wxRect& rect, const Track* pTrack)
{
    const auto dc = &context.dc;

    const auto target = context.target.get();
    const auto hit = target && target->FindTrack().get() == pTrack;
    const auto captured = hit && target->IsDragging();

    { //close button
        wxRect bev = rect;
        GetCloseBoxHorizontalBounds(rect, bev);

        DrawToolButtonBackground(context, bev, captured);

        dc->DrawBitmap(theTheme.Bitmap(tcpClose), bev.GetLeftTop());
    }

    { //track title
        wxRect bev = rect;
        GetTrackTitleHorizontalBounds(rect, bev);
        const auto titleStr = TrackArt::TruncateText(*dc,
                                                     pTrack ? pTrack->GetName() : _("Name"), bev.width);

        TrackInfo::SetTrackInfoFont(dc);
        const auto metrics = dc->GetFontMetrics();

        dc->SetTextForeground(theTheme.Colour(clrTrackPanelText));
        dc->SetTextBackground(wxTRANSPARENT);
        dc->DrawText(titleStr, bev.x + 2, bev.y + (bev.height - (metrics.ascent + metrics.descent)) / 2);
    }

    { //minimize button
        const auto minimized
            =pTrack ? ChannelView::Get(*pTrack->GetChannel(0)).GetMinimized() : false;

        wxRect bev = rect;
        GetMinimizeHorizontalBounds(rect, bev);

        DrawToolButtonBackground(context, bev, captured);

        dc->DrawBitmap(
            minimized
            ? theTheme.Bitmap(tcpChevronDown)
            : theTheme.Bitmap(tcpChevron),
            bev.GetLeftTop());
    }

    { //track menu button
        wxRect bev = rect;
        GetTrackMenuButtonBounds(rect, bev);

        DrawToolButtonBackground(context, bev, captured);

        dc->DrawBitmap(theTheme.Bitmap(tcpEllipses), bev.GetLeftTop());
    }
}

void CommonTrackInfo::SyncLockDrawFunction
    (TrackPanelDrawingContext& context,
    const wxRect& rect, const Track* pTrack)
{
    auto dc = &context.dc;
    bool syncLockSelected
        =pTrack ? SyncLock::IsSyncLockSelected(*pTrack) : true;

    // Draw the sync-lock indicator if this track is in a sync-lock selected group.
    if (syncLockSelected) {
        wxRect syncLockIconRect = rect;

        GetSyncLockHorizontalBounds(rect, syncLockIconRect);
        wxBitmap syncLockBitmap(theTheme.Image(bmpSyncLockIcon));
        // Icon is 12x12 and syncLockIconRect is 16x16.
        dc->DrawBitmap(syncLockBitmap,
                       syncLockIconRect.x + 3,
                       syncLockIconRect.y + 2,
                       true);
    }
}

void CommonTrackInfo::GetCloseBoxHorizontalBounds(const wxRect& rect, wxRect& dest)
{
    dest.x = rect.x;
    dest.width = ToolButtonSize;
}

void CommonTrackInfo::GetCloseBoxRect(const wxRect& rect_, wxRect& dest)
{
    const auto rect = wxRect(rect_).Deflate(Margin);
    GetCloseBoxHorizontalBounds(rect, dest);
    auto results = CalcItemY(commonTrackTCPLines(), TCPLine::kItemBarButtons);
    dest.y = rect.y + results.first;
    dest.height = results.second;
}

void CommonTrackInfo::GetTrackTitleHorizontalBounds(const wxRect& rect, wxRect& dest)
{
    // to right of CloseBoxRect, plus a little more
    wxRect closeRect;
    wxRect minRect;
    GetCloseBoxHorizontalBounds(rect, closeRect);
    GetMinimizeHorizontalBounds(rect, minRect);
    dest.x = rect.x + closeRect.width + Padding;
    dest.width = minRect.x - closeRect.width - closeRect.x;
}

void CommonTrackInfo::GetTrackTitleRect(const wxRect& rect_, wxRect& dest)
{
    const auto rect = wxRect(rect_).Deflate(Margin);
    GetTrackTitleHorizontalBounds(rect, dest);
    auto results = CalcItemY(commonTrackTCPLines(), TCPLine::kItemBarButtons);
    dest.y = rect.y + results.first;
    dest.height = results.second;
}

void CommonTrackInfo::GetSliderHorizontalBounds(const wxRect& rect, wxRect& dest)
{
    dest.x = rect.x + (rect.width - kTrackInfoSliderWidth) / 2;
    dest.width = kTrackInfoSliderWidth;
}

void CommonTrackInfo::GetTrackMenuButtonBounds(const wxRect& rect, wxRect& dest)
{
    dest.x = rect.x + rect.width - ToolButtonSize;
    dest.width = ToolButtonSize;
}

void CommonTrackInfo::GetTrackMenuButtonRect(const wxRect& rect_, wxRect& dest)
{
    const auto rect = wxRect(rect_).Deflate(Margin);
    GetTrackMenuButtonBounds(rect, dest);
    auto results = TrackInfo::CalcItemY(commonTrackTCPLines(), TCPLine::kItemBarButtons);
    dest.y = rect.y + results.first;
    dest.height = ToolButtonSize;
}

void CommonTrackInfo::GetMinimizeHorizontalBounds(const wxRect& rect, wxRect& dest)
{
    GetTrackMenuButtonBounds(rect, dest);
    dest.x -= ToolButtonSize + Padding;
}

void CommonTrackInfo::GetMinimizeRect(const wxRect& rect_, wxRect& dest)
{
    const auto rect = wxRect(rect_).Deflate(Margin);

    GetMinimizeHorizontalBounds(rect, dest);
    const auto results = CalcItemY
                             (commonTrackTCPLines(), TCPLine::kItemBarButtons);
    dest.y = rect.y + results.first;
    dest.height = results.second;
}

void CommonTrackInfo::GetSyncLockHorizontalBounds(const wxRect& rect, wxRect& dest)
{
    dest.width = kTrackInfoBtnSize;
    dest.x = rect.x + rect.width - dest.width;
}

void CommonTrackInfo::GetSyncLockIconRect(const wxRect& rect_, wxRect& dest)
{
    const auto rect = wxRect(rect_).Deflate(Margin);
    GetSyncLockHorizontalBounds(rect, dest);
    auto results = CalcBottomItemY
                       (commonTrackTCPBottomLines, TCPLine::kItemSyncLock, rect.height);
    dest.y = rect.y + results.first;
    dest.height = results.second;
}

unsigned CommonTrackInfo::DefaultTrackHeight(const TCPLines& topLines)
{
    int needed
        =kVerticalPadding + Margin * 2
          + totalTCPLines(topLines, true)
          + totalTCPLines(commonTrackTCPBottomLines, false) + 1;
    return (unsigned)std::max(needed, (int)ChannelView::DefaultHeight);
}
