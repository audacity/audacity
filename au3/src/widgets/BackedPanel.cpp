//
//  BackedPanel.cpp
//  Audacity
//
//  Created by Paul Licameli on 5/7/16.
//
//

#include "BackedPanel.h"

BackedPanel::BackedPanel(wxWindow* parent, wxWindowID id,
                         const wxPoint& pos,
                         const wxSize& size,
                         long style)
    : wxPanelWrapper(parent, id, pos, size, style)
    , mBacking{std::make_unique<wxBitmap>(1, 1, 24)}
{
    // Preinit the backing DC and bitmap so routines that require it will
    // not cause a crash if they run before the panel is fully initialized.
    mBackingDC.SelectObject(*mBacking);
}

BackedPanel::~BackedPanel()
{
    if (mBacking) {
        mBackingDC.SelectObject(wxNullBitmap);
    }
}

wxDC& BackedPanel::GetBackingDC()
{
    return mBackingDC;
}

wxDC& BackedPanel::GetBackingDCForRepaint()
{
    if (mResizeBacking) {
        // Reset
        mResizeBacking = false;

        ResizeBacking();
    }

    return mBackingDC;
}

void BackedPanel::ResizeBacking()
{
    if (mBacking) {
        mBackingDC.SelectObject(wxNullBitmap);
    }

    wxSize sz = GetClientSize();
    mBacking = std::make_unique<wxBitmap>();
    // Bug 2040 - Avoid 0 x 0 bitmap when minimized.
    mBacking->Create(std::max(sz.x, 1), std::max(sz.y, 1), 24); //, *dc);
    mBackingDC.SelectObject(*mBacking);
}

void BackedPanel::RepairBitmap(wxDC& dc, wxCoord x, wxCoord y, wxCoord width, wxCoord height)
{
    dc.Blit(x, y, width, height, &mBackingDC, x, y);
}

void BackedPanel::DisplayBitmap(wxDC& dc)
{
    if (mBacking) {
        RepairBitmap(dc, 0, 0, mBacking->GetWidth(), mBacking->GetHeight());
    }
}

void BackedPanel::OnSize(wxSizeEvent& event)
{
    // Tell OnPaint() to recreate the backing bitmap
    mResizeBacking = true;
    event.Skip();
    // Refresh the entire area.  Really only need to refresh when
    // expanding...is it worth the trouble?
    Refresh();
}

BEGIN_EVENT_TABLE(BackedPanel, wxPanelWrapper)
EVT_SIZE(BackedPanel::OnSize)
END_EVENT_TABLE()
