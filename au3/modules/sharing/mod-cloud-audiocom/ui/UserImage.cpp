/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UserImage.cpp

  Dmitry Vedenko

**********************************************************************/
#include "UserImage.h"

#include <memory>

#include <wx/dcbuffer.h>
#include <wx/graphics.h>

namespace audacity::cloud::audiocom {
UserImage::UserImage(wxWindow* parent, const wxSize& size)
    : wxPanelWrapper(parent, wxID_ANY, wxDefaultPosition, size, wxBORDER_NONE)
{
    SetBackgroundStyle(wxBG_STYLE_PAINT);

    SetMinSize(size);
    SetMaxSize(size);

    Bind(wxEVT_PAINT, [this](auto&) { OnPaint(); });
}

void UserImage::SetBitmap(const wxBitmap& bitmap)
{
    mBitmap = bitmap;
    Refresh(true);
}

void UserImage::SetBitmap(const wxString& path)
{
    wxImage image(path);

    if (image.IsOk()) {
        mBitmap = wxBitmap(image);
        Refresh(true);
    }
}

void UserImage::OnPaint()
{
    wxAutoBufferedPaintDC dc(this);
    std::unique_ptr<wxGraphicsContext> gc(wxGraphicsContext::Create(dc));

    gc->SetInterpolationQuality(wxINTERPOLATION_BEST);

    const wxSize size = GetSize();

    gc->SetBrush(wxBrush(GetBackgroundColour()));
    gc->DrawRectangle(0, 0, size.x, size.y);

    if (!mBitmap.IsOk()) {
        gc->SetBrush(wxBrush(wxColour(255, 255, 255, 255)));
        gc->DrawEllipse(0, 0, size.x, size.y);
    } else {
        const wxSize resultingImageSize { std::min(size.x, mBitmap.GetWidth()),
                                          std::min(size.y, mBitmap.GetHeight()) };

        gc->DrawBitmap(
            mBitmap,
            (size.x - resultingImageSize.x) / 2,
            (size.y - resultingImageSize.y) / 2,
            resultingImageSize.x,
            resultingImageSize.y);

        auto path = gc->CreatePath();

        path.MoveToPoint(0, 0);
        path.AddLineToPoint(size.x / 2, 0);
        path.AddArc(size.x / 2, size.y / 2, size.x / 2, 3 * M_PI_2, M_PI, false);
        path.CloseSubpath();

        path.MoveToPoint(size.x, 0);
        path.AddLineToPoint(size.x, size.y / 2);
        path.AddArc(size.x / 2, size.y / 2, size.x / 2, 0, 3 * M_PI_2, false);
        path.CloseSubpath();

        path.MoveToPoint(size.x, size.y);
        path.AddLineToPoint(size.x / 2, size.y);
        path.AddArc(size.x / 2, size.y / 2, size.x / 2, M_PI_2, 0, false);
        path.CloseSubpath();

        path.MoveToPoint(0, size.y);
        path.AddLineToPoint(0, size.y / 2);
        path.AddArc(size.x / 2, size.y / 2, size.x / 2, M_PI, M_PI_2, false);
        path.CloseSubpath();

        gc->DrawPath(path);
    }
}
} // namespace audacity::cloud::audiocom
