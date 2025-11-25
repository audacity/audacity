/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file RoundedStaticBitmap.h

 Dmitry Makarenko

 **********************************************************************/
#pragma once

#include <wx/bitmap.h>
#include <wx/statbmp.h>

wxImage WX_WRAPPERS_API RoundedImage(const wxImage& source, int radius);

class WX_WRAPPERS_API RoundedStaticBitmap final
   : public wxStaticBitmap
{
public:
   RoundedStaticBitmap(wxWindow *parent,
                       wxWindowID id,
                       const wxBitmap& bitmap,
                       int radius,
                       const wxPoint& pos = wxDefaultPosition,
                       const wxSize& size = wxDefaultSize,
                       long style = 0,
                       const wxString& name = wxStaticBitmapNameStr);

   void SetRadius(int radius);
   int GetRadius() const;

#if wxCHECK_VERSION(3, 1, 6)
   void SetBitmap(const wxBitmapBundle& bitmap) override;
#else
   void SetBitmap(const wxBitmap& bitmap) override;
#endif

   void SetImage(const wxImage& image);

private:

   wxImage mImage;
   int mRadius;
};
