/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UserImage.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "wxPanelWrapper.h"

#include <wx/bitmap.h>

namespace cloud::audiocom
{
class UserImage final : public wxPanelWrapper
{
public:
   UserImage(wxWindow* parent, const wxSize& size);

   void SetBitmap(const wxBitmap& bitmap);
   void SetBitmap(const wxString& path);

private:
   void OnPaint();

   wxBitmap mBitmap;
}; // class UserImage
} // namespace cloud::audiocom
