/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file UpdateNoticeDialog.h
 @brief Define a dialog to notify the user about automatic update checking.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include "wxPanelWrapper.h"
#include "wx/string.h"

class HtmlWindow;
class wxWindow;

//! Dialog, that notifies the users about automatic updates checking
class UpdateNoticeDialog final : public wxDialogWrapper
{
    DECLARE_DYNAMIC_CLASS (AboutDialog)
public:
    explicit UpdateNoticeDialog (wxWindow* parent);

 private:
    void OnOk (wxCommandEvent&);
    void OnSize(wxSizeEvent&);

    DECLARE_EVENT_TABLE ()
};
