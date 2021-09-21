/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file UpdatePopupDialog.h
 @brief Define a dialog for notifying users about new version available.

 Anton Gerasimov
 **********************************************************************/

#pragma once

#include "widgets/wxPanelWrapper.h"
#include "wx/string.h"

#include "VersionPatch.h"

class HtmlWindow;
class wxWindow;

/// Show dialog window with update information for the user.
class UpdatePopupDialog final : public wxDialogWrapper
{
    DECLARE_DYNAMIC_CLASS (AboutDialog)
public:
    explicit UpdatePopupDialog (wxWindow* parent, const VersionPatch& versionPatch, bool configurableNotification);
    virtual ~UpdatePopupDialog();

    void OnUpdate (wxCommandEvent& event);
    void OnSkip (wxCommandEvent& event);
    void OnDontShow (wxCommandEvent& event);

    DECLARE_EVENT_TABLE()

private:
    HtmlWindow* AddHtmlContent (wxWindow* parent);

    const VersionPatch& mVersionPatch;
};
