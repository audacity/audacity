/**********************************************************************

  Audacity: A Digital Audio Editor

  MissingPluginsDialogError.h

**********************************************************************/

#pragma once

#include "wxPanelWrapper.h"

/// An error dialog about missing plugins
class MissingPluginsErrorDialog final : public wxDialogWrapper
{
public:
    explicit MissingPluginsErrorDialog(wxWindow* parent);
    virtual ~MissingPluginsErrorDialog();

    void OnOk(wxCommandEvent& event);

    DECLARE_EVENT_TABLE()
};
