/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NoUpdatesAvailableDialog.h
  @brief Define a dialog with Information about no updates available.

  Anton Gerasimov

**********************************************************************/

#pragma once

#include "wxPanelWrapper.h"

//! Information dialog about no updates available, that allows to navigate to settings quickly
class NoUpdatesAvailableDialog final : public wxDialogWrapper
{
public:
    explicit NoUpdatesAvailableDialog(wxWindow* parent);
    virtual ~NoUpdatesAvailableDialog();

    void OnOk(wxCommandEvent& event);

    DECLARE_EVENT_TABLE()
};
