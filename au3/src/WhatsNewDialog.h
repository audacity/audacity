/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WhatsNewDialog.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include "wxPanelWrapper.h"

class wxCheckBox;
class ShuttleGui;
class AudacityProject;

class WhatsNewDialog final : public wxDialogWrapper
{
    wxCheckBox* mDontShowAgain{};
public:
    WhatsNewDialog(wxWindow* parent, wxWindowID id);
    ~WhatsNewDialog() override;

    static void Show(AudacityProject& project);

private:
    void Populate(ShuttleGui& S);
    void OnOK(wxCommandEvent&);

    DECLARE_EVENT_TABLE()
};
