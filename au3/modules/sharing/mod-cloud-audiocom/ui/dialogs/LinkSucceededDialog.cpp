/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LinkSucceededDialog.cpp

  Dmitry Vedenko

**********************************************************************/
#include "LinkSucceededDialog.h"

#include <wx/button.h>

#include "CodeConversions.h"
#include "ServiceConfig.h"

#include "ShuttleGui.h"

#include "HelpSystem.h"

namespace audacity::cloud::audiocom {
LinkSucceededDialog::LinkSucceededDialog(wxWindow* parent)
    : wxDialogWrapper(
        parent, wxID_ANY, XO("Link account"), wxDefaultPosition, { 442, -1 },
        wxDEFAULT_DIALOG_STYLE)
{
    SetMinSize({ 442, -1 });

    ShuttleGui s(this, eIsCreating);

    s.StartVerticalLay();
    {
        s.StartInvisiblePanel(16);
        {
            s.SetBorder(0);

            s.AddFixedText(XO("Account linked successfully!"), false, 410);

            s.AddSpace(0, 16, 0);

            s.StartHorizontalLay(wxEXPAND, 0);
            {
                s.AddSpace(1, 0, 1);

                auto btn = s.AddButton(XO("&OK"));

                btn->Bind(wxEVT_BUTTON, [this](auto) { EndModal(wxID_OK); });
                btn->SetDefault();
            }
            s.EndHorizontalLay();
        }
        s.EndInvisiblePanel();
    }
    s.EndVerticalLay();

    Layout();
    Fit();
    Center();

    Bind(
        wxEVT_CHAR_HOOK,
        [this](auto& evt)
    {
        if (!IsEscapeKey(evt)) {
            evt.Skip();
            return;
        }

        EndModal(wxID_OK);
    });
}

LinkSucceededDialog::~LinkSucceededDialog()
{
}
} // namespace audacity::cloud::audiocom
