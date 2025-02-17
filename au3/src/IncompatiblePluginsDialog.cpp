/**********************************************************************

  Audacity: A Digital Audio Editor

  @file IncompatiblePluginsDialog.cpp

  @author Vitaly Sverchinsky

**********************************************************************/
#include "IncompatiblePluginsDialog.h"

#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/button.h>

#include "Internat.h"
#include "BasicUI.h"
#include "MenuCreator.h"
#include "PluginRegistrationDialog.h"

IncompatiblePluginsDialog::IncompatiblePluginsDialog(
    wxWindow* parent,
    wxWindowID id,
    enum ScanType scanType,
    const std::vector<wxString>& plugins,
    const wxPoint& pos,
    const wxSize& size)
    : wxDialogWrapper(parent, id, XO("New Plugins"), pos, size, wxDEFAULT_DIALOG_STYLE, XO("New Plugins"))
    , m_scanType(scanType)
{
    SetSize(635, 414);
    auto layout = std::make_unique<wxBoxSizer>(wxVERTICAL);

    layout->AddSpacer(40);

    auto header = safenew wxStaticText(this, wxID_ANY, _("Incompatible plugin(s) found"));
    header->SetFont(wxFont(wxFontInfo(12).Bold(true)));
    layout->Add(header, 0, wxLEFT | wxRIGHT, 40);
    layout->AddSpacer(10);

    mText = safenew wxStaticText(this, wxID_ANY, wxEmptyString);
    mText->SetFont(wxFont(wxFontInfo(10)));

    layout->Add(mText, 0, wxLEFT | wxRIGHT, 40);
    layout->AddSpacer(40);

    mPluginList
        = safenew wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_READONLY | wxTE_MULTILINE | wxNO_BORDER);
    layout->Add(mPluginList, 1, wxEXPAND | wxLEFT | wxRIGHT, 40);

    auto buttonsLayout = std::make_unique<wxBoxSizer>(wxHORIZONTAL);

    if (m_scanType == ScanType::Startup) {
        auto pluginManagerButton = safenew wxButton(this, wxID_ANY, _("&Manage Plugins"));
        pluginManagerButton->Bind(wxEVT_BUTTON, &IncompatiblePluginsDialog::OnPluginManagerClicked, this);
        buttonsLayout->Add(pluginManagerButton);

        buttonsLayout->AddSpacer(15);
    }

    auto continueButton = safenew wxButton(this, wxID_OK,
                                           m_scanType == ScanType::Startup
                                           ? _("C&ontinue")
                                           : _("&OK")
                                           );
    continueButton->SetDefault();
    continueButton->SetFocusFromKbd();

    buttonsLayout->Add(continueButton);
    layout->Add(buttonsLayout.release(), 0, wxALIGN_RIGHT | wxALL, 15);

    if (!plugins.empty()) {
        SetPlugins(plugins);
    }

    SetSizer(layout.release());
}

void IncompatiblePluginsDialog::SetPlugins(const std::vector<wxString>& plugins)
{
    if (m_scanType == ScanType::Startup) {
        mText->SetLabelText(XO(
                                "Audacity has found %d incompatible plugins which could " \
                                "not be loaded. We have disabled these plugins to avoid any " \
                                "stalling or crashes. If you would still like to attempt " \
                                "to use these plugins, you can enable them using " \
                                "\"Manage Plugins\". Otherwise, select \"Continue\".")
                            .Format(static_cast<int>(plugins.size())).Translation());
    } else {
        mText->SetLabelText(XO(
                                "Audacity has found %d incompatible plugins which could " \
                                "not be loaded. We have disabled these plugins to avoid any " \
                                "stalling or crashes.")
                            .Format(static_cast<int>(plugins.size())).Translation());
    }
    mText->Wrap(GetClientSize().GetWidth() - 80);

    wxString pluginListText;
    for (const auto& path : plugins) {
        pluginListText += path + "\n";
    }

    mPluginList->SetValue(pluginListText);
}

void IncompatiblePluginsDialog::OnPluginManagerClicked(wxCommandEvent&)
{
    Destroy();
    BasicUI::CallAfter([]()
    {
        PluginRegistrationDialog dlg(nullptr);
        if (dlg.ShowModal() == wxID_OK) {
            MenuCreator::RebuildAllMenuBars();
        }
    });
}

void IncompatiblePluginsDialog::OnContinueClicked(wxCommandEvent&)
{
    Close();
}
