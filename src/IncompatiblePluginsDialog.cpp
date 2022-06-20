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
#include "Menus.h"
#include "PluginRegistrationDialog.h"

IncompatiblePluginsDialog::IncompatiblePluginsDialog(
   wxWindow* parent,
   wxWindowID id,
   const std::vector<wxString>& plugins,
   const wxPoint& pos,
   const wxSize& size)
      : wxDialogWrapper(parent, id, XO("New Plugins"), pos, size, wxDEFAULT_DIALOG_STYLE, XO("New Plugins"))
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

   mPluginList = safenew wxTextCtrl(this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_READONLY | wxTE_MULTILINE | wxNO_BORDER);
   layout->Add(mPluginList, 1, wxEXPAND | wxLEFT | wxRIGHT, 40);

   {
      auto buttonsLayout = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
      auto pluginManagerButton = safenew wxButton(this, wxID_ANY, _("Manage Plug-ins"));
      pluginManagerButton->Bind(wxEVT_BUTTON, &IncompatiblePluginsDialog::OnPluginManagerClicked, this);
      buttonsLayout->Add(pluginManagerButton);

      buttonsLayout->AddSpacer(15);

      auto continueButton = safenew wxButton(this, wxID_ANY, _("Continue"));
      continueButton->Bind(wxEVT_BUTTON, &IncompatiblePluginsDialog::OnContinueClicked, this);
      buttonsLayout->Add(continueButton);

      layout->Add(buttonsLayout.release(), 0, wxALIGN_RIGHT | wxALL, 15);
   }
   if(!plugins.empty())
      SetPlugins(plugins);

   SetSizer(layout.release());
}

void IncompatiblePluginsDialog::SetPlugins(const std::vector<wxString>& plugins)
{
   mText->SetLabelText(XO(
      "Audacity has found %d incompatible plugins which could "\
      "not be loaded and are disabled. If you wish to enable them, "\
      "you can do so from \"Manage Plug-Ins\" or click \"Continue\".")
      .Format(static_cast<int>(plugins.size())).Translation());
   mText->Wrap(GetClientSize().GetWidth() - 80);

   wxString pluginListText;
   for(const auto& path : plugins)
      pluginListText += path + "\n";

   mPluginList->SetValue(pluginListText);
}

void IncompatiblePluginsDialog::OnPluginManagerClicked(wxCommandEvent&)
{
   Destroy();
   BasicUI::CallAfter([]()
   {
      PluginRegistrationDialog dlg(nullptr);
      if(dlg.ShowModal() == wxID_OK)
         MenuCreator::RebuildAllMenuBars();
   });
}

void IncompatiblePluginsDialog::OnContinueClicked(wxCommandEvent&)
{
   Destroy();
}
