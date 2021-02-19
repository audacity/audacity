/**********************************************************************

Audacity: A Digital Audio Editor

@file HelpUtilities.cpp

Paul Licameli split from HelpMenus.cpp

**********************************************************************/

#include "HelpUtilities.h"

#include "Project.h"
#include "ProjectWindows.h"
#include "SelectFile.h"
#include "ShuttleGui.h"
#include "widgets/AudacityMessageBox.h"
#include "wxPanelWrapper.h"

#include <wx/button.h>
#include <wx/frame.h>
#include <wx/textctrl.h>

void ShowDiagnostics(
   AudacityProject &project, const wxString &info,
   const TranslatableString &description, const wxString &defaultPath,
   bool fixedWidth)
{
   auto &window = GetProjectFrame( project );
   wxDialogWrapper dlg( &window, wxID_ANY, description);
   dlg.SetName();
   ShuttleGui S(&dlg, eIsCreating);

   wxTextCtrl *text;
   S.StartVerticalLay();
   {
      text = S.Id(wxID_STATIC)
         .Style(wxTE_MULTILINE | wxTE_READONLY | wxTE_RICH)
         .AddTextWindow("");

      wxButton *save = safenew wxButton(S.GetParent(), wxID_OK, _("&Save"));
      S.AddStandardButtons(eCancelButton, save);
   }
   S.EndVerticalLay();

   if (fixedWidth) {
      auto style = text->GetDefaultStyle();
      style.SetFontFamily( wxFONTFAMILY_TELETYPE );
      text->SetDefaultStyle(style);
   }

   *text << info;

   dlg.SetSize(350, 450);

   if (dlg.ShowModal() == wxID_OK)
   {
      const auto fileDialogTitle = XO("Save %s").Format( description );
      wxString fName = SelectFile(FileNames::Operation::Export,
         fileDialogTitle,
         wxEmptyString,
         defaultPath,
         wxT("txt"),
         { FileNames::TextFiles },
         wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
         &window);
      if (!fName.empty())
      {
         if (!text->SaveFile(fName))
         {
            AudacityMessageBox(
               XO("Unable to save %s").Format( description ),
               fileDialogTitle);
         }
      }
   }
}
