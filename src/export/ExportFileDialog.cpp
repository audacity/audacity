/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportFileDialog.cpp

  Dominic Mazzoni
 
  Vitaly Sverchinsky split from Export.cpp

*******************************************************************/

#include "ExportFileDialog.h"
#include "ShuttleGui.h"
#include "Export.h"
#include "Theme.h"
#include "AllThemeResources.h"
#include "HelpSystem.h"
#include "AudacityMessageBox.h"
#include "ExportUtils.h"
#include "ExportOptionsHandler.h"

#include <wx/bmpbuttn.h>
#include <wx/simplebook.h>
#include <wx/filectrl.h>

#include "widgets/FileHistory.h"

BEGIN_EVENT_TABLE(ExportFileDialog, wxEvtHandler)
   EVT_BUTTON(wxID_HELP, ExportFileDialog::OnHelp)
   EVT_COMMAND(wxID_ANY, AUDACITY_FILE_SUFFIX_EVENT, ExportFileDialog::OnExtensionChanged)
END_EVENT_TABLE()

StringSetting DefaultExportFormat{ L"/Export/Format", L"WAV" };

namespace {

bool IsExtension(const ExportPlugin& plugin, int formatIndex, const FileExtension& ext)
{
   if(formatIndex >= 0 && formatIndex < plugin.GetFormatCount())
   {
      auto formatInfo = plugin.GetFormatInfo(formatIndex);
      if(formatInfo.mExtensions[0].empty() || formatInfo.mExtensions.Index(ext, false) != wxNOT_FOUND)
         return true;
   }
   return false;
}

}

ExportFileDialog::ExportFileDialog(wxWindow *parent,
                                   Exporter& exporter,
                                   const wxString& defaultDir,
                                   const wxString& defaultName,
                                   const wxString& defaultFormatName,
                                   const TranslatableString& title,
                                   const wxPoint& pos,
                                   const wxSize& sz,
                                   const wxString& name)
   : mExporter(exporter)
{
   FileNames::FileTypes fileTypes;
   
   const auto& plugins = exporter.GetPlugins();
   
   int pluginIndex{0};
   int formatIndex{0};
   int filterIndex{0};
   {
      bool formatFound {false};
      int i = 0;
      for (const auto &plugin : plugins)
      {
         for (int j = 0; j < plugin->GetFormatCount(); j++)
         {
            auto formatInfo = plugin->GetFormatInfo(j);
            fileTypes.insert(fileTypes.end(), { formatInfo.mDescription, formatInfo.mExtensions });
            if(!formatFound)
            {
               if (formatInfo.mFormat == defaultFormatName) {
                  pluginIndex = i;
                  formatIndex = j;
                  formatFound = true;
               }
               else
                  ++filterIndex;
            }
         }
         ++i;
      }
   }
   
   wxFileName filename;
   filename.SetPath(defaultDir);
   
   if(defaultName.empty())
      filename.SetName(_("untitled"));
   else
      filename.SetFullName(defaultName);
   
   if (!filename.HasExt())
   {
      auto defext = plugins[pluginIndex]->GetFormatInfo(formatIndex).mExtensions[0].Lower();
      filename.SetExt(defext);
   }
   
   wxTabTraversalWrapper<FileDialog>::Create(parent,
                                             title.Translation(),
                                             defaultDir,
                                             filename.GetFullName(),
                                             FileNames::FormatWildcard(fileTypes),
                                             wxRESIZE_BORDER | wxFD_SAVE | wxFD_OVERWRITE_PROMPT,
                                             pos, sz, name);
   
   Bind(wxEVT_FILECTRL_FILTERCHANGED, &ExportFileDialog::OnFilterChanged, this);
   SetUserPaneCreator(CreateUserPaneCallback, (wxUIntPtr) this);
   SetFilterIndex(filterIndex);
}

ExportFileDialog::~ExportFileDialog() = default;

int ExportFileDialog::RunModal(wxWindow* parent,
                               Exporter& exporter,
                               const wxString& defaultName,
                               const wxString& defaultFormatName,
                               const TranslatableString& title,
                               const wxPoint& pos,
                               const wxSize& sz,
                               const wxString& name)
{
   wxString formatName = defaultFormatName;
   if(defaultFormatName.empty())
      formatName = DefaultExportFormat.Read();
   
   wxFileName filename;
   //Bug 1304: Set a default path if none was given.  For Export.
   filename.SetPath(FileNames::FindDefaultPath(FileNames::Operation::Export));
   filename.SetFullName(defaultName);
   
   //`FileDialog` doesn't send button events (wxID_OK, wxID_CANCEL...),
   //in that case we can't perform data validation before closing the dialog.
   //Workaround is to reopen dialog until input is fine.
   while(true)
   {
      ExportFileDialog dialog(parent,
                              exporter,
                              filename.GetPath(),
                              filename.GetFullName(),
                              formatName,
                              title,
                              pos,
                              sz,
                              name);
      
      auto returnCode = dialog.ShowModal();
      if(returnCode != wxID_OK)
         return returnCode;
      
      const auto& plugins = exporter.GetPlugins();
      int pluginIndex { 0 };
      int formatIndex { 0 };
      bool formatFound { false };
      {
         int counter = 0;
         int i = 0;
         for(const auto& plugin : plugins)
         {
            for(int j = 0; j < plugin->GetFormatCount(); ++j)
            {
               if(counter == dialog.GetFilterIndex())
               {
                  pluginIndex = i;
                  formatIndex = j;
                  formatName = plugin->GetFormatInfo(j).mFormat;
                  formatFound = true;
                  break;
               }
               ++counter;
            }
            if(formatFound)
               break;
            ++i;
         }
      }
      
      if(!formatFound)
         continue;
      
      filename = dialog.GetPath();
      const auto ext = filename.GetExt();
      const auto defext = plugins[pluginIndex]->GetFormatInfo(formatIndex).mExtensions[0].Lower();

      //
      // Check the extension - add the default if it's not there,
      // and warn user if it's abnormal.
      //
      if (ext.empty()) {
         //
         // Make sure the user doesn't accidentally save the file
         // as an extension with no name, like just plain ".wav".
         //
         if (filename.GetName().Left(1) == wxT(".")) {
            auto prompt =
               XO("Are you sure you want to export the file as \"%s\"?\n")
                  .Format( filename.GetFullName() );

            int action = AudacityMessageBox(
               prompt,
               XO("Warning"),
               wxYES_NO | wxICON_EXCLAMATION);
            if (action != wxYES) {
               continue;
            }
         }

         filename.SetExt(defext);
      }
      
      if (!plugins[pluginIndex]->CheckFileName(filename, formatIndex))
      {
         continue;
      }
      else if (!ext.empty() && IsExtension(*plugins[pluginIndex], formatIndex, ext) && ext.CmpNoCase(defext)) {
         auto prompt = XO("You are about to export a %s file with the name \"%s\".\n\nNormally these files end in \".%s\", and some programs will not open files with nonstandard extensions.\n\nAre you sure you want to export the file under this name?")
               .Format(plugins[pluginIndex]->GetFormatInfo(formatIndex).mFormat,
                       filename.GetFullName(),
                       defext);

         int action = AudacityMessageBox(
            prompt,
            XO("Warning"),
            wxYES_NO | wxICON_EXCLAMATION);
         if (action != wxYES) {
            continue;
         }
      }

      if (filename.GetFullPath().length() >= 256) {
         AudacityMessageBox(
            XO( "Sorry, pathnames longer than 256 characters not supported.") );
         continue;
      }
      
   // For Mac, it's handled by the FileDialog
   #if !defined(__WXMAC__)
         if (filename.FileExists()) {
            auto prompt = XO("A file named \"%s\" already exists. Replace?")
               .Format( filename.GetFullPath() );

            int action = AudacityMessageBox(
               prompt,
               XO("Warning"),
               wxYES_NO | wxICON_EXCLAMATION);
            if (action != wxYES) {
               continue;
            }
         }
   #endif

      dialog.mOptionsHandlers[dialog.GetFilterIndex()]->TransferDataFromEditor();

      exporter.Configure(filename,
                         pluginIndex,
                         formatIndex,
                         dialog.mOptionsHandlers[dialog.GetFilterIndex()]->GetParameters());
      
      if(defaultFormatName.empty())
         DefaultExportFormat.Write(plugins[pluginIndex]->GetFormatInfo(formatIndex).mFormat);
      
      FileNames::UpdateDefaultPath(FileNames::Operation::Export, filename.GetPath());

      break;
   }
   gPrefs->Flush();
   return wxID_OK;
}

void ExportFileDialog::OnExtensionChanged(wxCommandEvent &evt)
{
   SetFileExtension(evt.GetString().BeforeFirst(' ').Lower());
}

void ExportFileDialog::OnHelp(wxCommandEvent&)
{
   HelpSystem::ShowHelp(wxGetTopLevelParent(this), L"File_Export_Dialog", true);
}

void ExportFileDialog::OnFilterChanged(wxFileCtrlEvent & evt)
{
   int index = evt.GetFilterIndex();

   // On GTK, this event can fire before the userpane is created
   if (mBook == NULL || index < 0 || index >= (int) mBook->GetPageCount())
   {
      return;
   }

   
   {
      FileNames::FileTypes fileTypes;

      int counter = 0;
      for (const auto &plugin : mExporter.GetPlugins())
      {
         for (int j = 0; j < plugin->GetFormatCount(); j++)
         {
            if(counter == index)
            {
               const auto formatInfo = plugin->GetFormatInfo(j);
               if(!formatInfo.mExtensions.empty())
                  SetFileExtension(formatInfo.mExtensions[0]);
            }
            ++counter;
         }
      }
   }
   mBook->ChangeSelection(index);
}

void ExportFileDialog::CreateExportOptions(wxWindow *exportOptionsPane)
{
   ShuttleGui S(exportOptionsPane, eIsCreating);

   S.StartStatic(XO("Format Options"), 1);
   {
      S.StartHorizontalLay(wxEXPAND);
      {
         mBook = S.Position(wxEXPAND).StartSimplebook();
         {
            for (const auto &plugin : mExporter.GetPlugins())
            {
               for (int j = 0; j < plugin->GetFormatCount(); j++)
               {
                  // Name of simple book page is not displayed
                  S.StartNotebookPage( {} );
                  {
                     mOptionsHandlers.push_back(
                        std::make_unique<ExportOptionsHandler>(S, *plugin, j));
                  }
                  S.EndNotebookPage();
               }
            }
         }
         S.EndSimplebook();

         auto b = safenew wxBitmapButton(S.GetParent(), wxID_HELP, theTheme.Bitmap( bmpHelpIcon ));
         b->SetToolTip( XO("Help").Translation() );
         b->SetLabel(XO("Help").Translation());       // for screen readers
         S.Position(wxALIGN_BOTTOM | wxRIGHT | wxBOTTOM).AddWindow(b);
      }
      S.EndHorizontalLay();
   }
   S.EndStatic();
}

void ExportFileDialog::CreateUserPaneCallback(wxWindow *parent, wxUIntPtr userdata)
{
   ExportFileDialog *self = (ExportFileDialog *) userdata;
   if (self)
   {
      self->CreateExportOptions(parent);
   }
}
