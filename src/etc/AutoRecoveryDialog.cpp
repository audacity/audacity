/**********************************************************************

Audacity: A Digital Audio Editor

AutoRecoveryDialog.cpp

Paul Licameli split from AutoRecovery.cpp

**********************************************************************/

#include "AutoRecoveryDialog.h"

#include "FileNames.h"
#include "ProjectManager.h"
#include "ShuttleGui.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/wxPanelWrapper.h"

#include <wx/evtloop.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/listctrl.h>

enum {
   ID_RECOVER_ALL = 10000,
   ID_RECOVER_NONE,
   ID_QUIT_AUDACITY,
   ID_FILE_LIST
};

class AutoRecoveryDialog final : public wxDialogWrapper
{
public:
   AutoRecoveryDialog(wxWindow *parent);

private:
   void PopulateList();
   void PopulateOrExchange(ShuttleGui & S);

   void OnQuitAudacity(wxCommandEvent &evt);
   void OnRecoverNone(wxCommandEvent &evt);
   void OnRecoverAll(wxCommandEvent &evt);

   wxListCtrl *mFileList;

public:
   DECLARE_EVENT_TABLE()
};

AutoRecoveryDialog::AutoRecoveryDialog(wxWindow *parent) :
   wxDialogWrapper(parent, -1, XO("Automatic Crash Recovery"),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE & (~wxCLOSE_BOX)) // no close box
{
   SetName();
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

BEGIN_EVENT_TABLE(AutoRecoveryDialog, wxDialogWrapper)
   EVT_BUTTON(ID_RECOVER_ALL, AutoRecoveryDialog::OnRecoverAll)
   EVT_BUTTON(ID_RECOVER_NONE, AutoRecoveryDialog::OnRecoverNone)
   EVT_BUTTON(ID_QUIT_AUDACITY, AutoRecoveryDialog::OnQuitAudacity)
END_EVENT_TABLE()

void AutoRecoveryDialog::PopulateOrExchange(ShuttleGui& S)
{
   S.SetBorder(5);
   S.StartVerticalLay();
   {
      S.AddVariableText(
         XO(
"Some projects were not saved properly the last time Audacity was run.\nFortunately, the following projects can be automatically recovered:"),
         false);

      S.StartStatic(XO("Recoverable projects"));
      {
         mFileList = S.Id(ID_FILE_LIST)
            /*i18n-hint: (noun).  It's the name of the project to recover.*/
            .AddListControlReportMode( { XO("Name") } );
         PopulateList();
      }
      S.EndStatic();

      S.AddVariableText(
         XO("After recovery, save the project to save the changes to disk."),
         false);

      S.StartHorizontalLay();
      {
         S.Id(ID_QUIT_AUDACITY).AddButton(XXO("Quit Audacity"));
         S.Id(ID_RECOVER_NONE).AddButton(XXO("Discard Projects"));
         S.Id(ID_RECOVER_ALL).AddButton(XXO("Recover Projects"));
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   Layout();
   Fit();
   SetMinSize(GetSize());

   // Sometimes it centers on wxGTK and sometimes it doesn't.
   // Yielding before centering seems to be a good workaround,
   // but will leave to implement on a rainy day.
   Center();
}

void AutoRecoveryDialog::PopulateList()
{
   mFileList->DeleteAllItems();

   wxDir dir(FileNames::AutoSaveDir());
   if (!dir.IsOpened())
      return;

   wxString filename;
   int i = 0;
   for (bool c = dir.GetFirst(&filename, wxT("*.autosave"), wxDIR_FILES);
        c; c = dir.GetNext(&filename))
        mFileList->InsertItem(i++, wxFileName{ filename }.GetName());

   mFileList->SetColumnWidth(0, wxLIST_AUTOSIZE);
}

void AutoRecoveryDialog::OnQuitAudacity(wxCommandEvent & WXUNUSED(event))
{
   EndModal(ID_QUIT_AUDACITY);
}

void AutoRecoveryDialog::OnRecoverNone(wxCommandEvent & WXUNUSED(event))
{
   int ret = AudacityMessageBox(
      XO(
"Are you sure you want to discard all recoverable projects?\n\nChoosing \"Yes\" discards all recoverable projects immediately."),
      XO("Confirm Discard Projects"),
      wxICON_QUESTION | wxYES_NO | wxNO_DEFAULT, this);

   if (ret == wxYES)
      EndModal(ID_RECOVER_NONE);
}

void AutoRecoveryDialog::OnRecoverAll(wxCommandEvent & WXUNUSED(event))
{
   EndModal(ID_RECOVER_ALL);
}

////////////////////////////////////////////////////////////////////////////

static bool HaveFilesToRecover()
{
   wxDir dir(FileNames::AutoSaveDir());
   if (!dir.IsOpened())
   {
      AudacityMessageBox(
         XO("Could not enumerate files in auto save directory."),
         XO("Error"),
         wxICON_STOP);
      return false;
   }

   wxString filename;
   bool c = dir.GetFirst(&filename, wxT("*.autosave"), wxDIR_FILES);

   return c;
}

static bool RemoveAllAutoSaveFiles()
{
   FilePaths files;
   wxDir::GetAllFiles(FileNames::AutoSaveDir(), &files,
                      wxT("*.autosave"), wxDIR_FILES);

   for (unsigned int i = 0; i < files.size(); i++)
   {
      if (!wxRemoveFile(files[i]))
      {
         // I don't think this error message is actually useful.
         // -dmazzoni
         //AudacityMessageBox(
         //   XO("Could not remove auto save file: %s".Format( files[i] ),
         //   XO("Error"),
         //   wxICON_STOP);
         return false;
      }
   }

   return true;
}

static bool RecoverAllProjects(AudacityProject** pproj)
{
   wxDir dir(FileNames::AutoSaveDir());
   if (!dir.IsOpened())
   {
      AudacityMessageBox(
         XO("Could not enumerate files in auto save directory."),
         XO("Error"),
         wxICON_STOP);
      return false;
   }

   // Open a project window for each auto save file
   wxString filename;

   FilePaths files;
   wxDir::GetAllFiles(FileNames::AutoSaveDir(), &files,
                      wxT("*.autosave"), wxDIR_FILES);

   for (unsigned int i = 0; i < files.size(); i++)
   {
      AudacityProject* proj{};
      if (*pproj)
      {
         // Reuse existing project window
         proj = *pproj;
         *pproj = NULL;
      }

      // Open project. When an auto-save file has been opened successfully,
      // the opened auto-save file is automatically deleted and a NEW one
      // is created.
      (void) ProjectManager::OpenProject( proj, files[i], false );
   }

   return true;
}

bool ShowAutoRecoveryDialogIfNeeded(AudacityProject** pproj,
                                    bool *didRecoverAnything)
{
   if (didRecoverAnything)
      *didRecoverAnything = false;
   if (HaveFilesToRecover())
   {
      // Under wxGTK3, the auto recovery dialog will not get
      // the focus since the project window hasn't been allowed
      // to completely initialize.
      //
      // Yielding seems to allow the initialization to complete.
      //
      // Additionally, it also corrects a sizing issue in the dialog
      // related to wxWidgets bug:
      //
      //    http://trac.wxwidgets.org/ticket/16440
      //
      // This must be done before "dlg" is declared.
      wxEventLoopBase::GetActive()->YieldFor(wxEVT_CATEGORY_UI);

      int ret = AutoRecoveryDialog{nullptr}.ShowModal();

      switch (ret)
      {
      case ID_RECOVER_NONE:
         return RemoveAllAutoSaveFiles();

      case ID_RECOVER_ALL:
         if (didRecoverAnything)
            *didRecoverAnything = true;
         return RecoverAllProjects(pproj);

      default:
         // This includes ID_QUIT_AUDACITY
         return false;
      }
   } else
   {
      // Nothing to recover, move along
      return true;
   }
}
