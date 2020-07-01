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
   AutoRecoveryDialog(const FilePaths &files);

private:
   void PopulateOrExchange(ShuttleGui &S);
   void PopulateList();

   void OnQuitAudacity(wxCommandEvent &evt);
   void OnRecoverNone(wxCommandEvent &evt);
   void OnRecoverAll(wxCommandEvent &evt);

   const FilePaths & mFiles;
   wxListCtrl *mFileList;

public:
   DECLARE_EVENT_TABLE()
};

AutoRecoveryDialog::AutoRecoveryDialog(const FilePaths &files)
:  wxDialogWrapper(nullptr, -1, XO("Automatic Crash Recovery"),
                   wxDefaultPosition, wxDefaultSize,
                   wxDEFAULT_DIALOG_STYLE & (~wxCLOSE_BOX)), // no close box
   mFiles(files)
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

void AutoRecoveryDialog::PopulateOrExchange(ShuttleGui &S)
{
   S.SetBorder(5);
   S.StartVerticalLay();
   {
      S.AddVariableText(
         XO("Some projects were not saved properly the last time Audacity was run.\nFortunately, the following projects can be automatically recovered:"),
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

   for (int i = 0, cnt = mFiles.size(); i < cnt; ++i)
   {
        mFileList->InsertItem(i, wxFileName{ mFiles[i] }.GetName());
   }

   mFileList->SetColumnWidth(0, wxLIST_AUTOSIZE);
}

void AutoRecoveryDialog::OnQuitAudacity(wxCommandEvent & WXUNUSED(event))
{
   EndModal(ID_QUIT_AUDACITY);
}

void AutoRecoveryDialog::OnRecoverNone(wxCommandEvent & WXUNUSED(event))
{
   int ret = AudacityMessageBox(
      XO("Are you sure you want to discard all recoverable projects?\n\nChoosing \"Yes\" discards all recoverable projects immediately."),
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

static FilePaths HaveFilesToRecover()
{
   wxString tempdir = FileNames::TempDir();
   wxString pattern = wxT("*.") + FileNames::UnsavedProjectExtension();
   FilePaths files;

   wxDir::GetAllFiles(tempdir, &files, pattern, wxDIR_FILES);

   return files;
}

static bool RemoveAllAutoSaveFiles(const FilePaths &files)
{
   for (int i = 0, cnt = files.size(); i < cnt; ++i)
   {
      FilePath file = files[i];

      if (wxRemoveFile(file))
      {
         if (wxFileExists(file + wxT("-shm")))
         {
            wxRemoveFile(file + wxT("-shm"));
         }

         if (wxFileExists(file + wxT("-wal")))
         {
            wxRemoveFile(file + wxT("-wal"));
         }

         if (wxFileExists(file + wxT("-journal")))
         {
            wxRemoveFile(file + wxT("-journal"));
         }
      }
   }

   return true;
}

static bool RecoverAllProjects(const FilePaths &files,
                               AudacityProject **pproj)
{
   // Open a project window for each auto save file
   wxString filename;

   for (int i = 0, cnt = files.size(); i < cnt; ++i)
   {
      AudacityProject *proj = nullptr;
      if (*pproj)
      {
         // Reuse existing project window
         proj = *pproj;
         *pproj = NULL;
      }

      // Open project. When an auto-save file has been opened successfully,
      // the opened auto-save file is automatically deleted and a NEW one
      // is created.
      (void) ProjectManager::OpenProject(proj, files[i], false);
   }

   return true;
}

bool ShowAutoRecoveryDialogIfNeeded(AudacityProject **pproj,
                                    bool *didRecoverAnything)
{
   if (didRecoverAnything)
      *didRecoverAnything = false;

   FilePaths files = HaveFilesToRecover();
   if (files.size())
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

      int ret = AutoRecoveryDialog(files).ShowModal();

      switch (ret)
      {
      case ID_RECOVER_NONE:
         return RemoveAllAutoSaveFiles(files);

      case ID_RECOVER_ALL:
         if (didRecoverAnything)
            *didRecoverAnything = true;
         return RecoverAllProjects(files, pproj);

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
