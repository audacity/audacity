/**********************************************************************

Audacity: A Digital Audio Editor

AutoRecoveryDialog.cpp

Paul Licameli split from AutoRecovery.cpp

**********************************************************************/

#include "AutoRecoveryDialog.h"

#include "ActiveProjects.h"
#include "FileNames.h"
#include "ProjectManager.h"
#include "ShuttleGui.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/wxPanelWrapper.h"

#include <wx/dir.h>
#include <wx/evtloop.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/listctrl.h>

#define USE_CHECKBOXES

enum {
   ID_QUIT_AUDACITY = 10000,
   ID_DISCARD_SELECTED,
   ID_RECOVER_SELECTED,
   ID_FILE_LIST
};

class AutoRecoveryDialog final : public wxDialogWrapper
{
public:
   AutoRecoveryDialog();

   bool HasRecoverables() const;
   FilePaths GetRecoverables();

private:
   void PopulateOrExchange(ShuttleGui &S);
   void PopulateList();

   void OnQuitAudacity(wxCommandEvent &evt);
   void OnDiscardSelected(wxCommandEvent &evt);
   void OnRecoverSelected(wxCommandEvent &evt);

   FilePaths mFiles;
   wxListCtrl *mFileList;

public:
   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(AutoRecoveryDialog, wxDialogWrapper)
   EVT_BUTTON(ID_QUIT_AUDACITY, AutoRecoveryDialog::OnQuitAudacity)
   EVT_BUTTON(ID_DISCARD_SELECTED, AutoRecoveryDialog::OnDiscardSelected)
   EVT_BUTTON(ID_RECOVER_SELECTED, AutoRecoveryDialog::OnRecoverSelected)
END_EVENT_TABLE()

AutoRecoveryDialog::AutoRecoveryDialog()
:  wxDialogWrapper(nullptr, wxID_ANY, XO("Automatic Crash Recovery"),
                   wxDefaultPosition, wxDefaultSize,
                   wxDEFAULT_DIALOG_STYLE & (~wxCLOSE_BOX)) // no close box
{
   SetName();
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

bool AutoRecoveryDialog::HasRecoverables() const
{
   return mFiles.size() > 0;
}

FilePaths AutoRecoveryDialog::GetRecoverables()
{
   return mFiles;
}

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
         mFileList = S.Id(ID_FILE_LIST).AddListControlReportMode(
            {
#if defined(USE_CHECKBOXES)
               /*i18n-hint: (verb).  It instruct the user to select items.*/
               XO("Select"),
#endif
               /*i18n-hint: (noun).  It's the name of the project to recover.*/
               XO("Name")
            });
#if defined(USE_CHECKBOXES)
         mFileList->EnableCheckBoxes();
#endif
         PopulateList();
      }
      S.EndStatic();

      S.AddVariableText(
         XO("After recovery, save the project to save the changes to disk."),
         false);

      S.StartHorizontalLay();
      {
         S.Id(ID_QUIT_AUDACITY).AddButton(XXO("Quit Audacity"));
         S.Id(ID_DISCARD_SELECTED).AddButton(XXO("Discard Selected"));
         S.Id(ID_RECOVER_SELECTED).AddButton(XXO("Recover Selected"));
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
   wxString tempdir = FileNames::TempDir();
   wxString pattern = wxT("*.") + FileNames::UnsavedProjectExtension();
   FilePaths files;

   wxDir::GetAllFiles(tempdir, &files, pattern, wxDIR_FILES);

   FilePaths active = ActiveProjects::GetAll();

   mFileList->DeleteAllItems();

   long item = 0;
   for (auto file : active)
   {
      wxFileName fn = file;
      if (fn.FileExists())
      {
         FilePath fullPath = fn.GetFullPath();
         if (files.Index(fullPath) == wxNOT_FOUND)
         {
            files.push_back(fullPath);
         }
      }
   }

   for (auto file : files)
   {
      wxFileName fn = file;

      mFiles.push_back(fn.GetFullPath());
      mFileList->InsertItem(item, wxT(""));
      mFileList->SetItem(item, 1, fn.GetName());
      item++;
   }

#if defined(USE_CHECKBOXES)
   mFileList->SetColumnWidth(0, wxLIST_AUTOSIZE_USEHEADER);
   mFileList->SetColumnWidth(1, wxLIST_AUTOSIZE);
#else
   mFileList->SetColumnWidth(0, wxLIST_AUTOSIZE);
#endif
}

void AutoRecoveryDialog::OnQuitAudacity(wxCommandEvent & WXUNUSED(event))
{
   EndModal(ID_QUIT_AUDACITY);
}

void AutoRecoveryDialog::OnDiscardSelected(wxCommandEvent & WXUNUSED(event))
{
   int ret = AudacityMessageBox(
      XO("Are you sure you want to discard the selected projects?\n\n"
         "Choosing \"Yes\" permanently deletes the selected projects immediately."),
      XO("Confirm Discard Projects"),
      wxICON_QUESTION | wxYES_NO | wxNO_DEFAULT, this);

   if (ret == wxNO)
   {
      return;
   }

#define USE_CHECKBOXES
#if defined(USE_CHECKBOXES)
#define state wxLIST_STATE_DONTCARE
#else
#define state wxLIST_STATE_SELECTED
#endif

   long item = -1;
   while (true)
   {
      item = mFileList->GetNextItem(item, wxLIST_NEXT_ALL, state);
      if (item == wxNOT_FOUND)
      {
         break;
      }
#if defined(USE_CHECKBOXES)
      if (!mFileList->IsItemChecked(item))
      {
         continue;
      }
#endif
      FilePath file = mFiles[item];

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

         ActiveProjects::Remove(file);
      }
   }

   PopulateList();

   if (mFileList->GetItemCount() == 0)
   {
      EndModal(ID_DISCARD_SELECTED);
   }
}

void AutoRecoveryDialog::OnRecoverSelected(wxCommandEvent & WXUNUSED(event))
{
#define USE_CHECKBOXES
#if defined(USE_CHECKBOXES)
#define state wxLIST_STATE_DONTCARE
#else
#define state wxLIST_STATE_SELECTED
#endif

   FilePaths files;

   long item = -1;
   while (true)
   {
      item = mFileList->GetNextItem(item, wxLIST_NEXT_ALL, state);
      if (item == wxNOT_FOUND)
      {
         break;
      }
#if defined(USE_CHECKBOXES)
      if (!mFileList->IsItemChecked(item))
      {
         continue;
      }
#endif

      files.push_back(mFiles[item]);
   }

   mFiles = files;

   EndModal(ID_RECOVER_SELECTED);
}

////////////////////////////////////////////////////////////////////////////

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

      // Open project.
      if (ProjectManager::OpenProject(proj, files[i], false) == nullptr)
      {
         return false;
      }

      ActiveProjects::Remove(files[i]);
   }

   return true;
}

bool ShowAutoRecoveryDialogIfNeeded(AudacityProject **pproj, bool *didRecoverAnything)
{
   if (didRecoverAnything)
   {
      *didRecoverAnything = false;
   }

   bool success = true;

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

   AutoRecoveryDialog dialog;

   if (dialog.HasRecoverables())
   {
      int ret = dialog.ShowModal();
      
      switch (ret)
      {
      case ID_DISCARD_SELECTED:
         success = true;
         break;

      case ID_RECOVER_SELECTED:
         success = RecoverAllProjects(dialog.GetRecoverables(), pproj);
         if (success)
         {
            if (didRecoverAnything)
            {
               *didRecoverAnything = true;
            }
         }
         break;

      default:
         // This includes ID_QUIT_AUDACITY
         return false;
      }
   }
   
   return success;
}
