/**********************************************************************

Audacity: A Digital Audio Editor

AutoRecoveryDialog.cpp

Paul Licameli split from AutoRecovery.cpp

**********************************************************************/

#include "AutoRecoveryDialog.h"

#include "ActiveProjects.h"
#include "FileNames.h"
#include "ProjectManager.h"
#include "ProjectFileIO.h"
#include "ShuttleGui.h"
#include "widgets/AudacityMessageBox.h"
#include "widgets/wxPanelWrapper.h"

#include <wx/dir.h>
#include <wx/evtloop.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/listctrl.h>

enum {
   ID_QUIT_AUDACITY = 10000,
   ID_DISCARD_SELECTED,
   ID_RECOVER_SELECTED,
   ID_SKIP,
   ID_FILE_LIST
};

class AutoRecoveryDialog final : public wxDialogWrapper
{
public:
   AutoRecoveryDialog(AudacityProject *proj);

   bool HasRecoverables() const;
   FilePaths GetRecoverables();

private:
   void PopulateOrExchange(ShuttleGui &S);
   void PopulateList();
   bool HaveChecked();

   void OnQuitAudacity(wxCommandEvent &evt);
   void OnDiscardSelected(wxCommandEvent &evt);
   void OnRecoverSelected(wxCommandEvent &evt);
   void OnSkip(wxCommandEvent &evt);
   void OnColumnClicked(wxListEvent &evt);
   void OnItemActivated(wxListEvent &evt);

   FilePaths mFiles;
   wxListCtrl *mFileList;
   AudacityProject *mProject;

public:
   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(AutoRecoveryDialog, wxDialogWrapper)
   EVT_BUTTON(ID_QUIT_AUDACITY, AutoRecoveryDialog::OnQuitAudacity)
   EVT_BUTTON(ID_DISCARD_SELECTED, AutoRecoveryDialog::OnDiscardSelected)
   EVT_BUTTON(ID_RECOVER_SELECTED, AutoRecoveryDialog::OnRecoverSelected)
   EVT_BUTTON(ID_SKIP, AutoRecoveryDialog::OnSkip)
   EVT_LIST_COL_CLICK(ID_FILE_LIST, AutoRecoveryDialog::OnColumnClicked)
   EVT_LIST_ITEM_ACTIVATED(ID_FILE_LIST, AutoRecoveryDialog::OnItemActivated)
END_EVENT_TABLE()

AutoRecoveryDialog::AutoRecoveryDialog(AudacityProject *project)
:  wxDialogWrapper(nullptr, wxID_ANY, XO("Automatic Crash Recovery"),
                   wxDefaultPosition, wxDefaultSize,
                   (wxDEFAULT_DIALOG_STYLE & (~wxCLOSE_BOX)) | wxRESIZE_BORDER), // no close box
   mProject(project)
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
   S.StartVerticalLay(wxEXPAND, 1);
   {
      S.AddVariableText(
         XO("Some projects were not saved properly the last time Audacity was run.\nFortunately, the following projects can be automatically recovered:"),
         false);

      S.StartStatic(XO("Recoverable projects"), 1);
      {
         mFileList = S.Id(ID_FILE_LIST).AddListControlReportMode(
            {
               /*i18n-hint: (verb).  It instruct the user to select items.*/
               XO("Select"),
               /*i18n-hint: (noun).  It's the name of the project to recover.*/
               XO("Name")
            });
         mFileList->EnableCheckBoxes();
         PopulateList();
      }
      S.EndStatic();

      S.AddVariableText(
         XO("After recovery, save the project to save the changes to disk."),
         false);

      S.StartHorizontalLay(wxALIGN_CENTRE, 0);
      {
         S.Id(ID_QUIT_AUDACITY).AddButton(XXO("Quit Audacity"));
         S.Id(ID_DISCARD_SELECTED).AddButton(XXO("Discard Selected"));
         S.Id(ID_RECOVER_SELECTED).AddButton(XXO("Recover Selected"));
         S.Id(ID_SKIP).AddButton(XXO("Skip"));
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

   FilePath activeFile;
   if (mProject)
   {
      auto &projectFileIO = ProjectFileIO::Get(*mProject);
      activeFile = projectFileIO.GetFileName();
   }

   mFiles.clear();
   mFileList->DeleteAllItems();
   long item = 0;

   for (auto file : files)
   {
      wxFileName fn = file;
      if (fn != activeFile)
      {
         mFiles.push_back(fn.GetFullPath());
         mFileList->InsertItem(item, wxT(""));
         mFileList->SetItem(item, 1, fn.GetName());
         item++;
      }
   }
   mFileList->SetMinSize(mFileList->GetBestSize());
   mFileList->SetColumnWidth(0, wxLIST_AUTOSIZE_USEHEADER);
   mFileList->SetColumnWidth(1, 500);
}

bool AutoRecoveryDialog::HaveChecked()
{
   long item = -1;
   while (true)
   {
      item = mFileList->GetNextItem(item, wxLIST_NEXT_ALL, wxLIST_STATE_DONTCARE);
      if (item == wxNOT_FOUND)
      {
         break;
      }
      if (mFileList->IsItemChecked(item))
      {
         return true;
      }
   }

   AudacityMessageBox(XO("No projects selected"), XO("Automatic Crash Recovery"));

   return false;
}

void AutoRecoveryDialog::OnQuitAudacity(wxCommandEvent &WXUNUSED(evt))
{
   EndModal(ID_QUIT_AUDACITY);
}

void AutoRecoveryDialog::OnDiscardSelected(wxCommandEvent &WXUNUSED(evt))
{
   if (!HaveChecked())
   {
      return;
   }

   int ret = AudacityMessageBox(
      XO("Are you sure you want to discard the selected projects?\n\n"
         "Choosing \"Yes\" permanently deletes the selected projects immediately."),
      XO("Automatic Crash Recovery"),
      wxICON_QUESTION | wxYES_NO | wxNO_DEFAULT, this);

   if (ret == wxNO)
   {
      return;
   }

   long item = -1;
   while (true)
   {
      item = mFileList->GetNextItem(item, wxLIST_NEXT_ALL, wxLIST_STATE_DONTCARE);
      if (item == wxNOT_FOUND)
      {
         break;
      }
      if (!mFileList->IsItemChecked(item))
      {
         continue;
      }
      FilePath fileName = mFiles[item];

      // Only remove it from disk if it appears to be a temporary file.
      wxFileName file(fileName);
      if (file.GetExt().IsSameAs(FileNames::UnsavedProjectExtension()))
      {
         file.SetFullName(wxT(""));

         wxFileName temp(FileNames::TempDir(), wxT(""));
         if (file == temp)
         {
            if (wxRemoveFile(fileName))
            {
               if (wxFileExists(fileName + wxT("-shm")))
               {
                  wxRemoveFile(fileName + wxT("-shm"));
               }

               if (wxFileExists(fileName + wxT("-wal")))
               {
                  wxRemoveFile(fileName + wxT("-wal"));
               }

               if (wxFileExists(fileName + wxT("-journal")))
               {
                  wxRemoveFile(fileName + wxT("-journal"));
               }
            }
         }
      }

      // Forget all about it
      ActiveProjects::Remove(fileName);
   }

   PopulateList();

   if (mFileList->GetItemCount() == 0)
   {
      EndModal(ID_DISCARD_SELECTED);
   }
}

void AutoRecoveryDialog::OnRecoverSelected(wxCommandEvent &WXUNUSED(evt))
{
   if (!HaveChecked())
   {
      return;
   }

   FilePaths files;

   bool selected = false;
   long item = -1;
   while (true)
   {
      item = mFileList->GetNextItem(item, wxLIST_NEXT_ALL, wxLIST_STATE_DONTCARE);
      if (item == wxNOT_FOUND)
      {
         if (!selected)
         {
            AudacityMessageBox(XO("No projects selected"), XO("Automatic Crash Recovery"));
         }
         break;
      }
      selected = true;

      if (!mFileList->IsItemChecked(item))
      {
         continue;
      }

      files.push_back(mFiles[item]);
   }

   mFiles = files;

   EndModal(ID_RECOVER_SELECTED);
}

void AutoRecoveryDialog::OnSkip(wxCommandEvent &WXUNUSED(evt))
{
   EndModal(ID_SKIP);
}

void AutoRecoveryDialog::OnColumnClicked(wxListEvent &evt)
{
   if (evt.GetColumn() != 0)
   {
      return;
   }

   long item = -1;
   while (true)
   {
      item = mFileList->GetNextItem(item, wxLIST_NEXT_ALL, wxLIST_STATE_DONTCARE);
      if (item == wxNOT_FOUND)
      {
         break;
      }
      mFileList->CheckItem(item, !mFileList->IsItemChecked(item));
   }
}

void AutoRecoveryDialog::OnItemActivated(wxListEvent &evt)
{
   long item = evt.GetIndex();
   mFileList->CheckItem(item, !mFileList->IsItemChecked(item));
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

   AutoRecoveryDialog dialog(*pproj);

   if (dialog.HasRecoverables())
   {
      int ret = dialog.ShowModal();
      
      switch (ret)
      {
      case ID_SKIP:
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
