/**********************************************************************

Audacity: A Digital Audio Editor

AutoRecoveryDialog.cpp

Paul Licameli split from AutoRecovery.cpp

**********************************************************************/

#include "AutoRecoveryDialog.h"

#include "ActiveProjects.h"
#include "ProjectManager.h"
#include "ProjectFileIO.h"
#include "ProjectFileManager.h"
#include "ShuttleGui.h"
#include "TempDirectory.h"
#include "widgets/AudacityMessageBox.h"
#include "wxPanelWrapper.h"

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
   explicit AutoRecoveryDialog(AudacityProject *proj);

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
   void OnListKeyDown(wxKeyEvent &evt);

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
      S.AddFixedText(
         XO("The following projects were not saved properly the last time Audacity was run and "
            "can be automatically recovered.\n\n"
            "After recovery, save the projects to ensure changes are written to disk."),
         false,
         500);

      S.StartStatic(XO("Recoverable &projects"), 1);
      {
         mFileList = S.Id(ID_FILE_LIST)
            .ConnectRoot(wxEVT_KEY_DOWN, &AutoRecoveryDialog::OnListKeyDown)
            .AddListControlReportMode(
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

      S.StartHorizontalLay(wxALIGN_CENTRE, 0);
      {
         S.Id(ID_QUIT_AUDACITY).AddButton(XXO("&Quit Audacity"));
         S.Id(ID_DISCARD_SELECTED).AddButton(XXO("&Discard Selected"));
         S.Id(ID_RECOVER_SELECTED).AddButton(XXO("&Recover Selected"), wxALIGN_CENTRE, true);
         S.Id(ID_SKIP).AddButton(XXO("&Skip"));

         SetAffirmativeId(ID_RECOVER_SELECTED);
         SetEscapeId(ID_SKIP);
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
   wxString tempdir = TempDirectory::TempDir();
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
      else
      {
         ActiveProjects::Remove(file);
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
         mFileList->CheckItem(item, true);
         item++;
      }
   }
   mFileList->SetMinSize(mFileList->GetBestSize());
   mFileList->SetColumnWidth(0, wxLIST_AUTOSIZE_USEHEADER);
   mFileList->SetColumnWidth(1, 500);

   if (item)
   {
      mFileList->SetItemState(0,
                              wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED,
                              wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);
      mFileList->SetFocus();
   }
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

   long item = -1;
   bool selectedTemporary = false;
   while (!selectedTemporary)
   {
      item = mFileList->GetNextItem(item, wxLIST_NEXT_ALL, wxLIST_STATE_DONTCARE);
      if (item == wxNOT_FOUND)
         break;
      if (!mFileList->IsItemChecked(item))
         continue;
      FilePath fileName = mFiles[item];
      wxFileName file(fileName);
      if (file.GetExt().IsSameAs(FileNames::UnsavedProjectExtension()))
         selectedTemporary = true;
   }

   // Don't give this warning message if all of the checked items are
   // previously saved, non-temporary projects.
   if (selectedTemporary) {
      int ret = AudacityMessageBox(
         XO("Are you sure you want to discard the selected projects?\n\n"
            "Choosing \"Yes\" permanently deletes the selected projects immediately."),
         XO("Automatic Crash Recovery"),
         wxICON_QUESTION | wxYES_NO | wxNO_DEFAULT, this);

      if (ret == wxNO)
         return;
   }

   item = -1;
   FilePaths files;
   while (true)
   {
      item = mFileList->GetNextItem(item, wxLIST_NEXT_ALL, wxLIST_STATE_DONTCARE);
      if (item == wxNOT_FOUND)
      {
         break;
      }
      if (!mFileList->IsItemChecked(item))
      {
         // Keep in list
         files.push_back(mFiles[item]);
         continue;
      }
      FilePath fileName = mFiles[item];

      // Only remove it from disk if it appears to be a temporary file.
      wxFileName file(fileName);
      if (file.GetExt().IsSameAs(FileNames::UnsavedProjectExtension()))
      {
         file.SetFullName(wxT(""));

         wxFileName temp(TempDirectory::TempDir(), wxT(""));
         if (file == temp)
            ProjectFileIO::RemoveProject(fileName);
      }
      else
         // Don't remove from disk, but do (later) open the database
         // of this saved file, and discard edits
         files.push_back(fileName);

      // Forget all about it
      ActiveProjects::Remove(fileName);
   }

   PopulateList();

   mFiles = files;

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

void AutoRecoveryDialog::OnListKeyDown(wxKeyEvent &evt)
{
   switch (evt.GetKeyCode())
   {
      case WXK_SPACE:
      {
         bool selected = false;
         long item = -1;
         while (true)
         {
            item = mFileList->GetNextItem(item, wxLIST_NEXT_ALL, wxLIST_STATE_SELECTED);
            if (item == wxNOT_FOUND)
            {
               break;
            }

            mFileList->CheckItem(item, !mFileList->IsItemChecked(item));
         }
      }
      break;

      case WXK_RETURN:
         // Don't know why wxListCtrls prevent default dialog action,
         // but they do, so handle it.
         EmulateButtonClickIfPresent(GetAffirmativeId());
      break;

      default:
         evt.Skip();
      break;
   }
}

////////////////////////////////////////////////////////////////////////////

static bool RecoverAllProjects(const FilePaths &files,
                               AudacityProject *&pproj)
{
   // Open a project window for each auto save file
   wxString filename;
   bool result = true;

   for (auto &file: files)
   {
      AudacityProject *proj = nullptr;
      // Reuse any existing project window, which will be the empty project
      // created at application startup
      std::swap(proj, pproj);

      // Open project.
      if (ProjectManager::OpenProject(proj, file, false, true) == nullptr)
      {
         result = false;
      }
   }

   return result;
}

static void DiscardAllProjects(const FilePaths &files)
{
   // Open and close each file, invisibly, removing its Autosave blob
   for (auto &file: files)
      ProjectFileManager::DiscardAutosave(file);
}

bool ShowAutoRecoveryDialogIfNeeded(AudacityProject *&pproj, bool *didRecoverAnything)
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

   AutoRecoveryDialog dialog(pproj);

   if (dialog.HasRecoverables())
   {
      int ret = dialog.ShowModal();
      
      switch (ret)
      {
      case ID_SKIP:
         success = true;
         break;

      case ID_DISCARD_SELECTED:
         DiscardAllProjects(dialog.GetRecoverables());
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
