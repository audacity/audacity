/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2008 Audacity Team.
   License: GPL v2.  See License.txt.

   Dependencies.cpp

   Dominic Mazzoni
   Leland Lucius
   Markus Meyer
   LRN
   Michael Chinen
   Vaughan Johnson

   The primary function provided in this source file is
   ShowDependencyDialogIfNeeded.  It checks a project to see if
   any of its WaveTracks contain AliasBlockFiles; if so it
   presents a dialog to the user and lets them copy those block
   files into the project, making it self-contained.

**********************************************************************/

#include "Audacity.h"
#include "Dependencies.h"

#include <wx/button.h>
#include <wx/defs.h>
#include <wx/dialog.h>
#include <wx/filename.h>
#include <wx/hashmap.h>
#include <wx/progdlg.h>
#include <wx/choice.h>

#include "BlockFile.h"
#include "DirManager.h"
#include "Internat.h"
#include "Prefs.h"
#include "Project.h"
#include "Sequence.h"
#include "ShuttleGui.h"
#include "WaveTrack.h"
#include "WaveClip.h"

WX_DECLARE_HASH_MAP(wxString, AliasedFile *,
                    wxStringHash, wxStringEqual, AliasedFileHash);

// These two hash types are used only inside short scopes
// so it is safe to key them by plain pointers.
WX_DECLARE_HASH_MAP(BlockFile *, BlockFilePtr,
                    wxPointerHash, wxPointerEqual, ReplacedBlockFileHash);
WX_DECLARE_HASH_MAP(BlockFile *, bool,
                    wxPointerHash, wxPointerEqual, BoolBlockFileHash);

// Given a project, returns a single array of all SeqBlocks
// in the current set of tracks.  Enumerating that array allows
// you to process all block files in the current set.
static void GetAllSeqBlocks(AudacityProject *project,
                            BlockPtrArray *outBlocks)
{
   TrackList *tracks = project->GetTracks();
   TrackListIterator iter(tracks);
   Track *t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Wave) {
         WaveTrack *waveTrack = static_cast<WaveTrack*>(t);
         for(const auto &clip : waveTrack->GetAllClips()) {
            Sequence *sequence = clip->GetSequence();
            BlockArray &blocks = sequence->GetBlockArray();
            int i;
            for (i = 0; i < (int)blocks.size(); i++)
               outBlocks->push_back(&blocks[i]);
         }
      }
      t = iter.Next();
   }
}

// Given an Audacity project and a hash mapping aliased block
// files to un-aliased block files, walk through all of the
// tracks and replace each aliased block file with its replacement.
// Note that this code respects reference-counting and thus the
// process of making a project self-contained is actually undoable.
static void ReplaceBlockFiles(AudacityProject *project,
                              ReplacedBlockFileHash &hash)
{
   //const auto &dirManager = project->GetDirManager();
   BlockPtrArray blocks;
   GetAllSeqBlocks(project, &blocks);

   int i;
   for (i = 0; i < (int)blocks.size(); i++) {
      auto &f = blocks[i]->f;
      const auto src = &*f;
      if (hash.count( src ) > 0) {
         const auto &dst = hash[src];
         f = dst;
      }
   }
}

void FindDependencies(AudacityProject *project,
                      AliasedFileArray &outAliasedFiles)
{
   sampleFormat format = project->GetDefaultFormat();

   BlockPtrArray blocks;
   GetAllSeqBlocks(project, &blocks);

   AliasedFileHash aliasedFileHash;
   BoolBlockFileHash blockFileHash;

   for (const auto &blockFile : blocks) {
      const auto &f = blockFile->f;
      if (f->IsAlias() && (blockFileHash.count( &*f ) == 0))
      {
         // f is an alias block we have not yet counted.
         blockFileHash[ &*f ] = true; // Don't count the same blockfile twice.
         auto aliasBlockFile = static_cast<AliasBlockFile*>( &*f );
         const wxFileName &fileName = aliasBlockFile->GetAliasedFileName();

         // In DirManager::ProjectFSCK(), if the user has chosen to
         // "Replace missing audio with silence", the code there puts in an empty wxFileName.
         // Don't count those in dependencies.
         if (!fileName.IsOk())
            continue;

         const wxString &fileNameStr = fileName.GetFullPath();
         auto blockBytes = (SAMPLE_SIZE(format) *
                           aliasBlockFile->GetLength());
         if (aliasedFileHash.count(fileNameStr) > 0)
            // Already put this AliasBlockFile in aliasedFileHash.
            // Update block count.
            aliasedFileHash[fileNameStr]->mByteCount += blockBytes;
         else
         {
            // Haven't counted this AliasBlockFile yet.
            // Add to return array and internal hash.

            // PRL: do this in two steps so that we move instead of copying.
            // We don't have a moving push_back in all compilers.
            outAliasedFiles.push_back(AliasedFile{});
            outAliasedFiles.back() =
               AliasedFile {
                  wxFileNameWrapper { fileName },
                  wxLongLong(blockBytes), fileName.FileExists()
               };
            aliasedFileHash[fileNameStr] = &outAliasedFiles.back();
         }
      }
   }
}

// Given a project and a list of aliased files that should no
// longer be external dependencies (selected by the user), replace
// all of those alias block files with disk block files.
static void RemoveDependencies(AudacityProject *project,
                               AliasedFileArray &aliasedFiles)
{
   const auto &dirManager = project->GetDirManager();

   ProgressDialog progress
      (_("Removing Dependencies"),
      _("Copying audio data into project..."));
   int updateResult = eProgressSuccess;

   // Hash aliasedFiles based on their full paths and
   // count total number of bytes to process.
   AliasedFileHash aliasedFileHash;
   wxLongLong totalBytesToProcess = 0;
   for (auto &aliasedFile : aliasedFiles) {
      totalBytesToProcess += aliasedFile.mByteCount;
      const wxString &fileNameStr = aliasedFile.mFileName.GetFullPath();
      aliasedFileHash[fileNameStr] = &aliasedFile;
   }

   BlockPtrArray blocks;
   GetAllSeqBlocks(project, &blocks);

   const sampleFormat format = project->GetDefaultFormat();
   ReplacedBlockFileHash blockFileHash;
   wxLongLong completedBytes = 0;
   for (const auto blockFile : blocks) {
      const auto &f = blockFile->f;
      if (f->IsAlias() && (blockFileHash.count( &*f ) == 0))
      {
         // f is an alias block we have not yet processed.
         auto aliasBlockFile = static_cast<AliasBlockFile*>( &*f );
         const wxFileName &fileName = aliasBlockFile->GetAliasedFileName();
         const wxString &fileNameStr = fileName.GetFullPath();

         if (aliasedFileHash.count(fileNameStr) == 0)
            // This aliased file was not selected to be replaced. Skip it.
            continue;

         // Convert it from an aliased file to an actual file in the project.
         auto len = aliasBlockFile->GetLength();
         BlockFilePtr newBlockFile;
         {
            SampleBuffer buffer(len, format);
            f->ReadData(buffer.ptr(), format, 0, len);
            newBlockFile =
               dirManager->NewSimpleBlockFile(buffer.ptr(), len, format);
         }

         // Update our hash so we know what block files we've done
         blockFileHash[ &*f ] = newBlockFile;

         // Update the progress bar
         completedBytes += SAMPLE_SIZE(format) * len;
         updateResult = progress.Update(completedBytes, totalBytesToProcess);
         if (updateResult != eProgressSuccess)
           break;
      }
   }

   // Above, we created a SimpleBlockFile contained in our project
   // to go with each AliasBlockFile that we wanted to migrate.
   // However, that didn't actually change any references to these
   // blockfiles in the Sequences, so we do that next...
   ReplaceBlockFiles(project, blockFileHash);
}

//
// DependencyDialog
//

class DependencyDialog final : public wxDialogWrapper
{
public:
   DependencyDialog(wxWindow *parent,
                    wxWindowID id,
                    AudacityProject *project,
                    AliasedFileArray &aliasedFiles,
                    bool isSaving);

private:
   void PopulateList();
   void PopulateOrExchange(ShuttleGui & S);

   // event handlers
   void OnCancel(wxCommandEvent& evt);
   void OnCopySelectedFiles(wxCommandEvent &evt);
   void OnList(wxListEvent &evt);
   void OnSize(wxSizeEvent &evt);
   void OnNo(wxCommandEvent &evt);
   void OnYes(wxCommandEvent &evt);

   void SaveFutureActionChoice();


   AudacityProject  *mProject;
   AliasedFileArray &mAliasedFiles;
   bool              mIsSaving;
   bool              mHasMissingFiles;
   bool              mHasNonMissingFiles;

   wxStaticText     *mMessageStaticText;
   wxListCtrl       *mFileListCtrl;
   wxButton         *mCopySelectedFilesButton;
   wxButton         *mCopyAllFilesButton;
   wxChoice         *mFutureActionChoice;

public:
   DECLARE_EVENT_TABLE()
};

enum {
   FileListID = 6000,
   CopySelectedFilesButtonID,
   FutureActionChoiceID
};

BEGIN_EVENT_TABLE(DependencyDialog, wxDialogWrapper)
   EVT_LIST_ITEM_SELECTED(FileListID, DependencyDialog::OnList)
   EVT_LIST_ITEM_DESELECTED(FileListID, DependencyDialog::OnList)
   EVT_BUTTON(CopySelectedFilesButtonID, DependencyDialog::OnCopySelectedFiles)
   EVT_SIZE(DependencyDialog::OnSize)
   EVT_BUTTON(wxID_NO, DependencyDialog::OnNo) // mIsSaving ? "Cancel Save" : "Save without Copying"
   EVT_BUTTON(wxID_YES, DependencyDialog::OnYes) // "Copy All Files (Safer)"
   EVT_BUTTON(wxID_CANCEL, DependencyDialog::OnCancel)  // "Cancel Save"
END_EVENT_TABLE()

DependencyDialog::DependencyDialog(wxWindow *parent,
                                   wxWindowID id,
                                   AudacityProject *project,
                                   AliasedFileArray &aliasedFiles,
                                   bool isSaving)
: wxDialogWrapper(parent, id, _("Project Depends on Other Audio Files"),
            wxDefaultPosition, wxDefaultSize,
            (isSaving ?
                  (wxDEFAULT_DIALOG_STYLE & ~wxCLOSE_BOX) : // no close box when saving
                  wxDEFAULT_DIALOG_STYLE) |
               wxRESIZE_BORDER),
   mProject(project),
   mAliasedFiles(aliasedFiles),
   mIsSaving(isSaving),
   mHasMissingFiles(false),
   mHasNonMissingFiles(false),
   mMessageStaticText(NULL),
   mFileListCtrl(NULL),
   mCopySelectedFilesButton(NULL),
   mCopyAllFilesButton(NULL),
   mFutureActionChoice(NULL)
{
   SetName(GetTitle());
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

const wxString kStdMsg =
_("Copying these files into your project will remove this dependency.\
\nThis is safer, but needs more disk space.");

const wxString kExtraMsgForMissingFiles =
_("\n\nFiles shown as MISSING have been moved or deleted and cannot be copied.\
\nRestore them to their original location to be able to copy into project.");

void DependencyDialog::PopulateOrExchange(ShuttleGui& S)
{
   S.SetBorder(5);
   S.StartVerticalLay();
   {
      mMessageStaticText = S.AddVariableText(kStdMsg, false);

      S.StartStatic(_("Project Dependencies"));
      {
         mFileListCtrl = S.Id(FileListID).AddListControlReportMode();
         mFileListCtrl->InsertColumn(0, _("Audio File"));
         mFileListCtrl->SetColumnWidth(0, 220);
         mFileListCtrl->InsertColumn(1, _("Disk Space"));
         mFileListCtrl->SetColumnWidth(1, 120);
         PopulateList();

         mCopySelectedFilesButton =
            S.Id(CopySelectedFilesButtonID).AddButton(
               _("Copy Selected Files"),
               wxALIGN_LEFT);
         mCopySelectedFilesButton->Enable(
            mFileListCtrl->GetSelectedItemCount() > 0);
         mCopySelectedFilesButton->SetDefault();
         mCopySelectedFilesButton->SetFocus();
      }
      S.EndStatic();

      S.StartHorizontalLay(wxALIGN_CENTRE);
      {
         if (mIsSaving) {
            S.Id(wxID_CANCEL).AddButton(_("Cancel Save"));
            S.Id(wxID_NO).AddButton(_("Save without Copying"));
         }
         else
            S.Id(wxID_NO).AddButton(_("Do Not Copy"));

         mCopyAllFilesButton =
            S.Id(wxID_YES).AddButton(_("Copy All Files (Safer)"));

         // Enabling mCopyAllFilesButton is also done in PopulateList,
         // but at its call above, mCopyAllFilesButton does not yet exist.
         mCopyAllFilesButton->Enable(!mHasMissingFiles);
      }
      S.EndHorizontalLay();

      if (mIsSaving)
      {
         S.StartHorizontalLay(wxALIGN_LEFT);
         {
            wxArrayString choices;
            /*i18n-hint: One of the choices of what you want Audacity to do when
            * Audacity finds a project depends on another file.*/
            choices.Add(_("Ask me"));
            choices.Add(_("Always copy all files (safest)"));
            choices.Add(_("Never copy any files"));
            mFutureActionChoice =
               S.Id(FutureActionChoiceID).AddChoice(
                  _("Whenever a project depends on other files:"),
                  _("Ask me"), &choices);
         }
         S.EndHorizontalLay();
      } else
      {
         mFutureActionChoice = NULL;
      }
   }
   S.EndVerticalLay();
   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();
}

void DependencyDialog::PopulateList()
{
   mFileListCtrl->DeleteAllItems();

   mHasMissingFiles = false;
   mHasNonMissingFiles = false;
   long i = 0;
   for (const auto &aliasedFile : mAliasedFiles) {
      const wxFileName &fileName = aliasedFile.mFileName;
      wxLongLong byteCount = (aliasedFile.mByteCount * 124) / 100;
      bool bOriginalExists = aliasedFile.mbOriginalExists;

      if (bOriginalExists)
      {
         mFileListCtrl->InsertItem(i, fileName.GetFullPath());
         mHasNonMissingFiles = true;
         mFileListCtrl->SetItemState(i, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);
      }
      else
      {
         mFileListCtrl->InsertItem(i, _("MISSING ") + fileName.GetFullPath());
         mHasMissingFiles = true;
         mFileListCtrl->SetItemState(i, 0, wxLIST_STATE_SELECTED); // Deselect.
         mFileListCtrl->SetItemTextColour(i, *wxRED);
      }
      mFileListCtrl->SetItem(i, 1, Internat::FormatSize(byteCount));
      mFileListCtrl->SetItemData(i, long(bOriginalExists));

      ++i;
   }

   wxString msg = kStdMsg;
   if (mHasMissingFiles)
      msg += kExtraMsgForMissingFiles;
   mMessageStaticText->SetLabel(msg);

   if (mCopyAllFilesButton)
      mCopyAllFilesButton->Enable(!mHasMissingFiles);
}

void DependencyDialog::OnList(wxListEvent &evt)
{
   if (!mCopySelectedFilesButton || !mFileListCtrl)
      return;

   wxString itemStr = evt.GetText();
   if (evt.GetData() == 0)
      // This list item is one of mAliasedFiles for which
      // the original is missing, i.e., moved or deleted.
      // wxListCtrl does not provide for items that are not
      // allowed to be selected, so always deselect these items.
      mFileListCtrl->SetItemState(evt.GetIndex(), 0, wxLIST_STATE_SELECTED); // Deselect.

   mCopySelectedFilesButton->Enable(
      mFileListCtrl->GetSelectedItemCount() > 0);
}

void DependencyDialog::OnSize(wxSizeEvent &evt)
{
   int fileListCtrlWidth, fileListCtrlHeight;
   mFileListCtrl->GetSize(&fileListCtrlWidth, &fileListCtrlHeight);

   // File path is column 0. File size is column 1.
   // File size column is always 120 px wide.
   // Also subtract 8 from file path column width for borders.
   mFileListCtrl->SetColumnWidth(0, fileListCtrlWidth - 120 - 8);
   mFileListCtrl->SetColumnWidth(1, 120);
   wxDialogWrapper::OnSize(evt);
}

void DependencyDialog::OnNo(wxCommandEvent & WXUNUSED(event))
{
   SaveFutureActionChoice();
   EndModal(wxID_NO);
}

void DependencyDialog::OnYes(wxCommandEvent & WXUNUSED(event))
{
   SaveFutureActionChoice();
   EndModal(wxID_YES);
}

void DependencyDialog::OnCopySelectedFiles(wxCommandEvent & WXUNUSED(event))
{
   AliasedFileArray aliasedFilesToDelete;

   long i = 0;
   for(auto iter = mAliasedFiles.begin(); iter != mAliasedFiles.end();) {
      if (mFileListCtrl->GetItemState(i, wxLIST_STATE_SELECTED)) {
         // Two-step move could be simplified when all compilers have C++11 vector
         aliasedFilesToDelete.push_back(AliasedFile{});
         aliasedFilesToDelete.back() = std::move(*iter);
         iter = mAliasedFiles.erase(iter);
      }
      else
         ++iter;
      ++i;
   }

   RemoveDependencies(mProject, aliasedFilesToDelete);
   PopulateList();

   if (mAliasedFiles.empty() || !mHasNonMissingFiles)
   {
      SaveFutureActionChoice();
      EndModal(wxID_NO);  // Don't need to remove dependencies
   }
}

void DependencyDialog::OnCancel(wxCommandEvent& WXUNUSED(event))
{
   if (mIsSaving)
   {
      int ret = wxMessageBox(
         _("If you proceed, your project will not be saved to disk. Is this what you want?"),
         _("Cancel Save"), wxICON_QUESTION | wxYES_NO | wxNO_DEFAULT, this);
      if (ret != wxYES)
         return;
   }

   EndModal(wxID_CANCEL);
}

void DependencyDialog::SaveFutureActionChoice()
{
   if (mFutureActionChoice)
   {
      wxString savePref;
      int sel = mFutureActionChoice->GetSelection();
      switch (sel)
      {
      case 1: savePref = wxT("copy"); break;
      case 2: savePref = wxT("never"); break;
      default: savePref = wxT("ask");
      }
      gPrefs->Write(wxT("/FileFormats/SaveProjectWithDependencies"),
                    savePref);
      gPrefs->Flush();
   }
}

// Checks for alias block files, modifies the project if the
// user requests it, and returns true if the user continues.
// Returns false only if the user clicks Cancel.
bool ShowDependencyDialogIfNeeded(AudacityProject *project,
                                  bool isSaving)
{
   AliasedFileArray aliasedFiles;
   FindDependencies(project, aliasedFiles);

   if (aliasedFiles.empty()) {
      if (!isSaving)
      {
         wxString msg =
_("Your project is currently self-contained; it does not depend on any external audio files. \
\n\nIf you change the project to a state that has external dependencies on imported \
files, it will no longer be self-contained. If you then Save without copying those files in, \
you may lose data.");
         wxMessageBox(msg,
                      _("Dependency Check"),
                      wxOK | wxICON_INFORMATION,
                      project);
      }
      return true; // Nothing to do.
   }

   if (isSaving)
   {
      wxString action =
         gPrefs->Read(
            wxT("/FileFormats/SaveProjectWithDependencies"),
            wxT("ask"));
      if (action == wxT("copy"))
      {
         // User always wants to remove dependencies
         RemoveDependencies(project, aliasedFiles);
         return true;
      }
      if (action == wxT("never"))
         // User never wants to remove dependencies
         return true;
   }

   DependencyDialog dlog(project, -1, project, aliasedFiles, isSaving);
   int returnCode = dlog.ShowModal();
   if (returnCode == wxID_CANCEL)
      return false;
   else if (returnCode == wxID_YES)
      RemoveDependencies(project, aliasedFiles);

   return true;
}

