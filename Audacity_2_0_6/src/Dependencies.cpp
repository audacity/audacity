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

#include <wx/button.h>
#include <wx/defs.h>
#include <wx/dialog.h>
#include <wx/filename.h>
#include <wx/hashmap.h>
#include <wx/progdlg.h>
#include <wx/choice.h>

#include "BlockFile.h"
#include "Dependencies.h"
#include "DirManager.h"
#include "Internat.h"
#include "Prefs.h"
#include "Project.h"
#include "ShuttleGui.h"
#include "Track.h"

// Note, this #include must occur here, not up with the others!
// It must be between the WX_DECLARE_OBJARRAY and WX_DEFINE_OBJARRAY.
#include <wx/arrimpl.cpp>

WX_DEFINE_OBJARRAY( AliasedFileArray );

WX_DECLARE_HASH_MAP(wxString, AliasedFile *,
                    wxStringHash, wxStringEqual, AliasedFileHash);

WX_DECLARE_HASH_MAP(BlockFile *, BlockFile *,
                    wxPointerHash, wxPointerEqual, ReplacedBlockFileHash);

WX_DECLARE_HASH_MAP(BlockFile *, bool,
                    wxPointerHash, wxPointerEqual, BoolBlockFileHash);

// Given a project, returns a single array of all SeqBlocks
// in the current set of tracks.  Enumerating that array allows
// you to process all block files in the current set.
static void GetAllSeqBlocks(AudacityProject *project,
                            BlockArray *outBlocks)
{
   TrackList *tracks = project->GetTracks();
   TrackListIterator iter(tracks);
   Track *t = iter.First();
   while (t) {
      if (t->GetKind() == Track::Wave) {
         WaveTrack *waveTrack = (WaveTrack *)t;
         WaveClipList::compatibility_iterator node = waveTrack->GetClipIterator();
         while(node) {
            WaveClip *clip = node->GetData();
            Sequence *sequence = clip->GetSequence();
            BlockArray *blocks = sequence->GetBlockArray();
            int i;
            for (i = 0; i < (int)blocks->GetCount(); i++)
               outBlocks->Add(blocks->Item(i));
            node = node->GetNext();
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
   DirManager *dirManager = project->GetDirManager();
   BlockArray blocks;
   GetAllSeqBlocks(project, &blocks);

   int i;
   for (i = 0; i < (int)blocks.GetCount(); i++) {
      if (hash.count(blocks[i]->f) > 0) {
         BlockFile *src = blocks[i]->f;
         BlockFile *dst = hash[src];

         dirManager->Deref(src);
         dirManager->Ref(dst);

         blocks[i]->f = dst;
      }
   }
}

void FindDependencies(AudacityProject *project,
                      AliasedFileArray *outAliasedFiles)
{
   sampleFormat format = project->GetDefaultFormat();

   BlockArray blocks;
   GetAllSeqBlocks(project, &blocks);

   AliasedFileHash aliasedFileHash;
   BoolBlockFileHash blockFileHash;

   int i;
   for (i = 0; i < (int)blocks.GetCount(); i++) {
      BlockFile *f = blocks[i]->f;
      if (f->IsAlias() && (blockFileHash.count(f) == 0))
      {
         // f is an alias block we have not yet counted.
         blockFileHash[f] = true; // Don't count the same blockfile twice.
         AliasBlockFile *aliasBlockFile = (AliasBlockFile *)f;
         wxFileName fileName = aliasBlockFile->GetAliasedFileName();

         // In DirManager::ProjectFSCK(), if the user has chosen to
         // "Replace missing audio with silence", the code there puts in an empty wxFileName.
         // Don't count those in dependencies.
         if (!fileName.IsOk())
            continue;

         wxString fileNameStr = fileName.GetFullPath();
         int blockBytes = (SAMPLE_SIZE(format) *
                           aliasBlockFile->GetLength());
         if (aliasedFileHash.count(fileNameStr) > 0)
            // Already put this AliasBlockFile in aliasedFileHash.
            // Update block count.
            aliasedFileHash[fileNameStr]->mByteCount += blockBytes;
         else
         {
            // Haven't counted this AliasBlockFile yet.
            // Add to return array and internal hash.
            outAliasedFiles->Add(AliasedFile(fileName,
                                             blockBytes,
                                             fileName.FileExists()));
            aliasedFileHash[fileNameStr] =
               &((*outAliasedFiles)[outAliasedFiles->GetCount()-1]);
         }
      }
   }
}

// Given a project and a list of aliased files that should no
// longer be external dependencies (selected by the user), replace
// all of those alias block files with disk block files.
static void RemoveDependencies(AudacityProject *project,
                                 AliasedFileArray *aliasedFiles)
{
   DirManager *dirManager = project->GetDirManager();

   ProgressDialog *progress =
      new ProgressDialog(_("Removing Dependencies"),
                         _("Copying audio data into project..."));
   int updateResult = eProgressSuccess;

   // Hash aliasedFiles based on their full paths and
   // count total number of bytes to process.
   AliasedFileHash aliasedFileHash;
   wxLongLong totalBytesToProcess = 0;
   unsigned int i;
   for (i = 0; i < aliasedFiles->GetCount(); i++) {
      totalBytesToProcess += aliasedFiles->Item(i).mByteCount;
      wxString fileNameStr = aliasedFiles->Item(i).mFileName.GetFullPath();
      aliasedFileHash[fileNameStr] = &aliasedFiles->Item(i);
   }

   BlockArray blocks;
   GetAllSeqBlocks(project, &blocks);

   const sampleFormat format = project->GetDefaultFormat();
   ReplacedBlockFileHash blockFileHash;
   wxLongLong completedBytes = 0;
   for (i = 0; i < blocks.GetCount(); i++) {
      BlockFile *f = blocks[i]->f;
      if (f->IsAlias() && (blockFileHash.count(f) == 0))
      {
         // f is an alias block we have not yet processed.
         AliasBlockFile *aliasBlockFile = (AliasBlockFile *)f;
         wxFileName fileName = aliasBlockFile->GetAliasedFileName();
         wxString fileNameStr = fileName.GetFullPath();

         if (aliasedFileHash.count(fileNameStr) == 0)
            // This aliased file was not selected to be replaced. Skip it.
            continue;

         // Convert it from an aliased file to an actual file in the project.
         unsigned int len = aliasBlockFile->GetLength();
         samplePtr buffer = NewSamples(len, format);
         f->ReadData(buffer, format, 0, len);
         BlockFile *newBlockFile =
            dirManager->NewSimpleBlockFile(buffer, len, format);
         DeleteSamples(buffer);

         // Update our hash so we know what block files we've done
         blockFileHash[f] = newBlockFile;

         // Update the progress bar
         completedBytes += SAMPLE_SIZE(format) * len;
         updateResult = progress->Update(completedBytes, totalBytesToProcess);
         if (updateResult != eProgressSuccess)
           break;
      }
   }

   // Above, we created a SimpleBlockFile contained in our project
   // to go with each AliasBlockFile that we wanted to migrate.
   // However, that didn't actually change any references to these
   // blockfiles in the Sequences, so we do that next...
   ReplaceBlockFiles(project, blockFileHash);

   // Subtract one from reference count of new block files; they're
   // now all referenced the proper number of times by the Sequences
   ReplacedBlockFileHash::iterator it;
   for( it = blockFileHash.begin(); it != blockFileHash.end(); ++it )
   {
      BlockFile *f = it->second;
      dirManager->Deref(f);
   }

   delete progress;
}

//
// DependencyDialog
//

class DependencyDialog : public wxDialog
{
public:
   DependencyDialog(wxWindow *parent,
                    wxWindowID id,
                    AudacityProject *project,
                    AliasedFileArray *aliasedFiles,
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
   AliasedFileArray *mAliasedFiles;
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

BEGIN_EVENT_TABLE(DependencyDialog, wxDialog)
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
                                   AliasedFileArray *aliasedFiles,
                                   bool isSaving)
: wxDialog(parent, id, _("Project Depends on Other Audio Files"),
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
   unsigned int i;
   for (i = 0; i < mAliasedFiles->GetCount(); i++) {
      wxFileName fileName = mAliasedFiles->Item(i).mFileName;
      wxLongLong byteCount = (mAliasedFiles->Item(i).mByteCount * 124) / 100;
      bool bOriginalExists = mAliasedFiles->Item(i).mbOriginalExists;

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
   wxDialog::OnSize(evt);
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

   int i;
   // Count backwards so we can remove as we go
   for(i=(int)mAliasedFiles->GetCount()-1; i>=0; i--) {
      if (mFileListCtrl->GetItemState(i, wxLIST_STATE_SELECTED)) {
         aliasedFilesToDelete.Add(mAliasedFiles->Item(i));
         mAliasedFiles->RemoveAt(i);
      }
   }

   RemoveDependencies(mProject, &aliasedFilesToDelete);
   PopulateList();

   if ((mAliasedFiles->GetCount() == 0) || !mHasNonMissingFiles)
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
   FindDependencies(project, &aliasedFiles);

   if (aliasedFiles.GetCount() == 0) {
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
         RemoveDependencies(project, &aliasedFiles);
         return true;
      }
      if (action == wxT("never"))
         // User never wants to remove dependencies
         return true;
   }

   DependencyDialog dlog(project, -1, project, &aliasedFiles, isSaving);
   int returnCode = dlog.ShowModal();
   if (returnCode == wxID_CANCEL)
      return false;
   else if (returnCode == wxID_YES)
      RemoveDependencies(project, &aliasedFiles);

   return true;
}

