/**********************************************************************

  Audacity: A Digital Audio Editor
  
  Dependencies.cpp
 
  Dominic Mazzoni

  The primary function provided in this source file is
  ShowDependencyDialogIfNeeded.  It checks a project to see if
  any of its WaveTracks contain AliasBlockFiles; if so it
  presents a dialog to the user and lets them copy those block
  files into the project, making it self-contained.

**********************************************************************/

#include <wx/defs.h>
#include <wx/dialog.h>
#include <wx/hashmap.h>
#include <wx/progdlg.h>
#include <wx/choice.h>

#include "BlockFile.h"
#include "Dependencies.h"
#include "DirManager.h"
#include "Internat.h"
#include "Project.h"
#include "ShuttleGui.h"
#include "Prefs.h"

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
void GetAllSeqBlocks(AudacityProject *project,
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
            for(i=0; i<(int)blocks->GetCount(); i++)
               outBlocks->Add(blocks->Item(i));
            node = node->GetNext();
         }
      }
      t = iter.Next();
   }
}

// Given an Audacity project and a hash mapping aliased block
// files to un-aliased block files, walks through all of the
// tracks and replaces each block file with its replacement.
// Note that this code respects reference-counting and thus the
// process of making a project self-contained is actually undoable.
void ReplaceBlockFiles(AudacityProject *project,
		       ReplacedBlockFileHash &hash)
{
   DirManager *dirManager = project->GetDirManager();
   BlockArray blocks;
   GetAllSeqBlocks(project, &blocks);

   int i;
   for(i=0; i<(int)blocks.GetCount(); i++) {
      if(hash.count(blocks[i]->f) > 0) {
         BlockFile *src = blocks[i]->f;
         BlockFile *dst = hash[src];

         dirManager->Deref(src);
         dirManager->Ref(dst);

         blocks[i]->f = dst;
      }
   }
}

// True = success
bool FindDependencies(AudacityProject *project,
		      AliasedFileArray *outAliasedFiles)
{
   sampleFormat format = project->GetDefaultFormat();

   BlockArray blocks;
   GetAllSeqBlocks(project, &blocks);

   AliasedFileHash aliasedFileHash;
   BoolBlockFileHash blockFileHash;

   int i;
   for(i=0; i<(int)blocks.GetCount(); i++) {
      BlockFile *f = blocks[i]->f;
      if (f->IsAlias() && !blockFileHash[f]) {
         blockFileHash[f] = true; // Don't count the same blockfile twice
         AliasBlockFile *aliasBlockFile = (AliasBlockFile *)f;
         wxFileName fileName = aliasBlockFile->GetAliasedFile();
         wxString fileNameStr = fileName.GetFullPath();
         int blockBytes = (SAMPLE_SIZE(format) *
                           aliasBlockFile->GetLength());
         if (aliasedFileHash[fileNameStr])
            aliasedFileHash[fileNameStr]->bytes += blockBytes;
         else {
            outAliasedFiles->Add(AliasedFile(fileName, blockBytes));
            aliasedFileHash[fileNameStr] =
               &((*outAliasedFiles)[outAliasedFiles->GetCount()-1]);
         }
      }
   } 
  
   return true;
}

// Given a project and a list of aliased files that should no
// longer be self-contained (selected by the user), replace
// all of those alias block files with disk block files
bool RemoveDependencies(AudacityProject *project,
			AliasedFileArray *aliasedFiles)
{
   DirManager *dirManager = project->GetDirManager();

   ProgressDialog *progress = new ProgressDialog(_("Removing Dependencies"),
                                                 _("Copying audio data into project..."));

   AliasedFileHash aliasedFileHash;
   ReplacedBlockFileHash blockFileHash;   

   wxLongLong totalBytesToProcess = 0;
   wxLongLong completedBytes = 0;
   unsigned int i;
   for(i=0; i<aliasedFiles->GetCount(); i++) {
      totalBytesToProcess += aliasedFiles->Item(i).bytes;
      wxString fileNameStr = aliasedFiles->Item(i).fileName.GetFullPath();
      aliasedFileHash[fileNameStr] = &aliasedFiles->Item(i);
   }
   
   sampleFormat format = project->GetDefaultFormat();

   BlockArray blocks;
   GetAllSeqBlocks(project, &blocks);

   int updateResult = eProgressSuccess;
   for(i=0; i<blocks.GetCount(); i++) {
      BlockFile *f = blocks[i]->f;
      if (f->IsAlias() && !blockFileHash[f]) {
         AliasBlockFile *aliasBlockFile = (AliasBlockFile *)f;
         wxFileName fileName = aliasBlockFile->GetAliasedFile();
         wxString fileNameStr = fileName.GetFullPath();

         if (!aliasedFileHash[fileNameStr])
            continue;

         // Convert it from an aliased file to an actual file in the project
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
         
         //TODO: add a cancel checker to break if the user clicks cancel.
         /*
         cancelled = !mProgress->Update(i, fileTotalFrames);
         if (cancelled)
            break;*/
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

   return true;
}

//
// DependencyDialog
//

enum {
   FileListID = 6000,
   CopyID,
   FutureActionID
};

wxString kDependencyDialogPrompt =
_("Copying the following files into your project will remove this dependency.\
\nThis needs more disk space, but is safer.");

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
   void OnList(wxListEvent &evt);
   void OnCopy(wxCommandEvent &evt);
   void OnNo(wxCommandEvent &evt);
   void OnYes(wxCommandEvent &evt);

   void PopulateOrExchange(ShuttleGui & S);
   void SaveChoice();
   
   virtual void OnCancel(wxCommandEvent& evt);
   
   AudacityProject  *mProject;
   AliasedFileArray *mAliasedFiles;
   wxListCtrl       *mFileList;
   wxButton         *mCopyButton;
   wxChoice         *mChoice;
   bool              mIsSaving;

public:
   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(DependencyDialog, wxDialog)
   EVT_LIST_ITEM_SELECTED(FileListID, DependencyDialog::OnList)
   EVT_BUTTON(CopyID, DependencyDialog::OnCopy)
   EVT_BUTTON(wxID_NO, DependencyDialog::OnNo)
   EVT_BUTTON(wxID_YES, DependencyDialog::OnYes)
   EVT_BUTTON(wxID_CANCEL, DependencyDialog::OnCancel) // seems to be needed
END_EVENT_TABLE()

DependencyDialog::DependencyDialog(wxWindow *parent,
                                   wxWindowID id,
                                   AudacityProject *project,
                                   AliasedFileArray *aliasedFiles,
                                   bool isSaving):
   wxDialog(parent, id, _("Project depends on other audio files"),
            wxDefaultPosition, wxDefaultSize,
            isSaving ? wxDEFAULT_DIALOG_STYLE & (~wxCLOSE_BOX) :
                       wxDEFAULT_DIALOG_STYLE), // no close box when saving
   mProject(project),
   mAliasedFiles(aliasedFiles),
   mFileList(NULL),
   mCopyButton(NULL),
   mIsSaving(isSaving)
{
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

void DependencyDialog::PopulateOrExchange(ShuttleGui& S)
{
   S.SetBorder(5);
   S.StartVerticalLay();
   {
      S.AddVariableText(kDependencyDialogPrompt, false);

      S.StartStatic(_("Project Dependencies"));
      {  
         mFileList = S.Id(FileListID).AddListControlReportMode();
         mFileList->InsertColumn(0, _("Audio file"));
         mFileList->SetColumnWidth(0, 220);
         mFileList->InsertColumn(1, _("Disk space"));
         mFileList->SetColumnWidth(1, 120);
         PopulateList();

         mCopyButton = S.Id(CopyID).AddButton
            (_("Copy Selected Audio Into Project"), wxALIGN_LEFT);
      }
      S.EndStatic();

      S.StartHorizontalLay(wxALIGN_CENTRE);
      {
         if (mIsSaving) {
            S.Id(wxID_CANCEL).AddButton(_("Cancel Save"));
         }
         S.Id(wxID_NO).AddButton(_("Do Not Copy Any Audio"));

         S.Id(wxID_YES).AddButton(_("Copy All Audio into Project (Safer)"));
      }
      S.EndHorizontalLay();
      
      if (mIsSaving)
      {
         S.StartHorizontalLay(wxALIGN_LEFT);
         {
            wxArrayString choices;
            choices.Add(_("Ask me"));
            choices.Add(_("Always copy all audio (safest)"));
            choices.Add(_("Never copy any audio"));
            mChoice = S.Id(FutureActionID).AddChoice(
               _("Whenever a project depends on other files:"),
               _("Ask me"), &choices);
         }
         S.EndHorizontalLay();
      } else
      {
         mChoice = NULL;
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
   mFileList->DeleteAllItems();

   unsigned int i;
   for(i=0; i<mAliasedFiles->GetCount(); i++) {
      wxFileName fileName = mAliasedFiles->Item(i).fileName;
      wxLongLong bytes = (mAliasedFiles->Item(i).bytes * 124) / 100;

      mFileList->InsertItem(i, fileName.GetFullPath());
      mFileList->SetItem(i, 1, Internat::FormatSize(bytes));
      mFileList->SetItemState(i, wxLIST_STATE_SELECTED, wxLIST_STATE_SELECTED);
   }
}

void DependencyDialog::OnList(wxListEvent &evt)
{
   if (!mCopyButton || !mFileList)
      return;

   int selectedCount = 0;
   unsigned int i;
   for(i=0; i<mAliasedFiles->GetCount(); i++) {
      if (mFileList->GetItemState(i, wxLIST_STATE_SELECTED))
         selectedCount++;
   }

   if (selectedCount > 0)
      mCopyButton->Enable(true);
   else
      mCopyButton->Enable(false);
}

void DependencyDialog::OnNo(wxCommandEvent &evt)
{
   SaveChoice();
   EndModal(wxID_NO);
}

void DependencyDialog::OnYes(wxCommandEvent &evt)
{
   SaveChoice();
   EndModal(wxID_YES);
}

void DependencyDialog::OnCopy(wxCommandEvent &evt)
{
   AliasedFileArray ToDelete;

   int i;
   // Count backwards so we can remove as we go
   for(i=(int)mAliasedFiles->GetCount()-1; i>=0; i--) {
      if (mFileList->GetItemState(i, wxLIST_STATE_SELECTED)) {
         ToDelete.Add(mAliasedFiles->Item(i));
         mAliasedFiles->RemoveAt(i);
      }
   }  

   RemoveDependencies(mProject, &ToDelete);
   PopulateList();

   if (mAliasedFiles->GetCount() == 0) {
      SaveChoice();
      EndModal(wxID_NO);  // Don't need to remove dependencies
   }
}

void DependencyDialog::OnCancel(wxCommandEvent& evt)
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

void DependencyDialog::SaveChoice()
{
   if (mChoice)
   {
      wxString savePref;   
      int sel = mChoice->GetSelection();
      switch (sel)
      {
      case 1: savePref = wxT("copy"); break;
      case 2: savePref = wxT("never"); break;
      default: savePref = wxT("ask");
      }
      gPrefs->Write(wxT("/FileFormats/SaveProjectWithDependencies"),
                    savePref);
   }
}

// Checks for alias block files, modifies the project if the
// user requests it, and returns True if the user continues.
// Returns false only if the user clicks Cancel.
bool ShowDependencyDialogIfNeeded(AudacityProject *project,
                                  bool isSaving)
{
   AliasedFileArray aliasedFiles;
   bool success = FindDependencies(project,
                                   &aliasedFiles);
   if (!success)
      return true; // We failed, but that shouldn't count as a cancel

   if (aliasedFiles.GetCount() == 0) {
      if (!isSaving) {
         wxMessageBox(_("Your project is self-contained; it does not depend on any external audio files."),
                      _("Dependency check"),
                      wxOK | wxICON_INFORMATION,
                      project);
      }

      return true; // Nothing to do
   }
   
   if (isSaving)
   {
      wxString action = gPrefs->Read(
         wxT("/FileFormats/SaveProjectWithDependencies"), wxT("ask"));

      if (action == wxT("copy"))
      {
         // User always wants to remove dependencies
         RemoveDependencies(project, &aliasedFiles);
         return true;
      }

      if (action == wxT("never"))
      {
         // User never wants to remove dependencies
         return true;
      }

      // Fall through to show the dialog
   }

   DependencyDialog dlog(project, -1, project, &aliasedFiles, isSaving);
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL) {
      return false;
   }

   if (dlog.GetReturnCode() == wxID_YES) {
      RemoveDependencies(project, &aliasedFiles);
   }

   return true;
}

// Indentation settings for Vim and Emacs.
// Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

