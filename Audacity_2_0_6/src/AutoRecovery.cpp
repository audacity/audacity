/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2.  See License.txt.

   AutoRecovery.cpp

*******************************************************************//**

\class AutoRecoveryDialog
\brief The AutoRecoveryDialog prompts the user whether to
recover previous Audacity projects that were closed incorrectly.

*//********************************************************************/

#include "AutoRecovery.h"
#include "Audacity.h"
#include "AudacityApp.h"
#include "FileNames.h"
#include "blockfile/SimpleBlockFile.h"

#include <wx/wxprec.h>
#include <wx/filefn.h>
#include <wx/dir.h>
#include <wx/dialog.h>
#include <wx/app.h>

enum {
   ID_RECOVER_ALL = 10000,
   ID_RECOVER_NONE,
   ID_QUIT_AUDACITY,
   ID_FILE_LIST
};

class AutoRecoveryDialog : public wxDialog
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
   wxDialog(parent, -1, _("Automatic Crash Recovery"),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE & (~wxCLOSE_BOX)) // no close box
{
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

BEGIN_EVENT_TABLE(AutoRecoveryDialog, wxDialog)
   EVT_BUTTON(ID_RECOVER_ALL, AutoRecoveryDialog::OnRecoverAll)
   EVT_BUTTON(ID_RECOVER_NONE, AutoRecoveryDialog::OnRecoverNone)
   EVT_BUTTON(ID_QUIT_AUDACITY, AutoRecoveryDialog::OnQuitAudacity)
END_EVENT_TABLE()

void AutoRecoveryDialog::PopulateOrExchange(ShuttleGui& S)
{
   S.SetBorder(5);
   S.StartVerticalLay();
   {
      S.AddVariableText(_("Some projects were not saved properly the last time Audacity was run.\nFortunately, the following projects can be automatically recovered:"), false);

      S.StartStatic(_("Recoverable projects"));
      {
         mFileList = S.Id(ID_FILE_LIST).AddListControlReportMode();
         /*i18n-hint: (noun).  It's the name of the project to recover.*/
         mFileList->InsertColumn(0, _("Name"));
         mFileList->SetColumnWidth(0, 220);
         PopulateList();
      }
      S.EndStatic();

      S.AddVariableText(_("After recovery, save the project to save the changes to disk."), false);

      S.StartHorizontalLay(true);
      {
         S.Id(ID_QUIT_AUDACITY).AddButton(_("Quit Audacity"));
         S.Id(ID_RECOVER_NONE).AddButton(_("Discard Projects"));
         S.Id(ID_RECOVER_ALL).AddButton(_("Recover Projects"));
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   Layout();
   Fit();
   SetMinSize(GetSize());
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
      mFileList->InsertItem(i++, wxFileName(filename).GetName());

   mFileList->SetColumnWidth(0, wxLIST_AUTOSIZE);
}

void AutoRecoveryDialog::OnQuitAudacity(wxCommandEvent & WXUNUSED(event))
{
   EndModal(ID_QUIT_AUDACITY);
}

void AutoRecoveryDialog::OnRecoverNone(wxCommandEvent & WXUNUSED(event))
{
   int ret = wxMessageBox(
      _("Are you sure you want to discard all projects?\n\nChoosing \"Yes\" discards all projects immediately."),
      _("Confirm Discard Projects"), wxICON_QUESTION | wxYES_NO | wxNO_DEFAULT, this);

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
      wxMessageBox(_("Could not enumerate files in auto save directory."),
                   _("Error"), wxICON_STOP);
      return false;
   }

   wxString filename;
   bool c = dir.GetFirst(&filename, wxT("*.autosave"), wxDIR_FILES);

   return c;
}

static bool RemoveAllAutoSaveFiles()
{
   wxArrayString files;
   wxDir::GetAllFiles(FileNames::AutoSaveDir(), &files,
                      wxT("*.autosave"), wxDIR_FILES);

   for (unsigned int i = 0; i < files.GetCount(); i++)
   {
      if (!wxRemoveFile(files[i]))
      {
         // I don't think this error message is actually useful.
         // -dmazzoni
         //wxMessageBox(wxT("Could not remove auto save file: " + files[i]),
         //             _("Error"), wxICON_STOP);
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
      wxMessageBox(_("Could not enumerate files in auto save directory."),
                   _("Error"), wxICON_STOP);
      return false;
   }

   // Open a project window for each auto save file
   wxString filename;
   AudacityProject* proj = NULL;

   wxArrayString files;
   wxDir::GetAllFiles(FileNames::AutoSaveDir(), &files,
                      wxT("*.autosave"), wxDIR_FILES);

   for (unsigned int i = 0; i < files.GetCount(); i++)
   {
      if (*pproj)
      {
         // Reuse existing project window
         proj = *pproj;
         *pproj = NULL;
      } else
      {
         // Create new project window
         proj = CreateNewAudacityProject();
      }

      // Open project. When an auto-save file has been opened successfully,
      // the opened auto-save file is automatically deleted and a new one
      // is created.
      proj->OpenFile(files[i], false);
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
      AutoRecoveryDialog dlg(*pproj);
      int ret = dlg.ShowModal();

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

////////////////////////////////////////////////////////////////////////////
/// Recording recovery handler

RecordingRecoveryHandler::RecordingRecoveryHandler(AudacityProject* proj)
{
   mProject = proj;
   mChannel = -1;
   mNumChannels = -1;
}

bool RecordingRecoveryHandler::HandleXMLTag(const wxChar *tag,
                                            const wxChar **attrs)
{
   if (wxStrcmp(tag, wxT("simpleblockfile")) == 0)
   {
      // Check if we have a valid channel and numchannels
      if (mChannel < 0 || mNumChannels < 0 || mChannel >= mNumChannels)
      {
         // This should only happen if there is a bug
         wxASSERT(false);
         return false;
      }

      // We need to find the track and sequence where the blockfile belongs
      WaveTrackArray tracks = mProject->GetTracks()->GetWaveTrackArray(false);
      int index = tracks.GetCount() - mNumChannels + mChannel;
      if (index < 0 || index >= (int)tracks.GetCount())
      {
         // This should only happen if there is a bug
         wxASSERT(false);
         return false;
      }
      WaveTrack* track = tracks.Item(index);
      WaveClip*  clip  = track->GetLastOrCreateClip();
      Sequence* seq = clip->GetSequence();

      // Load the blockfile from the XML
      BlockFile* blockFile = NULL;
      DirManager* dirManager = mProject->GetDirManager();
      dirManager->SetLoadingFormat(seq->GetSampleFormat());
      dirManager->SetLoadingTarget(&blockFile);
      if (!dirManager->HandleXMLTag(tag, attrs) || !blockFile)
      {
         // This should only happen if there is a bug
         wxASSERT(false);
         return false;
      }

      seq->AppendBlockFile(blockFile);
      clip->UpdateEnvelopeTrackLen();

   } else if (wxStrcmp(tag, wxT("recordingrecovery")) == 0)
   {
      // loop through attrs, which is a null-terminated list of
      // attribute-value pairs
      long nValue;
      while(*attrs)
      {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
            break;

         const wxString strValue = value;
         //this channels value does not correspond to WaveTrack::Left/Right/Mono, but which channel of the recording device
         //it came from, and thus we can't use XMLValueChecker::IsValidChannel on it.  Rather we compare to the next attribute value.
         if (wxStrcmp(attr, wxT("channel")) == 0)
         {
            if (!XMLValueChecker::IsGoodInt(strValue) || !strValue.ToLong(&nValue) || nValue < 0)
               return false;
            mChannel = nValue;
         }
         else if (wxStrcmp(attr, wxT("numchannels")) == 0)
         {
            if (!XMLValueChecker::IsGoodInt(strValue) || !strValue.ToLong(&nValue) ||
                  (nValue < 1))
               return false;
            if(mChannel >= nValue )
               return false;
            mNumChannels = nValue;
         }

      }
   }

   return true;
}

XMLTagHandler* RecordingRecoveryHandler::HandleXMLChild(const wxChar *tag)
{
   if (wxStrcmp(tag, wxT("simpleblockfile")) == 0)
      return this; // HandleXMLTag also handles <simpleblockfile>

   return NULL;
}
