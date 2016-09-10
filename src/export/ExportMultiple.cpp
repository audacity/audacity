/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMultiple.cpp

  Dominic Mazzoni

*******************************************************************//**

\class ExportMultiple
\brief Presents a dialog box allowing the user to export multiple files
  either by exporting each track as a separate file, or by
  exporting each label as a separate file.

*//********************************************************************/

#include "../Audacity.h"
#include "ExportMultiple.h"

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/dirdlg.h>
#include <wx/dynarray.h>
#include <wx/event.h>
#include <wx/filedlg.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/textdlg.h>

#include "Export.h"

#include "../Internat.h"
#include "../FileFormats.h"
#include "../FileNames.h"
#include "../LabelTrack.h"
#include "../Project.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "../Tags.h"
#include "../WaveTrack.h"
#include "../widgets/HelpSystem.h"


/* define our dynamic array of export settings */
#include <wx/arrimpl.cpp>     // much hackery
WX_DEFINE_OBJARRAY( ExportKitArray )

enum {
   FormatID = 10001,
   OptionsID,
   DirID,
   CreateID,
   ChooseID,
   LabelID,
   FirstID,
   FirstFileNameID,
   TrackID,
   ByNameAndNumberID,
   ByNameID,
   ByNumberID,
   PrefixID,
   OverwriteID
};

//
// ExportMultiple methods
//

BEGIN_EVENT_TABLE(ExportMultiple, wxDialogWrapper)
   EVT_CHOICE(FormatID, ExportMultiple::OnFormat)
//   EVT_BUTTON(OptionsID, ExportMultiple::OnOptions)
   EVT_BUTTON(CreateID, ExportMultiple::OnCreate)
   EVT_BUTTON(ChooseID, ExportMultiple::OnChoose)
   EVT_BUTTON(wxID_OK, ExportMultiple::OnExport)
   EVT_BUTTON(wxID_CANCEL, ExportMultiple::OnCancel)
   EVT_RADIOBUTTON(LabelID, ExportMultiple::OnLabel)
   EVT_RADIOBUTTON(TrackID, ExportMultiple::OnTrack)
   EVT_RADIOBUTTON(ByNameAndNumberID, ExportMultiple::OnByName)
   EVT_RADIOBUTTON(ByNameID, ExportMultiple::OnByName)
   EVT_RADIOBUTTON(ByNumberID, ExportMultiple::OnByNumber)
   EVT_CHECKBOX(FirstID, ExportMultiple::OnFirst)
   EVT_TEXT(FirstFileNameID, ExportMultiple::OnFirstFileName)
   EVT_TEXT(PrefixID, ExportMultiple::OnPrefix)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(SuccessDialog, wxDialogWrapper)
   EVT_LIST_KEY_DOWN(wxID_ANY, SuccessDialog::OnKeyDown)
   EVT_LIST_ITEM_ACTIVATED(wxID_ANY, SuccessDialog::OnItemActivated) // happens when <enter> is pressed with list item having focus
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(MouseEvtHandler, wxEvtHandler)
   EVT_LEFT_DCLICK(MouseEvtHandler::OnMouse)
END_EVENT_TABLE()

ExportMultiple::ExportMultiple(AudacityProject *project)
: wxDialogWrapper(project, wxID_ANY, wxString(_("Export Multiple")))
{
   SetName(GetTitle());

   mProject = project;
   mTracks = project->GetTracks();
   // Construct an array of non-owning pointers
   for (const auto &plugin : mExporter.GetPlugins())
      mPlugins.push_back(plugin.get());

   this->CountTracksAndLabels();

   mBook = NULL;

   ShuttleGui S(this, eIsCreatingFromPrefs);

   // Creating some of the widgets cause events to fire
   // and we don't want that until after we're completely
   // created.  (Observed on Windows)
   mInitialized = false;
   PopulateOrExchange(S);
   mInitialized = true;

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   EnableControls();
}

ExportMultiple::~ExportMultiple()
{
}

void ExportMultiple::CountTracksAndLabels()
{
   mLabels = NULL;
   mNumLabels = 0;
   mNumWaveTracks = 0;

   const Track* pTrack;
   TrackListConstIterator iter;
   for (pTrack = iter.First(mTracks); pTrack != NULL; pTrack = iter.Next())
   {
      switch (pTrack->GetKind())
      {
         // Count WaveTracks, and for linked pairs, count only the second of the pair.
         case Track::Wave:
         {
            if (!pTrack->GetMute() && !pTrack->GetLinked()) // Don't count muted tracks.
               mNumWaveTracks++;
            break;
         }

         // Only support one label track???
         case Track::Label:
         {
            // Supports only one LabelTrack.
            if (mLabels == NULL) {
               mLabels = (LabelTrack*)pTrack;
               mNumLabels = mLabels->GetNumLabels();
            }
            break;
         }
      }
   }
}

int ExportMultiple::ShowModal()
{
   // Cannot export if all audio tracks are muted.
   if (mNumWaveTracks == 0)
   {
      ::wxMessageBox(_("All audio is muted."),
                     _("Cannot Export Multiple"),
                     wxOK | wxCENTRE, this);
      return wxID_CANCEL;
   }

   if ((mNumWaveTracks == 1) && (mNumLabels < 1))
   {
      ::wxMessageBox(_(
"You have only one unmuted Audio Track and no applicable \
\nlabels, so you cannot export to separate audio files."),
                     _("Cannot Export Multiple"),
                     wxOK | wxCENTRE, this);
      return wxID_CANCEL;
   }

   if (mNumLabels < 1) {
      mLabel->Enable(false);
      mTrack->SetValue(true);
      mLabel->SetValue(false);
   }

   if (mNumWaveTracks < 2) {
      mTrack->Enable(false);
      mLabel->SetValue(true);
      mTrack->SetValue(false);
   }

   EnableControls();

   return wxDialogWrapper::ShowModal();
}

void ExportMultiple::PopulateOrExchange(ShuttleGui& S)
{
   wxString name = mProject->GetName();
   wxString defaultFormat = gPrefs->Read(wxT("/Export/Format"), wxT("WAV"));

   wxArrayString formats;
   mPluginIndex = -1;
   mFilterIndex = 0;

   {
      int i = -1;
      for (const auto &pPlugin : mPlugins)
      {
         ++i;
         for (int j = 0; j < pPlugin->GetFormatCount(); j++)
         {
            formats.Add(mPlugins[i]->GetDescription(j));
            if (mPlugins[i]->GetFormat(j) == defaultFormat) {
               mPluginIndex = i;
               mSubFormatIndex = j;
            }
            if (mPluginIndex == -1) mFilterIndex++;
         }
      }
   }

   if (mPluginIndex == -1)
   {
      mPluginIndex = 0;
      mFilterIndex = 0;
      mSubFormatIndex = 0;
   }

   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, true);
   {
      S.SetBorder(5);
      S.StartStatic(_("Export files to:"), true);
      {
         S.StartMultiColumn(4, true);
         {
            mDir = S.Id(DirID)
               .TieTextBox(_("Folder:"),
                           wxT("/Export/MultiplePath"),
                           gPrefs->Read(wxT("/Export/Path"), ::wxGetCwd()),
                           64);
            S.Id(ChooseID).AddButton(_("Choose..."));
            S.Id(CreateID).AddButton(_("Create"));

            mFormat = S.Id(FormatID)
               .TieChoice(_("Format:"),
                          wxT("/Export/MultipleFormat"),
                          formats[mFilterIndex],
                          formats,
                          formats);
            S.AddVariableText(wxT(""), false);
            S.AddVariableText(wxT(""), false);

            S.AddPrompt(_("Options:"));
            if (!mBook)
            {
               mBook = safenew wxSimplebook(S.GetParent(), OptionsID, wxDefaultPosition, wxDefaultSize, wxBORDER_STATIC);
               for (const auto &pPlugin : mPlugins)
               {
                  for (int j = 0; j < pPlugin->GetFormatCount(); j++)
                  {
                     mBook->AddPage(pPlugin->OptionsCreate(mBook, j), wxEmptyString);
                  }
               }
               mBook->ChangeSelection(mFormat->GetSelection());
            }
            S.AddWindow(mBook);
            S.AddVariableText(wxT(""), false);
            S.AddVariableText(wxT(""), false);
         }
         S.EndMultiColumn();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.StartHorizontalLay(wxEXPAND, false);
   {
      S.SetBorder(5);
      S.StartStatic(_("Split files based on:"), 1);
      {
         // Row 1
         S.SetBorder(1);
         mTrack = S.Id(TrackID)
            .AddRadioButton(wxString(_("Tracks")));
         mTrack->SetName(_("Tracks"));

         // Row 2
         S.SetBorder(1);
         mLabel = S.Id(LabelID).AddRadioButtonToGroup(wxString(_("Labels")));
         mLabel->SetName(_("Labels"));
         S.SetBorder(3);

         S.StartMultiColumn(2, wxEXPAND);
         S.SetStretchyCol(1);
         {
            // Row 3 (indented)
            S.AddVariableText(wxT("   "), false);
            mFirst = S.Id(FirstID)
               .AddCheckBox(_("Include audio before first label"), wxT("false"));

            // Row 4
            S.AddVariableText(wxT(""), false);
            S.StartMultiColumn(2, wxEXPAND);
            S.SetStretchyCol(1);
            {
               mFirstFileLabel = S.AddVariableText(_("First file name:"), false);
               mFirstFileName = S.Id(FirstFileNameID)
                  .Prop(1).TieTextBox(wxT(""),
                              name,
                              30);
               mFirstFileName->SetName(_("First file name"));
            }
            S.EndMultiColumn();
         }
         S.EndMultiColumn();

         S.SetBorder(3);
      }
      S.EndStatic();

      S.SetBorder(5);
      S.StartStatic(_("Name files:"), 1);
      {
         S.SetBorder(2);
         S.StartRadioButtonGroup(wxT("/Export/TrackNameWithOrWithoutNumbers"), wxT("labelTrack"));
         {
            mByName = S.Id(ByNameID)
               .TieRadioButton(_("Using Label/Track Name"), wxT("labelTrack"));

            mByNumberAndName = S.Id(ByNameAndNumberID)
               .TieRadioButton(_("Numbering before Label/Track Name"), wxT("numberBefore"));

            mByNumber = S.Id(ByNumberID)
               .TieRadioButton(_("Numbering after File name prefix"), wxT("numberAfter"));
         }
         S.EndRadioButtonGroup();

         S.StartMultiColumn(3, wxEXPAND);
         S.SetStretchyCol(2);
         {
            // Row 3 (indented)
            S.AddVariableText(wxT("   "), false);
            mPrefixLabel = S.AddVariableText(_("File name prefix:"), false);
            mPrefix = S.Id(PrefixID)
               .TieTextBox(wxT(""),
                           name,
                           30);
            mPrefix->SetName(_("File name prefix"));
         }
         S.EndMultiColumn();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, false);
   {
      mOverwrite = S.Id(OverwriteID).TieCheckBox(_("Overwrite existing files"),
                                                 wxT("/Export/OverwriteExisting"),
                                                 false);
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();
   mExport = (wxButton *)wxWindow::FindWindowById(wxID_OK, this);
   mExport->SetLabel(_("Export"));

}

void ExportMultiple::EnableControls()
{
   bool enable;

   if (!mInitialized) {
      return;
   }

   mFirst->Enable(mLabel->GetValue());

   enable =  mLabel->GetValue() &&
            (mByName->GetValue() || mByNumberAndName->GetValue()) &&
             mFirst->GetValue();
   mFirstFileLabel->Enable(enable);
   mFirstFileName->Enable(enable);

   enable = mByNumber->GetValue();
   mPrefixLabel->Enable(enable);
   mPrefix->Enable(enable);

   bool ok = true;

   if (mLabel->GetValue() && mFirst->GetValue() &&
       mFirstFileName->GetValue() == wxT("") &&
       mPrefix->GetValue() == wxT(""))
      ok = false;

   if (mByNumber->GetValue() &&
       mPrefix->GetValue() == wxT(""))
      ok = false;

   mExport->Enable(ok);
}

void ExportMultiple::OnFormat(wxCommandEvent& WXUNUSED(event))
{
   mBook->ChangeSelection(mFormat->GetSelection());

   EnableControls();
}

void ExportMultiple::OnOptions(wxCommandEvent& WXUNUSED(event))
{
   const int sel = mFormat->GetSelection();
   if (sel != wxNOT_FOUND)
   {
     size_t c = 0;
     int i = -1;
     for (const auto &pPlugin : mPlugins)
     {
       ++i;
       for (int j = 0; j < pPlugin->GetFormatCount(); j++)
       {
         if ((size_t)sel == c)
         {
            mPluginIndex = i;
            mSubFormatIndex = j;
         }
         c++;
       }
     }
   }
   mPlugins[mPluginIndex]->DisplayOptions(this,mSubFormatIndex);
}

void ExportMultiple::OnCreate(wxCommandEvent& WXUNUSED(event))
{
   wxFileName fn;

   fn.AssignDir(mDir->GetValue());

   bool ok = fn.Mkdir(0777, wxPATH_MKDIR_FULL);

   if (!ok) {
      // Mkdir will produce an error dialog
      return;
   }

   ::wxMessageBox(wxString::Format(_("\"%s\" successfully created."),
                                   fn.GetPath().c_str()),
                  _("Export Multiple"),
                  wxOK | wxCENTRE, this);
}

void ExportMultiple::OnChoose(wxCommandEvent& WXUNUSED(event))
{
   wxDirDialog dlog(this,
                    _("Choose a location to save the exported files"),
                    mDir->GetValue());
   dlog.ShowModal();
   if (dlog.GetPath() != wxT(""))
      mDir->SetValue(dlog.GetPath());
}

void ExportMultiple::OnLabel(wxCommandEvent& WXUNUSED(event))
{
   EnableControls();
}

void ExportMultiple::OnFirst(wxCommandEvent& WXUNUSED(event))
{
   EnableControls();
}

void ExportMultiple::OnFirstFileName(wxCommandEvent& WXUNUSED(event))
{
   EnableControls();
}

void ExportMultiple::OnTrack(wxCommandEvent& WXUNUSED(event))
{
   EnableControls();
}

void ExportMultiple::OnByName(wxCommandEvent& WXUNUSED(event))
{
   EnableControls();
}

void ExportMultiple::OnByNumber(wxCommandEvent& WXUNUSED(event))
{
   EnableControls();
}

void ExportMultiple::OnPrefix(wxCommandEvent& WXUNUSED(event))
{
   EnableControls();
}

void ExportMultiple::OnCancel(wxCommandEvent& WXUNUSED(event))
{
   EndModal(0);
}

void ExportMultiple::OnExport(wxCommandEvent& WXUNUSED(event))
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   gPrefs->Flush();

   // Make sure the output directory is in good shape
   if (!DirOk()) {
      return;
   }

   mFilterIndex = mFormat->GetSelection();
   if (mFilterIndex != wxNOT_FOUND)
   {
      size_t c = 0;
      int i = -1;
      for (const auto &pPlugin : mPlugins)
      {
         ++i;
         for (int j = 0; j < pPlugin->GetFormatCount(); j++, c++)
         {
            if ((size_t)mFilterIndex == c)
            {  // this is the selected format. Store the plug-in and sub-format
               // needed to acheive it.
               mPluginIndex = i;
               mSubFormatIndex = j;
               mBook->GetPage(mFilterIndex)->TransferDataFromWindow();
            }
         }
      }
   }

//   bool overwrite = mOverwrite->GetValue();
   int ok;
   mExported.Empty();

   if (mLabel->GetValue()) {
      ok = ExportMultipleByLabel(mByName->GetValue() || mByNumberAndName->GetValue(),
                                 mPrefix->GetValue(),
                                 mByNumberAndName->GetValue());
   }
   else {
      ok = ExportMultipleByTrack(mByName->GetValue() || mByNumberAndName->GetValue(),
                                 mPrefix->GetValue(),
                                 mByNumberAndName->GetValue());
   }

   // Give 'em the result
   {
      wxString msg;
      msg.Printf(
         ok == eProgressSuccess ? _("Successfully exported the following %lld file(s).")
           : (ok == eProgressFailed ? _("Something went wrong after exporting the following %lld file(s).")
             : (ok == eProgressCancelled ? _("Export canceled after exporting the following %lld file(s).")
               : (ok == eProgressStopped ? _("Export stopped after exporting the following %lld file(s).")
                 : _("Something went really wrong after exporting the following %lld file(s).")
                 )
               )
             ), (long long) mExported.GetCount());

      wxString FileList;
      for (size_t i = 0; i < mExported.GetCount(); i++) {
         FileList += mExported[i];
         FileList += '\n';
      }
      // This results dialog is a child of this dialog.
	  HelpSystem::ShowInfoDialog( this,
         _("Export Multiple"),
         msg,
         FileList,
         450,400);
   }

   if (ok == eProgressSuccess || ok == eProgressStopped) {
      EndModal(1);
   }
}

bool ExportMultiple::DirOk()
{
   wxFileName fn;

   fn.AssignDir(mDir->GetValue());

   if (fn.DirExists()) {
      return true;
   }

   wxString prompt;

   prompt.Printf(_("\"%s\" doesn't exist.\n\nWould you like to create it?"),
                 fn.GetFullPath().c_str());

   int action = wxMessageBox(prompt,
                             wxT("Warning"),
                             wxYES_NO | wxICON_EXCLAMATION);
   if (action != wxYES) {
      return false;
   }

   return fn.Mkdir(0777, wxPATH_MKDIR_FULL);
}

// TODO: JKC July2016: Merge labels/tracks duplicated export code.
int ExportMultiple::ExportMultipleByLabel(bool byName,
   const wxString &prefix, bool addNumber)
{
   wxASSERT(mProject);
   bool tagsPrompt = mProject->GetShowId3Dialog();
   int numFiles = mNumLabels;
   int l = 0;        // counter for files done
   ExportKitArray exportSettings; // dynamic array for settings.
   exportSettings.Alloc(numFiles); // Allocate some guessed space to use.

   // Account for exporting before first label
   if( mFirst->GetValue() ) {
      l--;
      numFiles++;
   }

   // Figure out how many channels we should export.
   auto channels = mTracks->GetNumExportChannels(false);

   wxArrayString otherNames;  // keep track of file names we will use, so we
   // don't duplicate them
   ExportKit setting;   // the current batch of settings
   setting.destfile.SetPath(mDir->GetValue());
   setting.destfile.SetExt(mPlugins[mPluginIndex]->GetExtension(mSubFormatIndex));
   wxLogDebug(wxT("Plug-in index = %d, Sub-format = %d"), mPluginIndex, mSubFormatIndex);
   wxLogDebug(wxT("File extension is %s"), setting.destfile.GetExt().c_str());
   wxString name;    // used to hold file name whilst we mess with it
   wxString title;   // un-messed-with title of file for tagging with

   const LabelStruct *info = NULL;
   /* Examine all labels a first time, sort out all data but don't do any
    * exporting yet (so this run is quick but interactive) */
   while( l < mNumLabels ) {

      // Get file name and starting time
      if( l < 0 ) {
         // create wxFileName for output file
         name = (mFirstFileName->GetValue());
         setting.t0 = 0.0;
      } else {
         info = mLabels->GetLabel(l);
         name = (info->title);
         setting.t0 = info->selectedRegion.t0();
      }

      // Figure out the ending time
      if( info && !info->selectedRegion.isPoint() ) {
         setting.t1 = info->selectedRegion.t1();
      } else if( l < mNumLabels-1 ) {
         // Use start of next label as end
         const LabelStruct *info1 = mLabels->GetLabel(l+1);
         setting.t1 = info1->selectedRegion.t0();
      } else {
         setting.t1 = mTracks->GetEndTime();
      }

      if( name.IsEmpty() )
         name = _("untitled");

      // store title of label to use in tags
      title = name;

      // Numbering files...
      if( !byName ) {
         name.Printf(wxT("%s-%02d"), prefix.c_str(), l+1);
      } else if( addNumber ) {
         // Following discussion with GA, always have 2 digits
         // for easy file-name sorting (on Windows)
         name.Prepend(wxString::Format(wxT("%02d-"), l+1));
      }

      // store sanitised and user checked name in object
      setting.destfile.SetName(MakeFileName(name));
      if( setting.destfile.GetName().IsEmpty() )
      {  // user cancelled dialogue, or deleted everything in field.
         // or maybe the label was empty??
         // So we ignore this one and keep going.
      } 
      else
      {
         // FIXME: TRAP_ERR User could have given an illegal filename prefix.
         // in that case we should tell them, not fail silently.
         wxASSERT(setting.destfile.IsOk());     // burp if file name is broke

         // Make sure the (final) file name is unique within the set of exports
         FileNames::MakeNameUnique(otherNames, setting.destfile);

         /* do the metadata for this file */
         // copy project metadata to start with
         setting.filetags = *(mProject->GetTags());
         // over-ride with values
         setting.filetags.SetTag(TAG_TITLE, title);
         setting.filetags.SetTag(TAG_TRACK, l+1);
         // let the user have a crack at editing it, exit if cancelled
         if( !setting.filetags.ShowEditDialog(mProject, _("Edit Metadata Tags"), tagsPrompt) )
            return false;
      }

      /* add the settings to the array of settings to be used for export */
      exportSettings.Add(setting);

      l++;  // next label, count up one
   }

   int ok = eProgressSuccess;   // did it work?
   int count = 0; // count the number of sucessful runs
   ExportKit activeSetting;  // pointer to the settings in use for this export
   /* Go round again and do the exporting (so this run is slow but
    * non-interactive) */
   for (count = 0; count < numFiles; count++) {
      /* get the settings to use for the export from the array */
      activeSetting = exportSettings[count];
      // Bug 1440 fix.
      if( activeSetting.destfile.GetName().IsEmpty() )
         continue;

      // Export it
      ok = DoExport(channels, activeSetting.destfile, false, activeSetting.t0, activeSetting.t1, activeSetting.filetags);
      if (ok != eProgressSuccess && ok != eProgressStopped) {
         break;
      }
   }

   return ok;
}

int ExportMultiple::ExportMultipleByTrack(bool byName,
   const wxString &prefix, bool addNumber)
{
   wxASSERT(mProject);
   bool tagsPrompt = mProject->GetShowId3Dialog();
   Track *tr, *tr2;
   int l = 0;     // track counter
   int numTracks = 0;
   int ok = eProgressSuccess;
   wxArrayString otherNames;
   std::vector<Track*> selected; /**< Array of pointers to the tracks which were
                                selected when we started */
   ExportKitArray exportSettings; // dynamic array we will use to store the
                                  // settings needed to do the exports with in
   exportSettings.Alloc(mNumWaveTracks);   // Allocate some guessed space to use.
   ExportKit setting;   // the current batch of settings
   setting.destfile.SetPath(mDir->GetValue());
   setting.destfile.SetExt(mPlugins[mPluginIndex]->GetExtension(mSubFormatIndex));

   wxString name;    // used to hold file name whilst we mess with it
   wxString title;   // un-messed-with title of file for tagging with

   /* Remember which tracks were selected, and set them to unselected */
   TrackListIterator iter;
   for (tr = iter.First(mTracks); tr != NULL; tr = iter.Next()) {
      if (tr->GetKind() != Track::Wave) {
         continue;
      }

      if (tr->GetSelected()) {
         selected.push_back(tr);
         tr->SetSelected(false);
      }

      if (!tr->GetLinked()) {
         numTracks++;
      }
   }

   /* Examine all tracks in turn, collecting export information */
   for (tr = iter.First(mTracks); tr != NULL; tr = iter.Next()) {

      // Want only non-muted wave tracks.
      if ((tr->GetKind() != Track::Wave)  || tr->GetMute())
         continue;

      // Get the times for the track
      setting.t0 = tr->GetStartTime();
      setting.t1 = tr->GetEndTime();

      // Check for a linked track
      tr2 = NULL;
      if (tr->GetLinked()) {
         tr2 = iter.Next();
         if (tr2) {

            // Make sure it gets included
            if (tr2->GetStartTime() < setting.t0) {
               setting.t0 = tr2->GetStartTime();
            }

            if (tr2->GetEndTime() > setting.t1) {
               setting.t1 = tr2->GetEndTime();
            }
         }
      }

      // number of export channels?
      // Needs to be per track.
      if (tr2 == NULL && tr->GetChannel() == WaveTrack::MonoChannel &&
                 ((WaveTrack *)tr)->GetPan() == 0.0)
         setting.channels = 1;
      else
         setting.channels = 2;

      // Get name and title
      title = tr->GetName();
      if( title.IsEmpty() )
         title = _("untitled");

      if (byName) {
         name = title;
         if (addNumber) {
            name.Prepend(
               wxString::Format(wxT("%02d-"), l+1));
         }
      }
      else {
         name = (wxString::Format(wxT("%s-%02d"), prefix.c_str(), l+1));
      }

      // store sanitised and user checked name in object
      setting.destfile.SetName(MakeFileName(name));

      if (setting.destfile.GetName().IsEmpty())
      {  // user cancelled dialogue, or deleted everything in field.
         // So we ignore this one and keep going.
      }
      else 
      {

         // FIXME: TRAP_ERR User could have given an illegal track name.
         // in that case we should tell them, not fail silently.
         wxASSERT(setting.destfile.IsOk());     // burp if file name is broke

         // Make sure the (final) file name is unique within the set of exports
         FileNames::MakeNameUnique(otherNames, setting.destfile);

         /* do the metadata for this file */
         // copy project metadata to start with
         setting.filetags = *(mProject->GetTags());
         // over-ride with values
         setting.filetags.SetTag(TAG_TITLE, title);
         setting.filetags.SetTag(TAG_TRACK, l+1);
         // let the user have a crack at editing it, exit if cancelled
         if (!setting.filetags.ShowEditDialog(mProject,_("Edit Metadata Tags"), tagsPrompt))
            return false;
      }
      /* add the settings to the array of settings to be used for export */
      exportSettings.Add(setting);

      l++;  // next track, count up one
   }
   // end of user-interactive data gathering loop, start of export processing
   // loop
   int count = 0; // count the number of sucessful runs
   ExportKit activeSetting;  // pointer to the settings in use for this export
   for (tr = iter.First(mTracks); tr != NULL; tr = iter.Next()) {

      // Want only non-muted wave tracks.
      if ((tr->GetKind() != Track::Wave) || (tr->GetMute() == true)) {
         continue;
      }

      // Bug 1510 possibly increment iter, before deciding whether to export.
      // Check for a linked track
      tr2 = NULL;
      if (tr->GetLinked()) {
         tr2 = iter.Next();
      }

      wxLogDebug( "Get setting %i", count );
      /* get the settings to use for the export from the array */
      activeSetting = exportSettings[count];
      if( activeSetting.destfile.GetName().IsEmpty() ){
         count++;
         continue;
      }

      /* Select the track */
      tr->SetSelected(true);
      if (tr2) {
         // Select it also
         tr2->SetSelected(true);
      }

      // Export the data. "channels" are per track.
      ok = DoExport(activeSetting.channels, activeSetting.destfile, true, activeSetting.t0, activeSetting.t1, activeSetting.filetags);

      // Reset selection state
      tr->SetSelected(false);
      if (tr2) {
         tr2->SetSelected(false);
      }

      // Stop if an error occurred
      if (ok != eProgressSuccess && ok != eProgressStopped) {
         break;
      }
      // increment export counter
      count++;

   }

   // Restore the selection states
   for (auto pTrack : mSelected)
      pTrack->SetSelected(true);

   return ok ;
}

int ExportMultiple::DoExport(unsigned channels,
                              const wxFileName &inName,
                              bool selectedOnly,
                              double t0,
                              double t1,
                              const Tags &tags)
{
   wxFileName name;

   wxLogDebug(wxT("Doing multiple Export: File name \"%s\""), (inName.GetFullName()).c_str());
   wxLogDebug(wxT("Channels: %i, Start: %lf, End: %lf "), channels, t0, t1);
   if (selectedOnly) wxLogDebug(wxT("Selected Region Only"));
   else wxLogDebug(wxT("Whole Project"));

   if (mOverwrite->GetValue()) {
      // Make sure we don't overwrite (corrupt) alias files
      if (!mProject->GetDirManager()->EnsureSafeFilename(inName)) {
         return false;
      }
      name = inName;
   }
   else {
      name = inName;
      int i = 2;
      wxString base(name.GetName());
      while (name.FileExists()) {
         name.SetName(wxString::Format(wxT("%s-%d"), base.c_str(), i++));
      }
   }

   // Call the format export routine
   const wxString fullPath{name.GetFullPath()};
   int success = mPlugins[mPluginIndex]->Export(mProject,
                                                channels,
                                                fullPath,
                                                selectedOnly,
                                                t0,
                                                t1,
                                                NULL,
                                                &tags,
                                                mSubFormatIndex);

   if (success == eProgressSuccess || success == eProgressStopped) {
      mExported.Add(fullPath);
   }

   Refresh();
   Update();

   return success;
}

wxString ExportMultiple::MakeFileName(const wxString &input)
{
   wxString newname = input; // name we are generating

   // strip out anything that isn't allowed in file names on this platform
   auto changed = Internat::SanitiseFilename(newname, wxT("_"));

   if(changed)
   {  // need to get user to fix file name
      // build the dialog
      wxString msg;
      wxString excluded = ::wxJoin( Internat::GetExcludedCharacters(), wxChar(' ') );
      // TODO: For Russian langauge we should have separate cases for 2 and more than 2 letters.
      if( excluded.Length() > 1 ){
         // i18ln-hint: The second %s gives some letters that can't be used.
         msg.Printf(_("Label or track \"%s\" is not a legal file name. You cannot use any of: %s\nUse..."), input.c_str(),
            excluded.c_str());
      } else {
         // i18ln-hint: The second %s gives a letter that can't be used.
         msg.Printf(_("Label or track \"%s\" is not a legal file name. You cannot use \"%s\".\nUse..."), input.c_str(),
            excluded.c_str());
      }

      wxTextEntryDialog dlg( this, msg, _("Save As..."), newname );


      // And tell the validator about excluded chars
      dlg.SetTextValidator( wxFILTER_EXCLUDE_CHAR_LIST );
      wxTextValidator *tv = dlg.GetTextValidator();
      tv->SetExcludes(Internat::GetExcludedCharacters());

      // Show the dialog and bail if the user cancels
      if( dlg.ShowModal() == wxID_CANCEL )
      {
          return wxEmptyString;
      }
      // Extract the name from the dialog
      newname = dlg.GetValue();
   }  // phew - end of file name sanitisation procedure
   return newname;
}

void SuccessDialog::OnKeyDown(wxListEvent& event)
{
   if (event.GetKeyCode() == WXK_RETURN)
      EndModal(1);
   else
      event.Skip(); // allow standard behaviour
}

void SuccessDialog::OnItemActivated(wxListEvent& WXUNUSED(event))
{
   EndModal(1);
}

void MouseEvtHandler::OnMouse(wxMouseEvent& event)
{
   event.Skip(false);
}
