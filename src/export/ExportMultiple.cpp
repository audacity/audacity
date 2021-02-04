/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMultiple.cpp

  Dominic Mazzoni

*******************************************************************//**

\class ExportMultipleDialog
\brief Presents a dialog box allowing the user to export multiple files
  either by exporting each track as a separate file, or by
  exporting each label as a separate file.

*//********************************************************************/


#include "ExportMultiple.h"

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/dirdlg.h>
#include <wx/event.h>
#include <wx/listbase.h>
#include <wx/filefn.h>
#include <wx/filename.h>
#include <wx/intl.h>
#include <wx/log.h>
#include <wx/radiobut.h>
#include <wx/simplebook.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/textdlg.h>

#include "FileNames.h"
#include "LabelTrack.h"
#include "Project.h"
#include "ProjectSettings.h"
#include "ProjectWindow.h"
#include "ProjectWindows.h"
#include "Prefs.h"
#include "../SelectionState.h"
#include "../ShuttleGui.h"
#include "../Tags.h"
#include "../WaveTrack.h"
#include "../widgets/HelpSystem.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/AudacityTextEntryDialog.h"
#include "../widgets/ProgressDialog.h"


namespace {
/** \brief A private class used to store the information needed to do an
    * export.
    *
    * We create a set of these during the interactive phase of the export
    * cycle, then use them when the actual exports are done. */
   class ExportKit
   {
   public:
      Tags filetags; /**< The set of metadata to use for the export */
      wxFileNameWrapper destfile; /**< The file to export to */
      double t0;           /**< Start time for the export */
      double t1;           /**< End time for the export */
      unsigned channels;   /**< Number of channels for ExportMultipleByTrack */
   };  // end of ExportKit declaration
   /* we are going to want an set of these kits, and don't know how many until
    * runtime. I would dearly like to use a std::vector, but it seems that
    * this isn't done anywhere else in Audacity, presumably for a reason?, so
    * I'm stuck with wxArrays, which are much harder, as well as non-standard.
    */
}

/* define our dynamic array of export settings */

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
// ExportMultipleDialog methods
//

BEGIN_EVENT_TABLE(ExportMultipleDialog, wxDialogWrapper)
   EVT_CHOICE(FormatID, ExportMultipleDialog::OnFormat)
//   EVT_BUTTON(OptionsID, ExportMultipleDialog::OnOptions)
   EVT_BUTTON(CreateID, ExportMultipleDialog::OnCreate)
   EVT_BUTTON(ChooseID, ExportMultipleDialog::OnChoose)
   EVT_BUTTON(wxID_OK, ExportMultipleDialog::OnExport)
   EVT_BUTTON(wxID_CANCEL, ExportMultipleDialog::OnCancel)
   EVT_BUTTON(wxID_HELP, ExportMultipleDialog::OnHelp)
   EVT_RADIOBUTTON(LabelID, ExportMultipleDialog::OnLabel)
   EVT_RADIOBUTTON(TrackID, ExportMultipleDialog::OnTrack)
   EVT_RADIOBUTTON(ByNameAndNumberID, ExportMultipleDialog::OnByName)
   EVT_RADIOBUTTON(ByNameID, ExportMultipleDialog::OnByName)
   EVT_RADIOBUTTON(ByNumberID, ExportMultipleDialog::OnByNumber)
   EVT_CHECKBOX(FirstID, ExportMultipleDialog::OnFirst)
   EVT_TEXT(FirstFileNameID, ExportMultipleDialog::OnFirstFileName)
   EVT_TEXT(PrefixID, ExportMultipleDialog::OnPrefix)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(SuccessDialog, wxDialogWrapper)
   EVT_LIST_KEY_DOWN(wxID_ANY, SuccessDialog::OnKeyDown)
   EVT_LIST_ITEM_ACTIVATED(wxID_ANY, SuccessDialog::OnItemActivated) // happens when <enter> is pressed with list item having focus
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(MouseEvtHandler, wxEvtHandler)
   EVT_LEFT_DCLICK(MouseEvtHandler::OnMouse)
END_EVENT_TABLE()

ExportMultipleDialog::ExportMultipleDialog(AudacityProject *project)
: wxDialogWrapper( &GetProjectFrame( *project ),
   wxID_ANY, XO("Export Multiple") )
, mExporter{ *project }
, mSelectionState{ SelectionState::Get( *project ) }
{
   SetName();

   mProject = project;
   mTracks = &TrackList::Get( *project );
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

ExportMultipleDialog::~ExportMultipleDialog()
{
}

void ExportMultipleDialog::CountTracksAndLabels()
{
   bool anySolo = !(( mTracks->Any<const WaveTrack>() + &WaveTrack::GetSolo ).empty());

   mNumWaveTracks =
      (mTracks->Leaders< const WaveTrack >() - 
      (anySolo ? &WaveTrack::GetNotSolo : &WaveTrack::GetMute)).size();

   // only the first label track
   mLabels = *mTracks->Any< const LabelTrack >().begin();
   mNumLabels = mLabels ? mLabels->GetNumLabels() : 0;
}

int ExportMultipleDialog::ShowModal()
{
   // Cannot export if all audio tracks are muted.
   if (mNumWaveTracks == 0)
   {
      ::AudacityMessageBox(
         XO("All audio is muted."),
         XO("Cannot Export Multiple"),
         wxOK | wxCENTRE,
         this);
      return wxID_CANCEL;
   }

   if ((mNumWaveTracks < 1) && (mNumLabels < 1))
   {
      ::AudacityMessageBox(
         XO(
"You have no unmuted Audio Tracks and no applicable \
\nlabels, so you cannot export to separate audio files."),
         XO("Cannot Export Multiple"),
         wxOK | wxCENTRE,
         this);
      return wxID_CANCEL;
   }

   bool bHasLabels = (mNumLabels > 0);
   bool bHasTracks = (mNumWaveTracks > 0);
   
   mLabel->Enable(bHasLabels && bHasTracks);
   mTrack->Enable(bHasTracks);

   // If you have 2 or more tracks, then it is export by tracks.
   // If you have no labels, then it is export by tracks.
   // Otherwise it is export by labels, by default.
   bool bPreferByLabels = bHasLabels && (mNumWaveTracks < 2);
   mLabel->SetValue(bPreferByLabels);
   mTrack->SetValue(!bPreferByLabels);

   EnableControls();

   return wxDialogWrapper::ShowModal();
}

void ExportMultipleDialog::PopulateOrExchange(ShuttleGui& S)
{
   wxString name = mProject->GetProjectName();
   wxString defaultFormat = gPrefs->Read(wxT("/Export/Format"), wxT("WAV"));

   TranslatableStrings visibleFormats;
   wxArrayStringEx formats;
   mPluginIndex = -1;
   mFilterIndex = 0;

   {
      int i = -1;
      for (const auto &pPlugin : mPlugins)
      {
         ++i;
         for (int j = 0; j < pPlugin->GetFormatCount(); j++)
         {
            auto format = mPlugins[i]->GetDescription(j);
            visibleFormats.push_back( format );
            // use MSGID of description as a value too, written into config file
            // This is questionable.  A change in the msgid can make the
            // preference stored in old config files inapplicable
            formats.push_back( format.MSGID().GET() );
            if (mPlugins[i]->GetFormat(j) == defaultFormat) {
               mPluginIndex = i;
               mSubFormatIndex = j;
            }
            if (mPluginIndex == -1) mFilterIndex++;
         }
      }
   }


   // Bug 1304: Set the default file path.  It's used if none stored in config.
   auto DefaultPath = FileNames::FindDefaultPath(FileNames::Operation::Export);

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
      S.StartStatic(XO("Export files to:"), true);
      {
         S.StartMultiColumn(4, true);
         {
            mDir = S.Id(DirID)
               .AddTextBox(XXO("Folder:"),
                           DefaultPath,
                           64);
            S.Id(ChooseID).AddButton(XXO("Choose..."));
            S.Id(CreateID).AddButton(XXO("Create"));

            mFormat = S.Id(FormatID)
               .TieChoice( XXO("Format:"),
               {
                  wxT("/Export/MultipleFormat"),
                  {
                     ByColumns,
                     visibleFormats,
                     formats
                  },
                  mFilterIndex
               }
            );
            S.AddVariableText( {}, false);
            S.AddVariableText( {}, false);

            S.AddPrompt(XXO("Options:"));

            mBook = S.Id(OptionsID)
               .Style(wxBORDER_STATIC)
               .StartSimplebook();
            if (S.GetMode() == eIsCreating)
            {
               for (const auto &pPlugin : mPlugins)
               {
                  for (int j = 0; j < pPlugin->GetFormatCount(); j++)
                  {
                     // Name of simple book page is not displayed
                     S.StartNotebookPage( {} );
                     pPlugin->OptionsCreate(S, j);
                     S.EndNotebookPage();
                  }
               }
               mBook->ChangeSelection(mFormat->GetSelection());
            }
            S.EndSimplebook();
            S.AddVariableText( {}, false);
            S.AddVariableText( {}, false);
         }
         S.EndMultiColumn();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.StartHorizontalLay(wxEXPAND, false);
   {
      S.SetBorder(5);
      S.StartStatic(XO("Split files based on:"), 1);
      {
         // Row 1
         S.SetBorder(1);

         // Bug 2692: Place button group in panel so tabbing will work and,
         // on the Mac, VoiceOver will announce as radio buttons.
         S.StartPanel();
         {
            mTrack = S.Id(TrackID)
               .AddRadioButton(XXO("Tracks"));

            // Row 2
            S.SetBorder(1);
            mLabel = S.Id(LabelID)
               .AddRadioButtonToGroup(XXO("Labels"));
         }
         S.EndPanel();

         S.SetBorder(3);
         S.StartMultiColumn(2, wxEXPAND);
         S.SetStretchyCol(1);
         {
            // Row 3 (indented)
            S.AddVariableText(Verbatim("   "), false);
            mFirst = S.Id(FirstID)
               .AddCheckBox(XXO("Include audio before first label"), false);

            // Row 4
            S.AddVariableText( {}, false);
            S.StartMultiColumn(2, wxEXPAND);
            S.SetStretchyCol(1);
            {
               mFirstFileLabel =
                  S.AddVariableText(XO("First file name:"), false);
               mFirstFileName = S.Id(FirstFileNameID)
                  .Prop(1)
                  .Name(XO("First file name"))
                  .TieTextBox( {},
                              name,
                              30);
            }
            S.EndMultiColumn();
         }
         S.EndMultiColumn();

         S.SetBorder(3);
      }
      S.EndStatic();

      S.SetBorder(5);
      S.StartStatic(XO("Name files:"), 1);
      {
         S.SetBorder(2);

         // Bug 2692: Place button group in panel so tabbing will work and,
         // on the Mac, VoiceOver will announce as radio buttons.
         S.StartPanel();
         {
            S.StartRadioButtonGroup({
               wxT("/Export/TrackNameWithOrWithoutNumbers"),
               {
                  { wxT("labelTrack"), XXO("Using Label/Track Name") },
                  { wxT("numberBefore"), XXO("Numbering before Label/Track Name") },
                  { wxT("numberAfter"), XXO("Numbering after File name prefix") },
               },
               0 // labelTrack
            });
            {
               mByName = S.Id(ByNameID).TieRadioButton();

               mByNumberAndName = S.Id(ByNameAndNumberID).TieRadioButton();

               mByNumber = S.Id(ByNumberID).TieRadioButton();
            }
            S.EndRadioButtonGroup();
         }
         S.EndPanel();

         S.StartMultiColumn(3, wxEXPAND);
         S.SetStretchyCol(2);
         {
            // Row 3 (indented)
            S.AddVariableText(Verbatim("   "), false);
            mPrefixLabel = S.AddVariableText(XO("File name prefix:"), false);
            mPrefix = S.Id(PrefixID)
               .Name(XO("File name prefix"))
               .TieTextBox( {},
                           name,
                           30);
         }
         S.EndMultiColumn();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, false);
   {
      mOverwrite = S.Id(OverwriteID).TieCheckBox(XXO("Overwrite existing files"),
                                                 {wxT("/Export/OverwriteExisting"),
                                                  false});
   }
   S.EndHorizontalLay();

   S.AddStandardButtons(eOkButton | eCancelButton | eHelpButton);
   mExport = (wxButton *)wxWindow::FindWindowById(wxID_OK, this);
   mExport->SetLabel(_("Export"));

}

void ExportMultipleDialog::EnableControls()
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
       mFirstFileName->GetValue().empty() &&
       mPrefix->GetValue().empty())
      ok = false;

   if (mByNumber->GetValue() &&
       mPrefix->GetValue().empty())
      ok = false;

   mExport->Enable(ok);
}

void ExportMultipleDialog::OnFormat(wxCommandEvent& WXUNUSED(event))
{
   mBook->ChangeSelection(mFormat->GetSelection());

   EnableControls();
}

void ExportMultipleDialog::OnOptions(wxCommandEvent& WXUNUSED(event))
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

void ExportMultipleDialog::OnCreate(wxCommandEvent& WXUNUSED(event))
{
   wxFileName fn;

   fn.AssignDir(mDir->GetValue());

   bool ok = fn.Mkdir(0777, wxPATH_MKDIR_FULL);

   if (!ok) {
      // Mkdir will produce an error dialog
      return;
   }

   ::AudacityMessageBox(
      XO("\"%s\" successfully created.").Format( fn.GetPath() ),
      XO("Export Multiple"),
      wxOK | wxCENTRE,
      this);
}

void ExportMultipleDialog::OnChoose(wxCommandEvent& WXUNUSED(event))
{
   wxDirDialogWrapper dlog(this,
      XO("Choose a location to save the exported files"),
      mDir->GetValue());
   dlog.ShowModal();
   if (!dlog.GetPath().empty())
      mDir->SetValue(dlog.GetPath());
}

void ExportMultipleDialog::OnLabel(wxCommandEvent& WXUNUSED(event))
{
   EnableControls();
}

void ExportMultipleDialog::OnFirst(wxCommandEvent& WXUNUSED(event))
{
   EnableControls();
}

void ExportMultipleDialog::OnFirstFileName(wxCommandEvent& WXUNUSED(event))
{
   EnableControls();
}

void ExportMultipleDialog::OnTrack(wxCommandEvent& WXUNUSED(event))
{
   EnableControls();
}

void ExportMultipleDialog::OnByName(wxCommandEvent& WXUNUSED(event))
{
   EnableControls();
}

void ExportMultipleDialog::OnByNumber(wxCommandEvent& WXUNUSED(event))
{
   EnableControls();
}

void ExportMultipleDialog::OnPrefix(wxCommandEvent& WXUNUSED(event))
{
   EnableControls();
}

void ExportMultipleDialog::OnCancel(wxCommandEvent& WXUNUSED(event))
{
   EndModal(0);
}

void ExportMultipleDialog::OnHelp(wxCommandEvent& WXUNUSED(event))
{
   HelpSystem::ShowHelp(this, L"Export_Multiple", true);
}

void ExportMultipleDialog::OnExport(wxCommandEvent& WXUNUSED(event))
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   gPrefs->Flush();

   FileNames::UpdateDefaultPath(FileNames::Operation::Export, mDir->GetValue());

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
               // needed to achieve it.
               mPluginIndex = i;
               mSubFormatIndex = j;
               mBook->GetPage(mFilterIndex)->TransferDataFromWindow();
            }
         }
      }
   }

//   bool overwrite = mOverwrite->GetValue();
   ProgressResult ok = ProgressResult::Failed;
   mExported.clear();

   // Give 'em the result
   auto cleanup = finally( [&]
   {
      auto msg = (ok == ProgressResult::Success
         ? XO("Successfully exported the following %lld file(s).")
         : ok == ProgressResult::Failed
            ? XO("Something went wrong after exporting the following %lld file(s).")
            : ok == ProgressResult::Cancelled
               ? XO("Export canceled after exporting the following %lld file(s).")
               : ok == ProgressResult::Stopped
                  ? XO("Export stopped after exporting the following %lld file(s).")
                  : XO("Something went really wrong after exporting the following %lld file(s).")
         ).Format((long long) mExported.size());

      wxString FileList;
      for (size_t i = 0; i < mExported.size(); i++) {
         FileList += mExported[i];
         FileList += '\n';
      }

      // TODO: give some warning dialog first, when only some files exported
      // successfully.

      GuardedCall( [&] {
         // This results dialog is a child of this dialog.
         HelpSystem::ShowInfoDialog( this,
                                    XO("Export Multiple"),
                                    msg,
                                    FileList,
                                    450,400);
      } );
   } );

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

   if (ok == ProgressResult::Success || ok == ProgressResult::Stopped) {
      EndModal(1);
   }
}

bool ExportMultipleDialog::DirOk()
{
   wxFileName fn;

   fn.AssignDir(mDir->GetValue());

   if (fn.DirExists()) {
      return true;
   }

   auto prompt = XO("\"%s\" doesn't exist.\n\nWould you like to create it?")
      .Format( fn.GetFullPath() );

   int action = AudacityMessageBox(
      prompt,
      XO("Warning"),
      wxYES_NO | wxICON_EXCLAMATION);
   if (action != wxYES) {
      return false;
   }

   return fn.Mkdir(0777, wxPATH_MKDIR_FULL);
}

static unsigned GetNumExportChannels( const TrackList &tracks )
{
   /* counters for tracks panned different places */
   int numLeft = 0;
   int numRight = 0;
   //int numMono = 0;
   /* track iteration kit */

   bool anySolo = !(( tracks.Any<const WaveTrack>() + &WaveTrack::GetSolo ).empty());

   // Want only unmuted wave tracks.
   for (auto tr :
         tracks.Any< const WaveTrack >() - 
      (anySolo ? &WaveTrack::GetNotSolo : &WaveTrack::GetMute)
   ) {
      // Found a left channel
      if (tr->GetChannel() == Track::LeftChannel) {
         numLeft++;
      }

      // Found a right channel
      else if (tr->GetChannel() == Track::RightChannel) {
         numRight++;
      }

      // Found a mono channel, but it may be panned
      else if (tr->GetChannel() == Track::MonoChannel) {
         float pan = tr->GetPan();

         // Figure out what kind of channel it should be
         if (pan == -1.0) {   // panned hard left
            numLeft++;
         }
         else if (pan == 1.0) {  // panned hard right
            numRight++;
         }
         else if (pan == 0) { // panned dead center
            // numMono++;
         }
         else {   // panned somewhere else
            numLeft++;
            numRight++;
         }
      }
   }

   // if there is stereo content, report 2, else report 1
   if (numRight > 0 || numLeft > 0) {
      return 2;
   }

   return 1;
}

// TODO: JKC July2016: Merge labels/tracks duplicated export code.
// TODO: JKC Apr2019: Doubly so merge these!  Too much duplication.
ProgressResult ExportMultipleDialog::ExportMultipleByLabel(bool byName,
   const wxString &prefix, bool addNumber)
{
   wxASSERT(mProject);
   int numFiles = mNumLabels;
   int l = 0;        // counter for files done
   std::vector<ExportKit> exportSettings; // dynamic array for settings.
   exportSettings.reserve(numFiles); // Allocate some guessed space to use.

   // Account for exporting before first label
   if( mFirst->GetValue() ) {
      l--;
      numFiles++;
   }

   // Figure out how many channels we should export.
   auto channels = GetNumExportChannels( *mTracks );

   FilePaths otherNames;  // keep track of file names we will use, so we
   // don't duplicate them
   ExportKit setting;   // the current batch of settings
   setting.destfile.SetPath(mDir->GetValue());
   setting.destfile.SetExt(mPlugins[mPluginIndex]->GetExtension(mSubFormatIndex));
   wxLogDebug(wxT("Plug-in index = %d, Sub-format = %d"), mPluginIndex, mSubFormatIndex);
   wxLogDebug(wxT("File extension is %s"), setting.destfile.GetExt());
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

      if( name.empty() )
         name = _("untitled");

      // store title of label to use in tags
      title = name;

      // Numbering files...
      if( !byName ) {
         name.Printf(wxT("%s-%02d"), prefix, l+1);
      } else if( addNumber ) {
         // Following discussion with GA, always have 2 digits
         // for easy file-name sorting (on Windows)
         name.Prepend(wxString::Format(wxT("%02d-"), l+1));
      }

      // store sanitised and user checked name in object
      setting.destfile.SetName(MakeFileName(name));
      if( setting.destfile.GetName().empty() )
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
         setting.filetags = Tags::Get( *mProject );
         setting.filetags.LoadDefaults();
         if (exportSettings.size()) {
            setting.filetags = exportSettings.back().filetags;
         }
         // over-ride with values
         setting.filetags.SetTag(TAG_TITLE, title);
         setting.filetags.SetTag(TAG_TRACK, l+1);
         // let the user have a crack at editing it, exit if cancelled
         auto &settings = ProjectSettings::Get( *mProject );
         bool bShowTagsDialog = settings.GetShowId3Dialog();

         bShowTagsDialog = bShowTagsDialog && mPlugins[mPluginIndex]->GetCanMetaData(mSubFormatIndex);

         if( bShowTagsDialog ){
            bool bCancelled = !setting.filetags.ShowEditDialog(
               ProjectWindow::Find( mProject ),
               XO("Edit Metadata Tags"), bShowTagsDialog);
            gPrefs->Read(wxT("/AudioFiles/ShowId3Dialog"), &bShowTagsDialog, true);
            settings.SetShowId3Dialog( bShowTagsDialog );
            if( bCancelled )
               return ProgressResult::Cancelled;
         }
      }

      /* add the settings to the array of settings to be used for export */
      exportSettings.push_back(setting);

      l++;  // next label, count up one
   }

   auto ok = ProgressResult::Success;   // did it work?
   int count = 0; // count the number of successful runs
   ExportKit activeSetting;  // pointer to the settings in use for this export
   /* Go round again and do the exporting (so this run is slow but
    * non-interactive) */
   std::unique_ptr<ProgressDialog> pDialog;
   for (count = 0; count < numFiles; count++) {
      /* get the settings to use for the export from the array */
      activeSetting = exportSettings[count];
      // Bug 1440 fix.
      if( activeSetting.destfile.GetName().empty() )
         continue;

      // Export it
      ok = DoExport(pDialog, channels, activeSetting.destfile, false,
         activeSetting.t0, activeSetting.t1, activeSetting.filetags);
      if (ok == ProgressResult::Stopped) {
         AudacityMessageDialog dlgMessage(
            nullptr,
            XO("Continue to export remaining files?"),
            XO("Export"),
            wxYES_NO | wxNO_DEFAULT | wxICON_WARNING);
         if (dlgMessage.ShowModal() != wxID_YES ) {
            // User decided not to continue - bail out!
            break;
         }
      }
      else if (ok != ProgressResult::Success) {
         break;
      }
   }

   return ok;
}

ProgressResult ExportMultipleDialog::ExportMultipleByTrack(bool byName,
   const wxString &prefix, bool addNumber)
{
   wxASSERT(mProject);
   int l = 0;     // track counter
   auto ok = ProgressResult::Success;
   FilePaths otherNames;
   std::vector<ExportKit> exportSettings; // dynamic array we will use to store the
                                  // settings needed to do the exports with in
   exportSettings.reserve(mNumWaveTracks);   // Allocate some guessed space to use.
   ExportKit setting;   // the current batch of settings
   setting.destfile.SetPath(mDir->GetValue());
   setting.destfile.SetExt(mPlugins[mPluginIndex]->GetExtension(mSubFormatIndex));

   wxString name;    // used to hold file name whilst we mess with it
   wxString title;   // un-messed-with title of file for tagging with

   /* Remember which tracks were selected, and set them to deselected */
   SelectionStateChanger changer{ mSelectionState, *mTracks };
   for (auto tr : mTracks->Selected<WaveTrack>())
      tr->SetSelected(false);

   bool anySolo = !(( mTracks->Any<const WaveTrack>() + &WaveTrack::GetSolo ).empty());

   bool skipSilenceAtBeginning;
   gPrefs->Read(wxT("/AudioFiles/SkipSilenceAtBeginning"), &skipSilenceAtBeginning, false);

   /* Examine all tracks in turn, collecting export information */
   for (auto tr : mTracks->Leaders<WaveTrack>() - 
      (anySolo ? &WaveTrack::GetNotSolo : &WaveTrack::GetMute)) {

      // Get the times for the track
      auto channels = TrackList::Channels(tr);
      setting.t0 = skipSilenceAtBeginning ? channels.min(&Track::GetStartTime) : 0;
      setting.t1 = channels.max( &Track::GetEndTime );

      // number of export channels?
      setting.channels = channels.size();
      if (setting.channels == 1 &&
          !(tr->GetChannel() == WaveTrack::MonoChannel &&
                  tr->GetPan() == 0.0))
         setting.channels = 2;

      // Get name and title
      title = tr->GetName();
      if( title.empty() )
         title = _("untitled");

      if (byName) {
         name = title;
         if (addNumber) {
            name.Prepend(
               wxString::Format(wxT("%02d-"), l+1));
         }
      }
      else {
         name = (wxString::Format(wxT("%s-%02d"), prefix, l+1));
      }

      // store sanitised and user checked name in object
      setting.destfile.SetName(MakeFileName(name));

      if (setting.destfile.GetName().empty())
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
         setting.filetags = Tags::Get( *mProject );
         setting.filetags.LoadDefaults();
         if (exportSettings.size()) {
            setting.filetags = exportSettings.back().filetags;
         }
         // over-ride with values
         setting.filetags.SetTag(TAG_TITLE, title);
         setting.filetags.SetTag(TAG_TRACK, l+1);
         // let the user have a crack at editing it, exit if cancelled
         auto &settings = ProjectSettings::Get( *mProject );
         bool bShowTagsDialog = settings.GetShowId3Dialog();

         bShowTagsDialog = bShowTagsDialog && mPlugins[mPluginIndex]->GetCanMetaData(mSubFormatIndex);

         if( bShowTagsDialog ){
            bool bCancelled = !setting.filetags.ShowEditDialog(
               ProjectWindow::Find( mProject ),
               XO("Edit Metadata Tags"), bShowTagsDialog);
            gPrefs->Read(wxT("/AudioFiles/ShowId3Dialog"), &bShowTagsDialog, true);
            settings.SetShowId3Dialog( bShowTagsDialog );
            if( bCancelled )
               return ProgressResult::Cancelled;
         }
      }
      /* add the settings to the array of settings to be used for export */
      exportSettings.push_back(setting);

      l++;  // next track, count up one
   }
   // end of user-interactive data gathering loop, start of export processing
   // loop
   int count = 0; // count the number of successful runs
   ExportKit activeSetting;  // pointer to the settings in use for this export
   std::unique_ptr<ProgressDialog> pDialog;

   for (auto tr : mTracks->Leaders<WaveTrack>() - 
      (anySolo ? &WaveTrack::GetNotSolo : &WaveTrack::GetMute)) {

      wxLogDebug( "Get setting %i", count );
      /* get the settings to use for the export from the array */
      activeSetting = exportSettings[count];
      if( activeSetting.destfile.GetName().empty() ){
         count++;
         continue;
      }

      /* Select the track */
      SelectionStateChanger changer2{ mSelectionState, *mTracks };
      const auto range = TrackList::Channels(tr);
      for (auto channel : range)
         channel->SetSelected(true);

      // Export the data. "channels" are per track.
      ok = DoExport(pDialog,
         activeSetting.channels, activeSetting.destfile, true,
         activeSetting.t0, activeSetting.t1, activeSetting.filetags);
      if (ok == ProgressResult::Stopped) {
         AudacityMessageDialog dlgMessage(
            nullptr,
            XO("Continue to export remaining files?"),
            XO("Export"),
            wxYES_NO | wxNO_DEFAULT | wxICON_WARNING);
         if (dlgMessage.ShowModal() != wxID_YES ) {
            // User decided not to continue - bail out!
            break;
         }
      }
      else if (ok != ProgressResult::Success) {
         break;
      }
      // increment export counter
      count++;

   }

   return ok ;
}

ProgressResult ExportMultipleDialog::DoExport(std::unique_ptr<ProgressDialog> &pDialog,
                              unsigned channels,
                              const wxFileName &inName,
                              bool selectedOnly,
                              double t0,
                              double t1,
                              const Tags &tags)
{
   wxFileName name;

   wxLogDebug(wxT("Doing multiple Export: File name \"%s\""), (inName.GetFullName()));
   wxLogDebug(wxT("Channels: %i, Start: %lf, End: %lf "), channels, t0, t1);
   if (selectedOnly)
      wxLogDebug(wxT("Selected Region Only"));
   else
      wxLogDebug(wxT("Whole Project"));

   wxFileName backup;
   if (mOverwrite->GetValue()) {
      name = inName;
      backup.Assign(name);

      int suffix = 0;
      do {
         backup.SetName(name.GetName() +
                           wxString::Format(wxT("%d"), suffix));
         ++suffix;
      }
      while (backup.FileExists());
      ::wxRenameFile(inName.GetFullPath(), backup.GetFullPath());
   }
   else {
      name = inName;
      int i = 2;
      wxString base(name.GetName());
      while (name.FileExists()) {
         name.SetName(wxString::Format(wxT("%s-%d"), base, i++));
      }
   }

   ProgressResult success = ProgressResult::Cancelled;
   const wxString fullPath{name.GetFullPath()};

   auto cleanup = finally( [&] {
      bool ok =
         success == ProgressResult::Stopped ||
         success == ProgressResult::Success;
      if (backup.IsOk()) {
         if ( ok )
            // Remove backup
            ::wxRemoveFile(backup.GetFullPath());
         else {
            // Restore original
            ::wxRemoveFile(fullPath);
            ::wxRenameFile(backup.GetFullPath(), fullPath);
         }
      }
      else {
         if ( ! ok )
            // Remove any new, and only partially written, file.
            ::wxRemoveFile(fullPath);
      }
   } );

   // Call the format export routine
   success = mPlugins[mPluginIndex]->Export(mProject,
                                            pDialog,
                                                channels,
                                                fullPath,
                                                selectedOnly,
                                                t0,
                                                t1,
                                                NULL,
                                                &tags,
                                                mSubFormatIndex);

   if (success == ProgressResult::Success || success == ProgressResult::Stopped) {
      mExported.push_back(fullPath);
   }

   Refresh();
   Update();

   return success;
}

wxString ExportMultipleDialog::MakeFileName(const wxString &input)
{
   wxString newname = input; // name we are generating

   // strip out anything that isn't allowed in file names on this platform
   auto changed = Internat::SanitiseFilename(newname, wxT("_"));

   if(changed)
   {  // need to get user to fix file name
      // build the dialog
      TranslatableString msg;
      wxString excluded = ::wxJoin( Internat::GetExcludedCharacters(), wxT(' '), wxT('\0') );
      // TODO: For Russian language we should have separate cases for 2 and more than 2 letters.
      if( excluded.length() > 1 ){
         msg = XO(
// i18n-hint: The second %s gives some letters that can't be used.
"Label or track \"%s\" is not a legal file name.\nYou cannot use any of these characters:\n\n%s\n\nSuggested replacement:")
            .Format( input, excluded );
      } else {
         msg = XO(
// i18n-hint: The second %s gives a letter that can't be used.
"Label or track \"%s\" is not a legal file name. You cannot use \"%s\".\n\nSuggested replacement:")
            .Format( input, excluded );
      }

      AudacityTextEntryDialog dlg( this, msg, XO("Save As..."), newname );


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
