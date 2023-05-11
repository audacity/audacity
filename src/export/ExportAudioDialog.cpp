/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportAudioDialog.cpp
 
  Dominic Mazzoni

  Vitaly Sverchinsky split from ExportMultiple.cpp and ExportAudioDialog.cpp

**********************************************************************/

#include "ExportAudioDialog.h"

#include <numeric>

#include "Export.h"
#include "ExportUtils.h"
#include "ProjectSettings.h"
#include "WaveTrack.h"
#include "LabelTrack.h"
#include "Mix.h"
#include "Prefs.h"
#include "ViewInfo.h"
#include "Project.h"
#include "SelectionState.h"

#include <wx/log.h>
#include <wx/checkbox.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/radiobut.h>
#include <wx/stattext.h>
#include <wx/event.h>

#include "ShuttleGui.h"
#include "ProjectWindow.h"
#include "AudacityMessageBox.h"
#include "Theme.h"
#include "HelpSystem.h"
#include "TagsEditor.h"
#include "AudacityTextEntryDialog.h"
#include "ExportFilePanel.h"
#include "ExportProgressUI.h"

namespace
{

ChoiceSetting ExportAudioExportRange { L"/ExportAudioDialog/ExportRange",
   {
      { "project", XO("Entire Project") },
      { "selection", XO("Current selection") },
      { "split", XO("Split in parts") }
   },
   0, //project
};

ChoiceSetting ExportAudioSplitMode { L"/ExportAudioDialog/SplitMode",
   {
      { "tracks", XO("Tracks") },
      { "labels", XO("Labels") }
   },
   0
};

ChoiceSetting ExportAudioSplitNamePolicy { L"/ExportAudioDialog/SplitNamePolicy",
   {
      { "name", XO("Using Label/Track Name") },//->"label_track"
      { "num_and_name", XO("Numbering before Label/Track Name") },//->"number_before"
      { "num_and_prefix", XO("Numbering after File name prefix") }//->"number_after"
   },
   0
};

IntSetting ExportAudioSampleRate { "L/ExportAudioDialog/SampleRate", 0 };//use project rate until overwritten

BoolSetting ExportAudioIncludeAudioBeforeFirstLabel { L"/ExportAudioDialog/IncludeAudioBeforeFirstLabel", false };

BoolSetting ExportAudioOverwriteExisting { L"/ExportAudioDialog/OverwriteExisting", false };

BoolSetting ExportAudioSkipSilenceAtBeginning { L"/ExportAudioDialog/SkipSilenceAtBeginning", false };

StringSetting ExportAudioDefaultFormat{ L"/ExportAudioDialog/Format", L"WAV" };

enum {
   ExportFilePanelID = 10000,

   ExportRangeProjectID,
   ExportRangeSelectionID,
   ExportRangeSplitID,

   TrimBlankSpaceBeforeFirstClipID,

   ExportModeTracksID,
   ExportModeLabelsID,

   IncludeAudioBeforeFirstLabelID,

   ExportSplitNamePolicyTrackNameID,
   ExportSplitNamePolicyNumberingBeforeNameID,
   ExportSplitNamePolicyNumberingAfterPrefixID,

   FileNamePrefixID,

   OverwriteExistingFilesID,
   
   EditMetadataID,
};

}

BEGIN_EVENT_TABLE(ExportAudioDialog, wxDialogWrapper)
   EVT_COMMAND(ExportFilePanelID, AUDACITY_EXPORT_FORMAT_CHANGE_EVENT, ExportAudioDialog::OnFormatChange)

   EVT_RADIOBUTTON(ExportRangeProjectID, ExportAudioDialog::OnExportRangeChange)
   EVT_RADIOBUTTON(ExportRangeSelectionID, ExportAudioDialog::OnExportRangeChange)
   EVT_RADIOBUTTON(ExportRangeSplitID, ExportAudioDialog::OnExportRangeChange)

   EVT_RADIOBUTTON(ExportModeTracksID, ExportAudioDialog::OnSplitModeChange)
   EVT_RADIOBUTTON(ExportModeLabelsID, ExportAudioDialog::OnSplitModeChange)

   EVT_RADIOBUTTON(ExportSplitNamePolicyTrackNameID, ExportAudioDialog::OnSplitNamePolicyChange)
   EVT_RADIOBUTTON(ExportSplitNamePolicyNumberingBeforeNameID, ExportAudioDialog::OnSplitNamePolicyChange)
   EVT_RADIOBUTTON(ExportSplitNamePolicyNumberingAfterPrefixID, ExportAudioDialog::OnSplitNamePolicyChange)

   EVT_BUTTON(EditMetadataID, ExportAudioDialog::OnEditMetadata)
   EVT_BUTTON(wxID_HELP, ExportAudioDialog::OnHelp)

   EVT_BUTTON(wxID_OK, ExportAudioDialog::OnExport)
END_EVENT_TABLE()

ExportAudioDialog::ExportAudioDialog(wxWindow* parent,
                                     AudacityProject& project,
                                     const wxString& defaultName,
                                     const wxString& defaultFormat)
   : wxDialogWrapper(parent, wxID_ANY, XO("Export Audio"))
   , mProject(project)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   
   SetMinSize({GetBestSize().GetWidth(), -1});
   
   wxFileName filename;
   filename.SetPath(FileNames::FindDefaultPath(FileNames::Operation::Export));

   //extension will be set in `ChangeFormat`
   filename.SetEmptyExt();
   if(defaultName.empty())
      filename.SetName(_("untitled"));
   else
      filename.SetName(defaultName);

   int sampleRate{};
   ExportAudioSampleRate.Read(&sampleRate);

   wxString format = defaultFormat;
   if(format.empty())
      ExportAudioDefaultFormat.Read(&format);

   mExportOptionsPanel->Init(filename, sampleRate, {}, format);

   if(ExportUtils::FindExportWaveTracks(TrackList::Get(mProject), true).empty())
   {
      mRangeSelection->Disable();
      if(ExportAudioExportRange.Read() == "selection")
         mRangeProject->SetValue(true);
   }
   
   if(TrackList::Get(mProject).Leaders<LabelTrack>().empty())
   {
      mSplitByLabels->Disable();
      mSplitByTracks->SetValue(true);
   }
   
   mIncludeAudioBeforeFirstLabel->Enable(mSplitByLabels->GetValue());
   
   if(ExportAudioSplitNamePolicy.Read() != "num_and_prefix")
      mSplitFileNamePrefix->Disable();
   
   if(ExportAudioExportRange.Read() != "split")
      mSplitsPanel->Hide();
   
   Layout();
   Fit();
}

ExportAudioDialog::~ExportAudioDialog() = default;

void ExportAudioDialog::PopulateOrExchange(ShuttleGui& S)
{
   S.SetBorder(5);
   S.StartVerticalLay();
   {
      if(S.GetMode() == eIsCreating)
      {
         mExportOptionsPanel = safenew ExportFilePanel(mProject, false, this, ExportFilePanelID);
         S.Id(ExportFilePanelID).AddWindow(mExportOptionsPanel, wxEXPAND);
      }

      S.StartPanel();
      {
         S.SetBorder(5);
         S.StartTwoColumn();
         {
            S.StartHorizontalLay(wxSHRINK | wxALIGN_TOP);
            {
               if(auto prompt = S.AddPrompt(XO("Export Range:")))
                  prompt->SetMinSize({145, -1});
            }
            S.EndHorizontalLay();

            S.StartVerticalLay();
            {
               S.StartRadioButtonGroup(ExportAudioExportRange);
               {
                  mRangeProject = S.Id(ExportRangeProjectID).TieRadioButton();
                  mRangeSelection = S.Id(ExportRangeSelectionID).TieRadioButton();
                  mRangeSplit = S.Id(ExportRangeSplitID).TieRadioButton();
               }
               S.EndRadioButtonGroup();
            }
            S.EndVerticalLay();
         }
         S.EndTwoColumn();

         S.AddSpace(10);

         S.StartTwoColumn();
         {
            S.AddSpace(155, 1);
            mSkipSilenceAtBeginning = S
               .Id(TrimBlankSpaceBeforeFirstClipID)
               .TieCheckBox(XO("Trim blank space before first clip"), ExportAudioSkipSilenceAtBeginning);
         }
         S.EndTwoColumn();
      }
      S.EndPanel();
      
      S.SetBorder(5);
      mSplitsPanel = S.StartPanel();
      {
         S.StartMultiColumn(2);
         {
            S.StartStatic(XO("Split files based on:"));
            {
               S.StartVerticalLay();
               {
                  S.StartRadioButtonGroup(ExportAudioSplitMode);
                  {
                     mSplitByTracks = S.Id(ExportModeTracksID).TieRadioButton();
                     mSplitByLabels = S.Id(ExportModeLabelsID).TieRadioButton();
                  }
                  S.EndRadioButtonGroup();
               
                  S.StartHorizontalLay(wxALIGN_TOP);
                  {
                     S.AddSpace(10, 1);
                     mIncludeAudioBeforeFirstLabel = S
                        .Id(IncludeAudioBeforeFirstLabelID)
                        .TieCheckBox(XO("Include audio before first label"), ExportAudioIncludeAudioBeforeFirstLabel);
                  }
                  S.EndHorizontalLay();
               }
               S.EndVerticalLay();
            }
            S.EndStatic();
            
            S.StartStatic(XO("Name files:"));
            {
               S.StartVerticalLay();
               {
                  S.StartRadioButtonGroup(ExportAudioSplitNamePolicy);
                  {
                     mSplitUseName = S.Id(ExportSplitNamePolicyTrackNameID).TieRadioButton();
                     mSplitUseNumAndName = S.Id(ExportSplitNamePolicyNumberingBeforeNameID).TieRadioButton();
                     mSplitUseNumAndPrefix = S.Id(ExportSplitNamePolicyNumberingAfterPrefixID).TieRadioButton();
                  }
                  S.EndRadioButtonGroup();
                  S.StartHorizontalLay(wxALIGN_TOP);
                  {
                     S.AddSpace(10, 1);
                     mSplitFileNamePrefix = S
                        .Id(FileNamePrefixID)
                        .AddTextBox(XO("File name prefix:"), {}, 0);
                  }
                  S.EndHorizontalLay();
               }
               S.EndVerticalLay();
            }
            S.EndStatic();
         }
         S.EndMultiColumn();
         
         mOverwriteExisting = S
            .Id(OverwriteExistingFilesID)
            .TieCheckBox(XO("Overwrite existing files"), ExportAudioOverwriteExisting);
      }
      S.EndPanel();
      
      S.AddSpace(1, 10);
      
      S.SetBorder(5);
      
      S.SetBorder(5);
      S.StartHorizontalLay(wxEXPAND);
      {
         mEditMetadata = S.Id(EditMetadataID).AddButton(XO("Edit &Metadata..."), wxLEFT | wxBOTTOM);
         S.AddSpace(1, 1, wxEXPAND);
         S.Id(wxID_CANCEL).AddButton(XO("&Cancel"), wxBOTTOM);
         S.Id(wxID_OK).AddButton(XO("&Export"), wxRIGHT | wxBOTTOM, true);
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();
}


void ExportAudioDialog::OnExportRangeChange(wxCommandEvent& event)
{
   const auto enableSplits = event.GetId() == ExportRangeSplitID;
   if(mSplitsPanel->IsShown() != enableSplits)
   {
      mSplitsPanel->Show(enableSplits);
      
      Layout();
      Fit();
   }
}
void ExportAudioDialog::OnSplitModeChange(wxCommandEvent& event)
{
   mIncludeAudioBeforeFirstLabel->Enable(event.GetId() == ExportModeLabelsID);
}

void ExportAudioDialog::OnSplitNamePolicyChange(wxCommandEvent& event)
{
   mSplitFileNamePrefix->Enable(event.GetId() == ExportSplitNamePolicyNumberingAfterPrefixID);
}

void ExportAudioDialog::OnEditMetadata(wxCommandEvent &event)
{
   TagsEditorDialog::DoEditMetadata(mProject,
      XO("Edit Metadata Tags"), XO("Exported Tags"),
      true);
}

void ExportAudioDialog::OnHelp(wxCommandEvent &event)
{
   HelpSystem::ShowHelp(wxGetTopLevelParent(this), L"File_Export_Dialog", true);
}

void ExportAudioDialog::OnExport(wxCommandEvent &event)
{
   auto selectedPlugin = mExportOptionsPanel->GetPlugin();
   if(selectedPlugin == nullptr)
      return;
   auto selectedFormat = mExportOptionsPanel->GetFormat();
   auto parameters = mExportOptionsPanel->GetParameters();

   const auto path = mExportOptionsPanel->GetPath();

   if(!wxDirExists(path))
      wxMkdir(path);
   
   if(!wxDirExists(path))
   {
      AudacityMessageBox(XO("Unable to create destination folder"),
                         XO("Export Audio"),
                         wxOK | wxCENTER,
                         this);
      return;
   }
   
   auto result = ExportResult::Error;
   
   if(mRangeSplit->GetValue())
   {
      const auto byName = mSplitUseName->GetValue() || mSplitUseNumAndName->GetValue();
      const auto addNumber = mSplitUseNumAndName->GetValue();
      const auto prefix = mSplitFileNamePrefix->GetValue();
      
      FilePaths exportedFiles;
      
      if(mSplitByLabels->GetValue())
         result = DoExportSplitByLabels(*selectedPlugin, selectedFormat, parameters, byName, addNumber, prefix, exportedFiles);
      else if(mSplitByTracks->GetValue())
         result = DoExportSplitByTracks(*selectedPlugin, selectedFormat, parameters, byName, addNumber, prefix, exportedFiles);
      
      auto msg = (result == ExportResult::Success
         ? XO("Successfully exported the following %lld file(s).")
         : result == ExportResult::Error
            ? XO("Something went wrong after exporting the following %lld file(s).")
            : result == ExportResult::Cancelled
               ? XO("Export canceled after exporting the following %lld file(s).")
               : result == ExportResult::Stopped
                  ? XO("Export stopped after exporting the following %lld file(s).")
                  : XO("Something went really wrong after exporting the following %lld file(s).")
         ).Format((long long) exportedFiles.size());

      wxString fileList;
      for (auto& path : exportedFiles)
         fileList += path + '\n';

      // TODO: give some warning dialog first, when only some files exported
      // successfully.

      HelpSystem::ShowInfoDialog( this,
                                 XO("Export Audio"),
                                 msg,
                                 fileList,
                                 450,400);
   }
   else
   {
      wxFileName filename(path, mExportOptionsPanel->GetFullName());
      
      if (filename.FileExists()) {
         auto result = AudacityMessageBox(
            XO("A file named \"%s\" already exists. Replace?")
              .Format( filename.GetFullPath() ),
            XO("Export Audio"),
            wxYES_NO | wxICON_EXCLAMATION);
         if (result != wxYES) {
            return;
         }
      }
      
      ExportTaskBuilder builder;
      builder.SetFileName(filename)
         .SetPlugin(selectedPlugin, selectedFormat)
         .SetParameters(parameters)
         .SetSampleRate(mExportOptionsPanel->GetSampleRate());
      
      const auto& viewInfo = ViewInfo::Get(mProject);
      
      const auto selectedOnly = mRangeSelection->GetValue();
      
      auto t0 = selectedOnly
         ? std::max(.0, viewInfo.selectedRegion.t0())
         : .0;
      
      auto t1 = selectedOnly
         ? std::min(TrackList::Get(mProject).GetEndTime(), viewInfo.selectedRegion.t1())
         : TrackList::Get(mProject).GetEndTime();
      
      std::unique_ptr<MixerOptions::Downmix> tempMixerSpec;
      const auto channels = mExportOptionsPanel->GetChannels();
      if(channels == 0)
      {
         auto tracks = TrackList::Get(mProject).Leaders<const WaveTrack>();
         std::vector<bool> channelMask(
            std::accumulate(
               tracks.begin(),
               tracks.end(),
               0, [](int sum, const auto& track) { return sum + track->NChannels(); }),
            false);
         int trackIndex = 0;
         for(auto track : tracks)
         {
            if(track->GetSolo())
            {
               channelMask = std::vector<bool>(tracks.size(), false);
               channelMask[trackIndex] = true;
               break;
            }
            if(!track->GetMute() && (!selectedOnly || track->GetSelected()))
               channelMask[trackIndex] = true;
            ++trackIndex;
         }
         
         tempMixerSpec = std::make_unique<MixerOptions::Downmix>(*mExportOptionsPanel->GetMixerSpec(), channelMask);
         builder.SetMixerSpec(tempMixerSpec.get());
      }
      else
         builder.SetNumChannels(channels);

      auto tracks = ExportUtils::FindExportWaveTracks(TrackList::Get(mProject), selectedOnly);
      
      if(mSkipSilenceAtBeginning->GetValue())
         t0 = std::max(t0, tracks.min(&Track::GetStartTime));
      builder.SetRange(t0, t1, selectedOnly);
      
      ExportProgressUI::ExceptionWrappedCall([&]
      {
         result = ExportProgressUI::Show(builder.Build(mProject));
      });
   }
   
   if(result == ExportResult::Success || result == ExportResult::Stopped)
   {
      ExportAudioSampleRate.Write(mExportOptionsPanel->GetSampleRate());
      ExportAudioDefaultFormat.Write(selectedPlugin->GetFormatInfo(selectedFormat).format);
      ExportAudioSampleRate.Write(mExportOptionsPanel->GetSampleRate());
      
      ShuttleGui S(this, eIsSavingToPrefs);
      PopulateOrExchange(S);
      event.Skip();
   }
}

void ExportAudioDialog::OnFormatChange(wxCommandEvent& event)
{
   Layout();
   Fit();
   
   auto enableMeta = false;
   if(auto plugin = mExportOptionsPanel->GetPlugin())
      enableMeta = plugin->GetFormatInfo(mExportOptionsPanel->GetFormat()).canMetaData;
   mEditMetadata->Enable(enableMeta);
}

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

wxString MakeFileName(wxWindow* parent, const wxString &input)
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

      AudacityTextEntryDialog dlg( parent, msg, XO("Save As..."), newname );


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

unsigned GetNumExportChannels( const TrackList &tracks )
{
   bool anySolo =
      !((tracks.Leaders<const WaveTrack>() + &WaveTrack::GetSolo).empty());

   // Want only unmuted wave tracks.
   const auto range = tracks.Leaders<const WaveTrack>() -
      (anySolo ? &WaveTrack::GetNotSolo : &WaveTrack::GetMute);
   return std::all_of(range.begin(), range.end(),
      [](auto *pTrack){ return IsMono(*pTrack); }
   )
      ? 1
      : 2;
}

}

ExportResult ExportAudioDialog::DoExportSplitByLabels(const ExportPlugin& plugin,
                                                      int formatIndex,
                                                      const ExportProcessor::Parameters& parameters,
                                                      bool byName,
                                                      bool addNumber,
                                                      const wxString& prefix,
                                                      FilePaths& exporterFiles)
{
   const auto& tracks = TrackList::Get(mProject);
   const auto& labels = (*tracks.Leaders<const LabelTrack>().begin());
   const auto numLabels = labels->GetNumLabels();
   
   auto numFiles = numLabels;
   auto fileIndex = 0;        // counter for files done
   std::vector<ExportKit> exportSettings; // dynamic array for settings.
   exportSettings.reserve(numFiles); // Allocate some guessed space to use.

   // Account for exporting before first label
   if( mIncludeAudioBeforeFirstLabel->GetValue() ) {
      fileIndex = -1;
      numFiles++;
   }

   // Figure out how many channels we should export.
   auto channels = GetNumExportChannels( tracks );
   const auto formatInfo = plugin.GetFormatInfo(formatIndex);
   
   FilePaths otherNames;  // keep track of file names we will use, so we
   // don't duplicate them
   ExportKit setting;   // the current batch of settings
   setting.destfile.SetPath(mExportOptionsPanel->GetPath());
   setting.destfile.SetFullName(mExportOptionsPanel->GetFullName());
   
   wxString name;    // used to hold file name whilst we mess with it
   wxString title;   // un-messed-with title of file for tagging with
   
   const LabelStruct *info = NULL;
   /* Examine all labels a first time, sort out all data but don't do any
    * exporting yet (so this run is quick but interactive) */
   while( fileIndex < numLabels ) {

      // Get file name and starting time
      if( fileIndex < 0 ) {
         // create wxFileName for output file
         name = setting.destfile.GetName();
         setting.t0 = 0.0;
      } else {
         info = labels->GetLabel(fileIndex);
         name = (info->title);
         setting.t0 = info->selectedRegion.t0();
      }

      // Figure out the ending time
      if( info && !info->selectedRegion.isPoint() ) {
         setting.t1 = info->selectedRegion.t1();
      } else if( fileIndex < numLabels - 1 ) {
         // Use start of next label as end
         const LabelStruct *info1 = labels->GetLabel(fileIndex+1);
         setting.t1 = info1->selectedRegion.t0();
      } else {
         setting.t1 = tracks.GetEndTime();
      }

      if( name.empty() )
         name = _("untitled");

      // store title of label to use in tags
      title = name;

      // Numbering files...
      if( !byName ) {
         name.Printf(wxT("%s-%02d"), prefix, fileIndex + 1);
      } else if( addNumber ) {
         // Following discussion with GA, always have 2 digits
         // for easy file-name sorting (on Windows)
         name.Prepend(wxString::Format(wxT("%02d-"), fileIndex + 1));
      }

      // store sanitised and user checked name in object
      setting.destfile.SetName(MakeFileName(this, name));
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
         setting.filetags = Tags::Get( mProject );
         setting.filetags.LoadDefaults();
         if (exportSettings.size()) {
            setting.filetags = exportSettings.back().filetags;
         }
         // over-ride with values
         setting.filetags.SetTag(TAG_TITLE, title);
         setting.filetags.SetTag(TAG_TRACK, fileIndex+1);
         // let the user have a crack at editing it, exit if cancelled
         auto &settings = ProjectSettings::Get( mProject );
         bool bShowTagsDialog = settings.GetShowId3Dialog();

         bShowTagsDialog = bShowTagsDialog && formatInfo.canMetaData;

         if( bShowTagsDialog ){
            bool bCancelled = !TagsEditorDialog::ShowEditDialog(setting.filetags,
               ProjectWindow::Find( &mProject ),
               XO("Edit Metadata Tags"), bShowTagsDialog);
            gPrefs->Read(wxT("/AudioFiles/ShowId3Dialog"), &bShowTagsDialog, true);
            settings.SetShowId3Dialog( bShowTagsDialog );
            if( bCancelled )
               return ExportResult::Cancelled;
         }
      }

      /* add the settings to the array of settings to be used for export */
      exportSettings.push_back(setting);

      fileIndex++;  // next label, count up one
   }

   auto ok = ExportResult::Success;   // did it work?
   int count = 0; // count the number of successful runs
   ExportKit activeSetting;  // pointer to the settings in use for this export
   /* Go round again and do the exporting (so this run is slow but
    * non-interactive) */
   for (count = 0; count < numFiles; count++) {
      /* get the settings to use for the export from the array */
      activeSetting = exportSettings[count];
      // Bug 1440 fix.
      if( activeSetting.destfile.GetName().empty() )
         continue;

      // Export it
      ok = DoExport(plugin, formatIndex, parameters, activeSetting.destfile, channels,
         activeSetting.t0, activeSetting.t1, false, activeSetting.filetags, exporterFiles);
      
      if (ok == ExportResult::Stopped) {
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
      else if (ok != ExportResult::Success) {
         break;
      }
   }

   return ok;
}

ExportResult ExportAudioDialog::DoExportSplitByTracks(const ExportPlugin& plugin,
                                                      int formatIndex,
                                                      const ExportProcessor::Parameters& parameters,
                                                      bool byName,
                                                      bool addNumber,
                                                      const wxString& prefix,
                                                      FilePaths& exporterFiles)
{
   auto& tracks = TrackList::Get(mProject);
   
   bool anySolo = !(( tracks.Leaders<const WaveTrack>() + &WaveTrack::GetSolo ).empty());
   
   auto waveTracks = tracks.Leaders<WaveTrack>() -
      (anySolo ? &WaveTrack::GetNotSolo : &WaveTrack::GetMute);
   
   const auto numWaveTracks = waveTracks.size();
   
   auto fileIndex = 0;     // track counter
   auto ok = ExportResult::Success;
   FilePaths otherNames;
   auto formatInfo = plugin.GetFormatInfo(formatIndex);
   std::vector<ExportKit> exportSettings; // dynamic array we will use to store the
                                  // settings needed to do the exports with in
   exportSettings.reserve(numWaveTracks);   // Allocate some guessed space to use.
   ExportKit setting;   // the current batch of settings
   setting.destfile.SetPath(mExportOptionsPanel->GetPath());
   setting.destfile.SetExt(wxFileName{mExportOptionsPanel->GetFullName()}.GetExt());

   wxString name;    // used to hold file name whilst we mess with it
   wxString title;   // un-messed-with title of file for tagging with

   auto& selectionState = SelectionState::Get( mProject );
   
   /* Remember which tracks were selected, and set them to deselected */
   SelectionStateChanger changer{ selectionState, tracks };
   for (auto tr : tracks.Selected<WaveTrack>())
      tr->SetSelected(false);

   const auto skipSilenceAtBeginning = mSkipSilenceAtBeginning->GetValue();

   /* Examine all tracks in turn, collecting export information */
   for (auto tr : waveTracks) {

      // Get the times for the track
      auto channels = TrackList::Channels(tr);
      setting.t0 = skipSilenceAtBeginning ? channels.min(&Track::GetStartTime) : 0;
      setting.t1 = channels.max( &Track::GetEndTime );

      // number of export channels?
      // It's 1 only for a center-panned mono track
      setting.channels = (IsMono(*tr) && tr->GetPan() == 0.0) ? 1 : 2;
      // Get name and title
      title = tr->GetName();
      if( title.empty() )
         title = _("untitled");

      if (byName) {
         name = title;
         if (addNumber) {
            name.Prepend(
               wxString::Format(wxT("%02d-"), fileIndex+1));
         }
      }
      else {
         name = (wxString::Format(wxT("%s-%02d"), prefix, fileIndex+1));
      }

      // store sanitised and user checked name in object
      setting.destfile.SetName(MakeFileName(this, name));

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
         setting.filetags = Tags::Get( mProject );
         setting.filetags.LoadDefaults();
         if (exportSettings.size()) {
            setting.filetags = exportSettings.back().filetags;
         }
         // over-ride with values
         setting.filetags.SetTag(TAG_TITLE, title);
         setting.filetags.SetTag(TAG_TRACK, fileIndex + 1);
         // let the user have a crack at editing it, exit if cancelled
         auto &settings = ProjectSettings::Get( mProject );
         bool bShowTagsDialog = settings.GetShowId3Dialog();

         bShowTagsDialog = bShowTagsDialog && formatInfo.canMetaData;

         if( bShowTagsDialog ){
            bool bCancelled = !TagsEditorDialog::ShowEditDialog(setting.filetags,
               ProjectWindow::Find( &mProject ),
               XO("Edit Metadata Tags"), bShowTagsDialog);
            gPrefs->Read(wxT("/AudioFiles/ShowId3Dialog"), &bShowTagsDialog, true);
            settings.SetShowId3Dialog( bShowTagsDialog );
            if( bCancelled )
               return ExportResult::Cancelled;
         }
      }
      /* add the settings to the array of settings to be used for export */
      exportSettings.push_back(setting);

      fileIndex++;  // next track, count up one
   }
   // end of user-interactive data gathering loop, start of export processing
   // loop
   int count = 0; // count the number of successful runs
   ExportKit activeSetting;  // pointer to the settings in use for this export

   for (auto tr : waveTracks) {

      wxLogDebug( "Get setting %i", count );
      /* get the settings to use for the export from the array */
      activeSetting = exportSettings[count];
      if( activeSetting.destfile.GetName().empty() ){
         count++;
         continue;
      }

      /* Select the track */
      SelectionStateChanger changer2{ selectionState, tracks };
      const auto range = TrackList::Channels(tr);
      for (auto channel : range)
         channel->SetSelected(true);

      // Export the data. "channels" are per track.
      ok = DoExport(plugin, formatIndex, parameters, activeSetting.destfile, activeSetting.channels,
         activeSetting.t0, activeSetting.t1, true, activeSetting.filetags, exporterFiles);
      
      if (ok == ExportResult::Stopped) {
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
      
      else if (ok != ExportResult::Success) {
         break;
      }
      // increment export counter
      count++;
   }
   
   
   return ok ;
}

ExportResult ExportAudioDialog::DoExport(const ExportPlugin& plugin,
                                         int formatIndex,
                                         const ExportProcessor::Parameters& parameters,
                                         const wxFileName& filename,
                                         int channels,
                                         double t0, double t1, bool selectedOnly,
                                         const Tags& tags,
                                         FilePaths& exportedFiles)
{
   wxFileName name;

   wxLogDebug(wxT("Doing multiple Export: File name \"%s\""), (filename.GetFullName()));
   wxLogDebug(wxT("Channels: %i, Start: %lf, End: %lf "), channels, t0, t1);
   if (selectedOnly)
      wxLogDebug(wxT("Selected Region Only"));
   else
      wxLogDebug(wxT("Whole Project"));

   wxFileName backup;
   if (mOverwriteExisting->GetValue()) {
      name = filename;
      backup.Assign(name);

      int suffix = 0;
      do {
         backup.SetName(name.GetName() +
                           wxString::Format(wxT("%d"), suffix));
         ++suffix;
      }
      while (backup.FileExists());
      ::wxRenameFile(filename.GetFullPath(), backup.GetFullPath());
   }
   else {
      name = filename;
      int i = 2;
      wxString base(name.GetName());
      while (name.FileExists()) {
         name.SetName(wxString::Format(wxT("%s-%d"), base, i++));
      }
   }

   bool success{false};
   const wxString fullPath{name.GetFullPath()};

   auto cleanup = finally( [&] {
      if (backup.IsOk()) {
         if ( success )
            // Remove backup
            ::wxRemoveFile(backup.GetFullPath());
         else {
            // Restore original
            ::wxRemoveFile(fullPath);
            ::wxRenameFile(backup.GetFullPath(), fullPath);
         }
      }
      else {
         if ( ! success )
            // Remove any new, and only partially written, file.
            ::wxRemoveFile(fullPath);
      }
   } );
   
   auto result = ExportResult::Error;
   ExportProgressUI::ExceptionWrappedCall([&]
   {
      result = ExportProgressUI::Show(ExportTaskBuilder{}.SetPlugin(&plugin, formatIndex)
                                    .SetParameters(parameters)
                                    .SetRange(t0, t1)
                                    .SetTags(&tags)
                                    .SetNumChannels(channels)
                                    .SetFileName(fullPath)
                                    .Build(mProject));
   });

   success = result == ExportResult::Success || result == ExportResult::Stopped;

   if(success)
      exportedFiles.push_back(fullPath);
   
   return result;
}


