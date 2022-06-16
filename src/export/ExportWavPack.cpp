/**********************************************************************

   SPDX-License-Identifier: GPL-2.0-or-later

   Audacity: A Digital Audio Editor

   ExportWavPack.cpp

   Subhradeep Chakraborty

   Based on ExportOGG.cpp, ExportMP2.cpp by:
   Joshua Haberman
   Markus Meyer

**********************************************************************/

#ifdef USE_WAVPACK

#include "Export.h"
#include "wxFileNameWrapper.h"
#include "FileIO.h"
#include "Prefs.h"
#include "Mix.h"

#include <wavpack/wavpack.h>
#include <wx/log.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/stream.h>

#include "../ShuttleGui.h"
#include "../ProjectSettings.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/ProgressDialog.h"
#include "Track.h"
#include "ProjectRate.h"
#include "../Tags.h"

//---------------------------------------------------------------------------
// ExportWavPackOptions
//---------------------------------------------------------------------------

#define ID_HYBRID_MODE 9000

class ExportWavPackOptions final : public wxPanelWrapper
{
public:

   ExportWavPackOptions(wxWindow *parent, int format);
   virtual ~ExportWavPackOptions();

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

   void OnHybridMode(wxCommandEvent& evt);

private:
   wxCheckBox *mHybridMode;
   wxCheckBox *mCreateCorrectionFile;
   wxChoice   *mBitRate;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(ExportWavPackOptions, wxPanelWrapper)
   EVT_CHECKBOX(ID_HYBRID_MODE, ExportWavPackOptions::OnHybridMode)
END_EVENT_TABLE()

ExportWavPackOptions::ExportWavPackOptions(wxWindow *parent, int WXUNUSED(format))
: wxPanelWrapper(parent, wxID_ANY)
{
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);

   TransferDataToWindow();
}

ExportWavPackOptions::~ExportWavPackOptions()
{
   TransferDataFromWindow();
}

const TranslatableStrings ExportQualityNames{
   XO("Low Quality(Fast)") ,
   XO("High Quality(Slow)") ,
   XO("Very High Quality(Slowest)") ,
};

const std::vector< int > ExportQualityValues{
   0,
   1,
   2,
};

/* 
Copied from ExportMP2.cpp by
   Joshua Haberman
   Markus Meyer
*/
namespace {

// i18n-hint kbps abbreviates "thousands of bits per second"
inline TranslatableString n_kbps( int n ) { return XO("%d kbps").Format( n ); }

const TranslatableStrings BitRateNames {
   n_kbps(16),
   n_kbps(24),
   n_kbps(32),
   n_kbps(40),
   n_kbps(48),
   n_kbps(56),
   n_kbps(64),
   n_kbps(80),
   n_kbps(96),
   n_kbps(112),
   n_kbps(128),
   n_kbps(160),
   n_kbps(192),
   n_kbps(224),
   n_kbps(256),
   n_kbps(320),
   n_kbps(384),
};

const std::vector< int > BitRateValues {
   16,
   24,
   32,
   40,
   48,
   56,
   64,
   80,
   96,
   112,
   128,
   160,
   192,
   224,
   256,
   320,
   384,
};

}

void ExportWavPackOptions::PopulateOrExchange(ShuttleGui & S)
{
   bool hybridMode = false;
   bool createCorrectionFile = false;
   IntSetting QualitySetting{ L"/FileFormats/WavPackEncodeQuality", 1 };
   IntSetting BitrateSetting{ L"/FileFormats/WavPackBitrate", 160 };

   gPrefs->Read(wxT("/FileFormats/WavPackHybridMode"), &hybridMode, 0);
   gPrefs->Read(wxT("/FileFormats/WavPackCreateCorrectionFile"), &createCorrectionFile, 0);

   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxEXPAND);
      {
         S.SetSizerProportion(1);
         S.StartMultiColumn(2, wxCENTER);
         {
            S.TieNumberAsChoice(
               XXO("Quality"),
               QualitySetting,
               ExportQualityNames,
               &ExportQualityValues
            );

            mHybridMode = S.Id(ID_HYBRID_MODE).TieCheckBox( XXO("Hybrid Mode"), hybridMode);
            mCreateCorrectionFile = S.Disable(!hybridMode).TieCheckBox( XXO("Create Correction(.wvc) File"), createCorrectionFile);

            mBitRate = S.Disable(!hybridMode).TieNumberAsChoice(
               XXO("Bit Rate:"),
               BitrateSetting,
               BitRateNames,
               &BitRateValues
            );
         }
         S.EndMultiColumn();
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();
}

bool ExportWavPackOptions::TransferDataToWindow()
{
   return true;
}

bool ExportWavPackOptions::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   gPrefs->Write(wxT("/FileFormats/WavPackCreateCorrectionFile"), mCreateCorrectionFile->GetValue());
   gPrefs->Flush();

   return true;
}

void ExportWavPackOptions::OnHybridMode(wxCommandEvent&)
{
   bool hybridMode = false;
   hybridMode = mHybridMode->GetValue();
   mCreateCorrectionFile->Enable(hybridMode);
   mBitRate->Enable(hybridMode);

   gPrefs->Write(wxT("/FileFormats/WavPackHybridMode"), hybridMode);
   gPrefs->Flush();
};

//---------------------------------------------------------------------------
// ExportWavPack
//---------------------------------------------------------------------------

typedef struct {
   uint32_t bytesWritten, firstBlockSize;
   std::unique_ptr<FileIO> file;
   int error;
} WriteId;

class ExportWavPack final : public ExportPlugin
{
public:

   ExportWavPack();

   void OptionsCreate(ShuttleGui &S, int format) override;

   ProgressResult Export(AudacityProject *project,
               std::unique_ptr<ProgressDialog> &pDialog,
               unsigned channels,
               const wxFileNameWrapper &fName,
               bool selectedOnly,
               double t0,
               double t1,
               MixerSpec *mixerSpec = NULL,
               const Tags *metadata = NULL,
               int subformat = 0) override;

   static int WriteBlock(void *id, void *data, int32_t length);
};

ExportWavPack::ExportWavPack()
:  ExportPlugin()
{
   AddFormat();
   SetFormat(wxT("WavPack"),0);
   AddExtension(wxT("wv"),0);
   SetMaxChannels(255,0);
   SetCanMetaData(true,0);
   SetDescription(XO("WavPack Files"),0);
}

ProgressResult ExportWavPack::Export(AudacityProject *project,
                       std::unique_ptr<ProgressDialog> &pDialog,
                       unsigned numChannels,
                       const wxFileNameWrapper &fName,
                       bool selectionOnly,
                       double t0,
                       double t1,
                       MixerSpec *mixerSpec,
                       const Tags *metadata,
                       int WXUNUSED(subformat))
{
   WavpackConfig config = {0};
   WriteId outWvFile = {NULL}, outWvcFile = {NULL};
   outWvFile.file = std::make_unique< FileIO >(fName, FileIO::Output);

   if (!outWvFile.file.get()->IsOpened()) {
      AudacityMessageBox( XO("Unable to open target file for writing") );
      return ProgressResult::Cancelled;
   }
   
   double rate = ProjectRate::Get( *project ).GetRate();
   const auto &tracks = TrackList::Get( *project );

   int quality = gPrefs->Read(wxT("/FileFormats/WavPackEncodeQuality"), 1);
   bool hybridMode = gPrefs->ReadBool(wxT("/FileFormats/WavPackHybridMode"), false);
   bool createCorrectionFile = gPrefs->ReadBool(wxT("/FileFormats/WavPackCreateCorrectionFile"), false);
   int bitRate = gPrefs->Read(wxT("/FileFormats/WavPackBitrate"), 160);

   config.num_channels = numChannels;
   config.sample_rate = rate;
   config.channel_mask = config.num_channels == 1 ? 4 : 3; // Microsoft standard, mono = 4, stereo = 3
   config.bits_per_sample = 16;
   config.bytes_per_sample = 2;

   if (quality == 0) {
      config.flags |= CONFIG_FAST_FLAG;
   } else if (quality == 1) {
      config.flags |= CONFIG_HIGH_FLAG;
   } else {
      config.flags |= CONFIG_VERY_HIGH_FLAG;
   }

   if (hybridMode) {
      config.flags |= CONFIG_HYBRID_FLAG;
      config.flags |= CONFIG_BITRATE_KBPS;
      config.bitrate = bitRate;

      if (createCorrectionFile) {
         config.flags |= CONFIG_CREATE_WVC;
         outWvcFile.file = std::make_unique< FileIO >(fName.GetFullPath().Append("c"), FileIO::Output);
      }
   }

   WavpackContext *wpc = WavpackOpenFileOutput(this->WriteBlock, &outWvFile, createCorrectionFile ? &outWvcFile : NULL);
   if (!WavpackSetConfiguration64(wpc, &config, -1, NULL) || !WavpackPackInit(wpc)) {
      WavpackCloseFile(wpc);
      return ProgressResult::Failed;
   }

// Samples to write per run
#define SAMPLES_PER_RUN 8192u

   uint32_t bufferSize = SAMPLES_PER_RUN * numChannels;
   ArrayOf<int32_t> wavpackBuffer{ bufferSize };
   auto updateResult = ProgressResult::Success;
   {
      auto mixer = CreateMixer(tracks, selectionOnly,
         t0, t1,
         numChannels, SAMPLES_PER_RUN, true,
         rate, int16Sample, mixerSpec);

      InitProgress( pDialog, fName,
         selectionOnly
            ? XO("Exporting selected audio as Wavpack file")
            : XO("Exporting the audio as Wavpack file") );
      auto &progress = *pDialog;

      while (updateResult == ProgressResult::Success) {
         auto samplesThisRun = mixer->Process(SAMPLES_PER_RUN);

         if (samplesThisRun == 0)
            break;

         char *mixed = (char *)(const char*)mixer->GetBuffer();
         for (decltype(samplesThisRun) j = 0; j < samplesThisRun; j++) {
            for (size_t i = 0; i < numChannels; i++) {
               int32_t value = *mixed++ & 0xff;
               value += *mixed++ << 8;
               wavpackBuffer[j*numChannels + i] = value;
            }
         }

         if (!WavpackPackSamples(wpc, wavpackBuffer.get(), samplesThisRun))
            updateResult = ProgressResult::Cancelled;

         if (updateResult == ProgressResult::Success)
            updateResult =
               progress.Update(mixer->MixGetCurrentTime() - t0, t1 - t0);
      }
   }

   if (!WavpackFlushSamples(wpc)) {
      updateResult = ProgressResult::Cancelled;
   } else {
      if (metadata == NULL)
         metadata = &Tags::Get( *project );

      wxString n;
      for (const auto &pair : metadata->GetRange()) {
         n = pair.first;
         const auto &v = pair.second;
         if (n == TAG_YEAR) {
            n = wxT("DATE");
         }
         WavpackAppendTagItem(wpc,
                              (char *) (const char *) n.mb_str(wxConvUTF8),
                              (char *) (const char *) v.mb_str(wxConvUTF8),
                              (int) v.length());
      }

      if (!WavpackWriteTag(wpc)) {
         // Not sure what to do
      }
   }

   WavpackCloseFile(wpc);

   if ( !outWvFile.file.get()->Close()
      || ( outWvcFile.file && outWvcFile.file.get() && !outWvcFile.file.get()->Close())) {
      updateResult = ProgressResult::Cancelled;
   }

   return updateResult;
}

// Based on the implementation of write_block in dbry/WavPack
// src: https://github.com/dbry/WavPack/blob/master/cli/wavpack.c
int ExportWavPack::WriteBlock(void *id, void *data, int32_t length)
{
   WriteId *outId = (WriteId *) id;

   uint32_t bcount;

   if (outId->error)
      return FALSE;

   if (outId && outId->file && outId->file.get() && data && length) {
      if ( outId->file.get()->Write(data, length).GetLastError() ) {
         outId->file = NULL;
         outId->error = 1;
         return FALSE;
      } else {
         outId->bytesWritten += length;

         if (!outId->firstBlockSize)
               outId->firstBlockSize = length;
      }
   }

   return TRUE;
}

void ExportWavPack::OptionsCreate(ShuttleGui &S, int format)
{
   S.AddWindow( safenew ExportWavPackOptions{ S.GetParent(), format } );
}

static Exporter::RegisteredExportPlugin sRegisteredPlugin{ "WavPack",
   []{ return std::make_unique< ExportWavPack >(); }
};

#endif
