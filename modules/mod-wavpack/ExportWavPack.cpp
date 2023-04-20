/**********************************************************************

   SPDX-License-Identifier: GPL-2.0-or-later

   Audacity: A Digital Audio Editor

   ExportWavPack.cpp

   Subhradeep Chakraborty

   Based on ExportOGG.cpp, ExportMP2.cpp by:
   Joshua Haberman
   Markus Meyer

**********************************************************************/

#include "Export.h"
#include "wxFileNameWrapper.h"
#include "Mix.h"

#include <wavpack/wavpack.h>

#include <rapidjson/document.h>

#include "Track.h"
#include "ProjectRate.h"
#include "Tags.h"

#include "ExportPluginHelpers.h"
#include "ExportOptionsEditor.h"

namespace
{

enum : int {
   OptionIDQuality = 0,
   OptionIDBitDepth,
   OptionIDHybridMode,
   OptionIDCreateCorrection,
   OptionIDBitRate
};

const TranslatableStrings ExportQualityNames{
   XO("Low Quality (Fast)") ,
   XO("Normal Quality") ,
   XO("High Quality (Slow)") ,
   XO("Very High Quality (Slowest)") ,
};

const TranslatableStrings ExportBitDepthNames {
   XO("16 bit"),
   XO("24 bit"),
   XO("32 bit float "),
};

/* 
Copied from ExportMP2.cpp by
   Joshua Haberman
   Markus Meyer
*/

// i18n-hint bps abbreviates "bits per sample"
inline TranslatableString n_bps( int n ) { return XO("%.1f bps").Format( n / 10.0 ); }

const TranslatableStrings BitRateNames {
   n_bps(22),
   n_bps(25),
   n_bps(30),
   n_bps(35),
   n_bps(40),
   n_bps(45),
   n_bps(50),
   n_bps(60),
   n_bps(70),
   n_bps(80),
};


const std::initializer_list<ExportOption> ExportWavPackOptions {
   {
      
      OptionIDQuality, XO("Quality"),
      1,
      ExportOption::TypeEnum,
      { 0, 1, 2, 3 },
      ExportQualityNames
   },
   {
      OptionIDBitDepth, XO("Bit Depth"),
      16,
      ExportOption::TypeEnum,
      { 16, 24, 32 },
      ExportBitDepthNames
   },
   {
      OptionIDHybridMode, XO("Hybrid Mode"),
      false
   },
   {
      OptionIDCreateCorrection, XO("Create Correction(.wvc) File"),
      false,
      ExportOption::ReadOnly
   },
   {
      OptionIDBitRate, XO("Bit Rate"),
      40,
      ExportOption::TypeEnum,
      { 22, 25, 30, 35, 40, 45, 50, 60, 70, 80 },
      BitRateNames
   }
};

class ExportOptionsWavPackEditor final : public ExportOptionsEditor
{
   Listener* mListener{nullptr};
   std::vector<ExportOption> mOptions = ExportWavPackOptions;
   std::unordered_map<ExportOptionID, ExportValue> mValues;
public:

   ExportOptionsWavPackEditor(Listener* listener)
      : mListener(listener)
   {
      for(const auto& option : mOptions)
         mValues[option.id] = option.defaultValue;
   }

   int GetOptionsCount() const override
   {
      return static_cast<int>(mOptions.size());
   }

   bool GetOption(int index, ExportOption& option) const override
   {
      if(index >= 0 && index < mOptions.size())
      {
         option = mOptions[index];
         return true;
      }
      return false;
   }

   bool GetValue(ExportOptionID id, ExportValue& value) const override
   {
      const auto it = mValues.find(id);
      if(it != mValues.end())
      {
         value = it->second;
         return true;
      }
      return false;
   }

   bool SetValue(ExportOptionID id, const ExportValue& value) override
   {
      auto it = mValues.find(id);
      if(it == mValues.end() || value.index() != it->second.index())
         return false;

      it->second = value;
      if(id == OptionIDHybridMode)
      {
         OnHybridModeChange(*std::get_if<bool>(&value));

         if(mListener)
         {
            mListener->OnExportOptionChangeBegin();
            mListener->OnExportOptionChange(mOptions[OptionIDCreateCorrection]);
            mListener->OnExportOptionChangeEnd();
         }
      }
      return true;
   }

   void Load(const wxConfigBase& config) override
   {
      auto quality = std::get_if<int>(&mValues[OptionIDQuality]);
      auto bitDepth = std::get_if<int>(&mValues[OptionIDBitDepth]);
      auto hybridMode = std::get_if<bool>(&mValues[OptionIDHybridMode]);
      auto createCorrection = std::get_if<bool>(&mValues[OptionIDCreateCorrection]);
      auto bitRate = std::get_if<int>(&mValues[OptionIDBitRate]);

      config.Read(L"/FileFormats/WavPackEncodeQuality", quality);
      config.Read(L"/FileFormats/WavPackBitDepth", bitDepth);
      config.Read(L"/FileFormats/WavPackHybridMode", hybridMode);
      config.Read(L"/FileFormats/WavPackCreateCorrectionFile", createCorrection);
      config.Read(L"/FileFormats/WavPackBitrate", bitRate);
      
      OnHybridModeChange(*hybridMode);
   }

   void Store(wxConfigBase& config) const override
   {
      auto it = mValues.find(OptionIDQuality);
      if(it != mValues.end())
         config.Write(L"/FileFormats/WavPackEncodeQuality", *std::get_if<int>(&it->second));

      it = mValues.find(OptionIDBitDepth);
      if(it != mValues.end())
         config.Write(L"/FileFormats/WavPackBitDepth", *std::get_if<int>(&it->second));

      it = mValues.find(OptionIDHybridMode);
      if(it != mValues.end())
         config.Write(L"/FileFormats/WavPackHybridMode", *std::get_if<bool>(&it->second));

      it = mValues.find(OptionIDCreateCorrection);
      if(it != mValues.end())
         config.Write(L"/FileFormats/WavPackCreateCorrectionFile", *std::get_if<bool>(&it->second));

      it = mValues.find(OptionIDBitRate);
      if(it != mValues.end())
         config.Write(L"/FileFormats/WavPackBitrate", *std::get_if<int>(&it->second));
   }
   
private:
   void OnHybridModeChange(bool hybridMode)
   {
      if(hybridMode)
         mOptions[OptionIDCreateCorrection].flags &= ~ExportOption::Flags::ReadOnly;
      else
         mOptions[OptionIDCreateCorrection].flags |= ExportOption::Flags::ReadOnly;
   }
};

}

struct WriteId final
{
   uint32_t bytesWritten {};
   uint32_t firstBlockSize {};
   std::unique_ptr<wxFile> file;
};

class ExportWavPack final : public ExportPlugin
{
public:

   ExportWavPack();

   int GetFormatCount() const override;
   FormatInfo GetFormatInfo(int) const override;

   std::vector<std::string> GetMimeTypes(int) const override;

   bool ParseConfig(int formatIndex, const rapidjson::Value& document, Parameters& parameters) const override;

   std::unique_ptr<ExportOptionsEditor>
   CreateOptionsEditor(int, ExportOptionsEditor::Listener*) const override;

   ExportResult Export(AudacityProject *project,
               ExportPluginDelegate &delegate,
               const Parameters& parameters,
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

ExportWavPack::ExportWavPack() = default;

int ExportWavPack::GetFormatCount() const
{
   return 1;
}

FormatInfo ExportWavPack::GetFormatInfo(int) const
{
   return {
      wxT("WavPack"), XO("WavPack Files"), { wxT("wv") }, 255, true
   };
}

std::vector<std::string> ExportWavPack::GetMimeTypes(int) const
{
   return { "audio/x-wavpack" };
}

bool ExportWavPack::ParseConfig(int formatIndex, const rapidjson::Value& config, Parameters& parameters) const
{
   if(!config.IsObject() || 
      !config.HasMember("quality") || !config["quality"].IsNumber() ||
      !config.HasMember("bit_rate") || !config["bit_rate"].IsNumber() ||
      !config.HasMember("bit_depth") || !config["bit_depth"].IsNumber() ||
      !config.HasMember("hybrid_mode") || !config["hybrid_mode"].IsBool())
      return false;

   const auto quality = ExportValue(config["quality"].GetInt());
   const auto bitRate = ExportValue(config["bit_rate"].GetInt());
   const auto bitDepth = ExportValue(config["bit_depth"].GetInt());
   const auto hybridMode = ExportValue(config["hybrid_mode"].GetBool());

   for(const auto& option : ExportWavPackOptions)
   {
      if((option.id == OptionIDQuality &&
         std::find(option.values.begin(),
            option.values.end(),
            quality) == option.values.end())
         ||
         (option.id == OptionIDBitRate &&
         std::find(option.values.begin(),
            option.values.end(),
            bitRate) == option.values.end())
         ||
         (option.id == OptionIDBitDepth &&
         std::find(option.values.begin(),
            option.values.end(),
            bitDepth) == option.values.end()))
         return false;
   }
   Parameters result {
      { OptionIDQuality, quality },
      { OptionIDBitRate, bitRate },
      { OptionIDBitDepth, bitDepth },
      { OptionIDHybridMode, hybridMode },
      { OptionIDCreateCorrection, false }
   };
   std::swap(parameters, result);
   return true;
}

std::unique_ptr<ExportOptionsEditor>
ExportWavPack::CreateOptionsEditor(int, ExportOptionsEditor::Listener* listener) const
{
   return std::make_unique<ExportOptionsWavPackEditor>(listener);
}


ExportResult ExportWavPack::Export(AudacityProject *project,
                           ExportPluginDelegate &delegate,
                           const Parameters& parameters,
                           unsigned numChannels,
                           const wxFileNameWrapper &fName,
                           bool selectionOnly,
                           double t0,
                           double t1,
                           MixerSpec *mixerSpec,
                           const Tags *metadata,
                           int)
{
   WavpackConfig config = {};
   WriteId outWvFile, outWvcFile;
   outWvFile.file = std::make_unique< wxFile >();

   if (!outWvFile.file->Create(fName.GetFullPath(), true) || !outWvFile.file.get()->IsOpened()) {
      delegate.SetErrorString(XO("Unable to open target file for writing"));
      return ExportResult::Error;
   }
   
   double rate = ProjectRate::Get( *project ).GetRate();
   const auto &tracks = TrackList::Get( *project );

   const auto quality = ExportPluginHelpers::GetParameterValue<int>(
      parameters,
      OptionIDQuality,
      1);
   const auto hybridMode = ExportPluginHelpers::GetParameterValue<bool>(
      parameters,
      OptionIDHybridMode,
      false);
   const auto createCorrectionFile = ExportPluginHelpers::GetParameterValue<bool>(
      parameters,
      OptionIDCreateCorrection,
      false);
   const auto bitRate = ExportPluginHelpers::GetParameterValue<int>(
      parameters,
      OptionIDBitRate,
      40);
   const auto bitDepth = ExportPluginHelpers::GetParameterValue<int>(
      parameters,
      OptionIDBitDepth,
      16);


   sampleFormat format = int16Sample;
   if (bitDepth == 24) {
      format = int24Sample;
   } else if (bitDepth == 32) {
      format = floatSample;
   }

   config.num_channels = numChannels;
   config.sample_rate = rate;
   config.bits_per_sample = bitDepth;
   config.bytes_per_sample = bitDepth/8;
   config.float_norm_exp = format == floatSample ? 127 : 0;

   if (config.num_channels <= 2)
      config.channel_mask = 0x5 - config.num_channels;
   else if (config.num_channels <= 18)
      config.channel_mask = (1U << config.num_channels) - 1;
   else
      config.channel_mask = 0x3FFFF;

   if (quality == 0) {
      config.flags |= CONFIG_FAST_FLAG;
   } else if (quality == 2) {
      config.flags |= CONFIG_HIGH_FLAG;
   } else if (quality == 3) {
      config.flags |= CONFIG_HIGH_FLAG | CONFIG_VERY_HIGH_FLAG;
   }

   if (hybridMode) {
      config.flags |= CONFIG_HYBRID_FLAG;
      config.bitrate = bitRate / 10.0;

      if (createCorrectionFile) {
         config.flags |= CONFIG_CREATE_WVC;

         outWvcFile.file = std::make_unique< wxFile >();
         if (!outWvcFile.file->Create(fName.GetFullPath().Append("c"), true)) {
            delegate.SetErrorString(XO("Unable to create target file for writing"));
            return ExportResult::Error;
         }
      }
   }

   // If we're not creating a correction file now, any one that currently exists with this name
   // will become obsolete now, so delete it if it happens to exist (although it usually won't)

   if (!hybridMode || !createCorrectionFile)
      wxRemoveFile(fName.GetFullPath().Append("c"));

   WavpackContext *wpc = WavpackOpenFileOutput(WriteBlock, &outWvFile, createCorrectionFile ? &outWvcFile : nullptr);
   auto closeWavPackContext = finally([wpc]() { WavpackCloseFile(wpc); });

   if (!WavpackSetConfiguration64(wpc, &config, -1, nullptr) || !WavpackPackInit(wpc)) {
      ShowExportErrorDialog( WavpackGetErrorMessage(wpc) );
      return ExportResult::Error;
   }

   // Samples to write per run
   constexpr size_t SAMPLES_PER_RUN = 8192u;

   const size_t bufferSize = SAMPLES_PER_RUN * numChannels;
   ArrayOf<int32_t> wavpackBuffer{ bufferSize };

   auto exportResult = ExportResult::Success;
   {
      auto mixer = ExportPluginHelpers::CreateMixer(tracks, selectionOnly,
         t0, t1,
         numChannels, SAMPLES_PER_RUN, true,
         rate, format, mixerSpec);

      delegate.SetStatusString(selectionOnly
         ? XO("Exporting selected audio as WavPack")
         : XO("Exporting the audio as WavPack"));

      while (exportResult == ExportResult::Success) {
         auto samplesThisRun = mixer->Process();

         if (samplesThisRun == 0)
            break;
         
         if (format == int16Sample) {
            const int16_t *mixed = reinterpret_cast<const int16_t*>(mixer->GetBuffer());
            for (decltype(samplesThisRun) j = 0; j < samplesThisRun; j++) {
               for (size_t i = 0; i < numChannels; i++) {
                  wavpackBuffer[j*numChannels + i] = (static_cast<int32_t>(*mixed++) * 65536) >> 16;
               }
            }
         } else {
            const int *mixed = reinterpret_cast<const int*>(mixer->GetBuffer());
            for (decltype(samplesThisRun) j = 0; j < samplesThisRun; j++) {
               for (size_t i = 0; i < numChannels; i++) {
                  wavpackBuffer[j*numChannels + i] = *mixed++;
               }
            }
         }

         if (!WavpackPackSamples(wpc, wavpackBuffer.get(), samplesThisRun)) {
            ShowExportErrorDialog( WavpackGetErrorMessage(wpc) );
            return ExportResult::Error;
         }
         exportResult = ExportPluginHelpers::UpdateProgress(
            delegate, *mixer, t0, t1);
      }
   }

   if (!WavpackFlushSamples(wpc)) {
      ShowExportErrorDialog( WavpackGetErrorMessage(wpc) );
      return ExportResult::Error;
   } else {
      if (metadata == NULL)
         metadata = &Tags::Get( *project );

      wxString n;
      for (const auto &pair : metadata->GetRange()) {
         n = pair.first;
         const auto &v = pair.second;

         WavpackAppendTagItem(wpc,
                              n.mb_str(wxConvUTF8),
                              v.mb_str(wxConvUTF8),
                              static_cast<int>( strlen(v.mb_str(wxConvUTF8)) ));
      }

      if (!WavpackWriteTag(wpc)) {
         ShowExportErrorDialog( WavpackGetErrorMessage(wpc) );
         return ExportResult::Error;
      }
   }

   if ( !outWvFile.file.get()->Close()
      || ( outWvcFile.file && outWvcFile.file.get() && !outWvcFile.file.get()->Close())) {
      return ExportResult::Error;
   }

   // wxFile::Create opens the file with only write access
   // So, need to open the file again with both read and write access
   if (!outWvFile.file->Open(fName.GetFullPath(), wxFile::read_write)) {
      ShowExportErrorDialog( "Unable to update the actual length of the file" );
      return ExportResult::Error;
   }

   ArrayOf<int32_t> firstBlockBuffer { outWvFile.firstBlockSize };
   size_t bytesRead = outWvFile.file->Read(firstBlockBuffer.get(), outWvFile.firstBlockSize);

   // Update the first block written with the actual number of samples written
   WavpackUpdateNumSamples(wpc, firstBlockBuffer.get());
   outWvFile.file->Seek(0);
   size_t bytesWritten = outWvFile.file->Write(firstBlockBuffer.get(), outWvFile.firstBlockSize);

   if ( !outWvFile.file.get()->Close() ) {
      return ExportResult::Error;
   }
   return exportResult;
}

// Based on the implementation of write_block in dbry/WavPack
// src: https://github.com/dbry/WavPack/blob/master/cli/wavpack.c
int ExportWavPack::WriteBlock(void *id, void *data, int32_t length)
{
    if (id == nullptr || data == nullptr || length == 0)
        return true; // This is considered to be success in wavpack.c reference code

    WriteId *outId = static_cast<WriteId*>(id);

    if (!outId->file)
        // This does not match the wavpack.c but in our case if file is nullptr - 
        // the stream error has occured
        return false; 

   //  if (!outId->file->Write(data, length).IsOk()) {
    if (outId->file->Write(data, length) != length) {
        outId->file.reset();
        return false;
    }

    outId->bytesWritten += length;

    if (outId->firstBlockSize == 0)
        outId->firstBlockSize = length;

    return true;
}

static Exporter::RegisteredExportPlugin sRegisteredPlugin{ "WavPack",
   []{ return std::make_unique< ExportWavPack >(); }
};
