/**********************************************************************

  Audacity: A Digital Audio Editor
  Audacity(R) is copyright (c) 1999-2021 Audacity Team.
  License: GPL-v2-or-later.  See License.txt.

  ImportWavPack.cpp

  Subhradeep Chakraborty

*//****************************************************************//**

\class WavPackImportFileHandle
\brief An ImportFileHandle for WavPack data

*//****************************************************************//**

\class WavPackImportPlugin
\brief An ImportPlugin for WavPack data

*//*******************************************************************/

#include "Import.h"
#include "ImportPlugin.h"

#include<wx/string.h>

#include "Prefs.h"
#include "../Tags.h"
#include "../WaveTrack.h"
#include "../widgets/ProgressDialog.h"

#define DESC XO("WavPack files")

static const auto exts = {
   wxT("wv")
};

#ifndef USE_WAVPACK

static Importer::RegisteredUnusableImportPlugin registered
{
   std::make_unique<UnusableImportPlugin>(DESC, FileExtensions(exts.begin(), exts.end()))
};

#else

extern "C" {
#include<wavpack.h>
}


class WavPackImportPlugin final : public ImportPlugin
{
public:
   WavPackImportPlugin();
   ~WavPackImportPlugin();

   wxString GetPluginStringID() override;
   TranslatableString GetPluginFormatDescription() override; 
   std::unique_ptr<ImportFileHandle> Open(
     const FilePath &Filename, AudacityProject*) override;
};

using NewChannelGroup = std::vector< std::shared_ptr<WaveTrack> >;

class WavPackImportFileHandle final : public ImportFileHandle
{
public:
   WavPackImportFileHandle(const FilePath &filename, WavpackContext* wavpackContext);
   ~WavPackImportFileHandle();

   TranslatableString GetFileDescription() override;
   ByteCount GetFileUncompressedBytes() override;
   ProgressResult Import(WaveTrackFactory *trackFactory, TrackHolders &outTracks, Tags *tags) override;
   wxInt32 GetStreamCount() override;
   const TranslatableStrings &GetStreamInfo() override;
   void SetStreamUsage(wxInt32 StreamID, bool Use) override;

private:
   WavpackContext *mWavPackContext;
   int mNumChannels;
   uint32_t mSampleRate;
   int mBitsPerSample;
   int mBytesPerSample;
   int64_t mNumSamples;
   ProgressResult mUpdateResult;
   NewChannelGroup mChannels;
   sampleFormat mFormat;
};

// ============================================================================
// WavPackImportPlugin
// ============================================================================

WavPackImportPlugin::WavPackImportPlugin()
:  ImportPlugin(FileExtensions(exts.begin(), exts.end()))
{
}

WavPackImportPlugin::~WavPackImportPlugin()
{
}

wxString WavPackImportPlugin::GetPluginStringID()
{
   return wxT("libwavpack");
}

TranslatableString WavPackImportPlugin::GetPluginFormatDescription()
{
   return DESC;
}

std::unique_ptr<ImportFileHandle> WavPackImportPlugin::Open(const FilePath &filename, AudacityProject*)
{
   char errMessage[100]; // To hold possible error message
   int flags = OPEN_WVC | OPEN_FILE_UTF8;
   WavpackContext *wavpackContext = WavpackOpenFileInput(filename, errMessage, flags, 0);
   
   if (!wavpackContext) {
      // Some error occured(e.g. File not found or is invalid)
      return nullptr;
   }

   auto handle = std::make_unique<WavPackImportFileHandle>(filename, wavpackContext);

   return std::move(handle);
}

// ============================================================================
// WavPackImportFileHandle
// ============================================================================

WavPackImportFileHandle::WavPackImportFileHandle(const FilePath &filename,
                                                WavpackContext *wavpackContext)
:  ImportFileHandle(filename),
   mWavPackContext(wavpackContext)
{
   mNumChannels = WavpackGetNumChannels(mWavPackContext);
   mSampleRate = WavpackGetSampleRate(mWavPackContext);
   mBitsPerSample = WavpackGetBitsPerSample(mWavPackContext);
   mBytesPerSample = WavpackGetBytesPerSample(mWavPackContext);
   mNumSamples = WavpackGetNumSamples64(mWavPackContext);

   if (mBitsPerSample <= 16) {
      mFormat = int16Sample;
   } else if (mBitsPerSample <= 24) {
      mFormat = int24Sample;
   } else {
      mFormat = floatSample;
   }
}

TranslatableString WavPackImportFileHandle::GetFileDescription()
{
   return DESC;
}

auto WavPackImportFileHandle::GetFileUncompressedBytes() -> ByteCount
{
   return 0;
}

ProgressResult WavPackImportFileHandle::Import(WaveTrackFactory *trackFactory, TrackHolders &outTracks, Tags *tags)
{
   outTracks.clear();

   CreateProgress();

   mChannels.resize(mNumChannels);

   {
      auto iter = mChannels.begin();
      for (size_t c = 0; c < mNumChannels; ++iter, ++c)
         *iter = NewWaveTrack(*trackFactory, mFormat, mSampleRate);
   }

/* The number of samples to read in each loop */
#define SAMPLES_TO_READ 100000
   auto updateResult = ProgressResult::Success;
   uint32_t totalSamplesRead = 0;

   {
      uint32_t bufferSize = mNumChannels * SAMPLES_TO_READ;
      ArrayOf<int32_t> wavpackBuffer{ bufferSize };
      uint32_t samplesRead = 0;

      do {
         samplesRead = WavpackUnpackSamples(mWavPackContext, wavpackBuffer.get(), SAMPLES_TO_READ);

         for (unsigned int c = 0; c<samplesRead; c += mNumChannels) {
            auto iter = mChannels.begin();
            for (unsigned chn = 0; chn<mNumChannels; ++iter, ++chn) {
               iter->get()->Append((char *)&wavpackBuffer[c], mFormat, 1);
            }
         }

         totalSamplesRead += samplesRead;
         updateResult = mProgress->Update(WavpackGetProgress(mWavPackContext), 1.0);
      } while (updateResult == ProgressResult::Success && samplesRead != 0);
   }

   if (totalSamplesRead < mNumSamples)
      updateResult = ProgressResult::Failed;

   if (updateResult == ProgressResult::Failed)
      return updateResult;

   for (const auto &channel : mChannels)
      channel->Flush();

   if (!mChannels.empty())
      outTracks.push_back(std::move(mChannels));

   return updateResult;
}

wxInt32 WavPackImportFileHandle::GetStreamCount()
{
   return 1;
}

const TranslatableStrings &WavPackImportFileHandle::GetStreamInfo()
{
   static TranslatableStrings empty;
   return empty;
}

void WavPackImportFileHandle::SetStreamUsage(wxInt32 WXUNUSED(StreamID), bool WXUNUSED(Use))
{
}

WavPackImportFileHandle::~WavPackImportFileHandle()
{
   WavpackCloseFile(mWavPackContext);
}

#endif