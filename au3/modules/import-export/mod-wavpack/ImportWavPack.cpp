/**********************************************************************

  Audacity: A Digital Audio Editor
  Audacity(R) is copyright (c) 1999-2021 Audacity Team.
  SPDX-License-Identifier: GPL-2.0-or-later.  See License.txt.

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

#include <wx/string.h>
#include <wx/log.h>
#include <stdlib.h>
#include <wavpack/wavpack.h>

#include "Tags.h"
#include "WaveTrack.h"
#include "CodeConversions.h"
#include "ImportUtils.h"
#include "ImportProgressListener.h"

#define DESC XO("WavPack files")

static const auto exts = {
    wxT("wv")
};

class WavPackImportPlugin final : public ImportPlugin
{
public:
    WavPackImportPlugin();
    ~WavPackImportPlugin();

    wxString GetPluginStringID() override;
    TranslatableString GetPluginFormatDescription() override;
    std::unique_ptr<ImportFileHandle> Open(
        const FilePath& Filename, AudacityProject*) override;
};

class WavPackImportFileHandle final : public ImportFileHandleEx
{
public:
    WavPackImportFileHandle(const FilePath& filename, WavpackContext* wavpackContext);
    ~WavPackImportFileHandle();

    TranslatableString GetFileDescription() override;
    ByteCount GetFileUncompressedBytes() override;
    void Import(ImportProgressListener& progressListener, WaveTrackFactory* trackFactory, TrackHolders& outTracks, Tags* tags,
                std::optional<LibFileFormats::AcidizerTags>& outAcidTags) override;

    wxInt32 GetStreamCount() override;
    const TranslatableStrings& GetStreamInfo() override;
    void SetStreamUsage(wxInt32 StreamID, bool Use) override;

private:
    WavpackContext* mWavPackContext;
    int mNumChannels;
    uint32_t mSampleRate;
    int mBitsPerSample;
    int mBytesPerSample;
    int64_t mNumSamples;
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

std::unique_ptr<ImportFileHandle> WavPackImportPlugin::Open(const FilePath& filename, AudacityProject*)
{
    char errMessage[100]; // To hold possible error message
    int flags = OPEN_WVC | OPEN_FILE_UTF8 | OPEN_TAGS | OPEN_DSD_AS_PCM | OPEN_NORMALIZE;
    WavpackContext* wavpackContext = WavpackOpenFileInput(filename, errMessage, flags, 0);

    if (!wavpackContext) {
        // Some error occured(e.g. File not found or is invalid)
        wxLogDebug("WavpackOpenFileInput() failed on file %s, error = %s", filename, errMessage);
        return nullptr;
    }

    auto handle = std::make_unique<WavPackImportFileHandle>(filename, wavpackContext);

    return std::move(handle);
}

static Importer::RegisteredImportPlugin registered{ "WavPack",
                                                    std::make_unique< WavPackImportPlugin >()
};

// ============================================================================
// WavPackImportFileHandle
// ============================================================================

WavPackImportFileHandle::WavPackImportFileHandle(const FilePath& filename,
                                                 WavpackContext* wavpackContext)
    :  ImportFileHandleEx(filename),
    mWavPackContext(wavpackContext),
    mNumChannels(WavpackGetNumChannels(mWavPackContext)),
    mSampleRate(WavpackGetSampleRate(mWavPackContext)),
    mBitsPerSample(WavpackGetBitsPerSample(mWavPackContext)),
    mBytesPerSample(WavpackGetBytesPerSample(mWavPackContext)),
    mNumSamples(WavpackGetNumSamples64(mWavPackContext))
{
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

void WavPackImportFileHandle::Import(
    ImportProgressListener& progressListener, WaveTrackFactory* trackFactory,
    TrackHolders& outTracks, Tags* tags,
    std::optional<LibFileFormats::AcidizerTags>&)
{
    BeginImport();

    const int wavpackMode = WavpackGetMode(mWavPackContext);

    outTracks.clear();

    auto tracks = trackFactory->CreateMany(mNumChannels, mFormat, mSampleRate);

    /* The number of samples to read in each loop */
    const size_t SAMPLES_TO_READ = (*tracks->Any<WaveTrack>().begin())->GetMaxBlockSize();
    uint32_t totalSamplesRead = 0;

    {
        const uint32_t bufferSize = mNumChannels * SAMPLES_TO_READ;
        ArrayOf<int32_t> wavpackBuffer{ bufferSize };
        ArrayOf<int16_t> int16Buffer;
        ArrayOf<float> floatBuffer;
        uint32_t samplesRead = 0;

        if (mFormat == int16Sample) {
            int16Buffer.reinit(bufferSize);
        } else if (mFormat == floatSample && (wavpackMode & MODE_FLOAT) != MODE_FLOAT) {
            floatBuffer.reinit(bufferSize);
        }

        do {
            samplesRead = WavpackUnpackSamples(mWavPackContext, wavpackBuffer.get(), SAMPLES_TO_READ);

            if (mFormat == int16Sample) {
                if (mBytesPerSample == 1) {
                    for (int64_t c = 0; c < samplesRead * mNumChannels; c++) {
                        int16Buffer[c] = static_cast<int16_t>(wavpackBuffer[c] * 256);
                    }
                } else {
                    for (int64_t c = 0; c < samplesRead * mNumChannels; c++) {
                        int16Buffer[c] = static_cast<int16_t>(wavpackBuffer[c]);
                    }
                }

                unsigned chn = 0;
                ImportUtils::ForEachChannel(*tracks, [&](auto& channel)
                {
                    channel.AppendBuffer(
                        reinterpret_cast<constSamplePtr>(int16Buffer.get() + chn),
                        mFormat,
                        samplesRead,
                        mNumChannels,
                        mFormat
                        );
                    ++chn;
                });
            } else if (mFormat == int24Sample || (wavpackMode & MODE_FLOAT) == MODE_FLOAT) {
                unsigned chn = 0;
                ImportUtils::ForEachChannel(*tracks, [&](auto& channel)
                {
                    channel.AppendBuffer(
                        reinterpret_cast<constSamplePtr>(wavpackBuffer.get() + chn),
                        mFormat,
                        samplesRead,
                        mNumChannels,
                        mFormat
                        );
                    ++chn;
                });
            } else {
                for (int64_t c = 0; c < samplesRead * mNumChannels; c++) {
                    floatBuffer[c] = static_cast<float>(wavpackBuffer[c] / static_cast<double>(std::numeric_limits<int32_t>::max()));
                }

                unsigned chn = 0;
                ImportUtils::ForEachChannel(*tracks, [&](auto& channel)
                {
                    channel.AppendBuffer(
                        reinterpret_cast<constSamplePtr>(floatBuffer.get() + chn),
                        mFormat,
                        samplesRead,
                        mNumChannels,
                        mFormat
                        );
                    ++chn;
                });
            }

            totalSamplesRead += samplesRead;

            progressListener.OnImportProgress(WavpackGetProgress(mWavPackContext));
        } while (!IsCancelled() && !IsStopped() && samplesRead != 0);
    }

    if (WavpackGetNumErrors(mWavPackContext)) {
        ImportUtils::ShowMessageBox(
            XO("Encountered %d errors decoding WavPack file!").Format(WavpackGetNumErrors(mWavPackContext)));
    }

    if (IsCancelled()) {
        progressListener.OnImportResult(ImportProgressListener::ImportResult::Cancelled);
        return;
    }

    if (totalSamplesRead < mNumSamples && !IsStopped()) {
        progressListener.OnImportResult(ImportProgressListener::ImportResult::Error);
        return;
    }

    ImportUtils::FinalizeImport(outTracks, std::move(*tracks));

    if (wavpackMode & MODE_VALID_TAG) {
        bool apeTag = wavpackMode & MODE_APETAG;
        int numItems = WavpackGetNumTagItems(mWavPackContext);

        if (numItems > 0) {
            tags->Clear();
            for (int i = 0; i < numItems; i++) {
                int itemLen = 0, valueLen = 0;
                wxString value, name;

                // Get the actual length of the item key at this index i
                itemLen = WavpackGetTagItemIndexed(mWavPackContext, i, NULL, 0);
                std::string item(itemLen + 1, '\0');
                WavpackGetTagItemIndexed(mWavPackContext, i, item.data(), itemLen + 1);
                item.resize(itemLen);       // remove terminating NULL from std::string
                name = audacity::ToWXString(item);

                // Get the actual length of the value for this item key
                valueLen = WavpackGetTagItem(mWavPackContext, item.data(), NULL, 0);
                std::string itemValue(valueLen + 1, '\0');
                WavpackGetTagItem(mWavPackContext, item.data(), itemValue.data(), valueLen + 1);
                itemValue.resize(valueLen); // remove terminating NULL from std::string

                if (apeTag) {
                    for (int j = 0; j < valueLen; j++) {
                        // APEv2 text tags can have multiple NULL separated string values
                        if (!itemValue[j]) {
                            itemValue[j] = '\n';
                        }
                    }
                }
                value = audacity::ToWXString(itemValue);

                if (name.Upper() == wxT("DATE") && !tags->HasTag(TAG_YEAR)) {
                    long val;
                    if (value.length() == 4 && value.ToLong(&val)) {
                        name = TAG_YEAR;
                    }
                }

                tags->SetTag(name, value);
            }
        }
    }

    progressListener.OnImportResult(IsStopped()
                                    ? ImportProgressListener::ImportResult::Stopped
                                    : ImportProgressListener::ImportResult::Success);
}

wxInt32 WavPackImportFileHandle::GetStreamCount()
{
    return 1;
}

const TranslatableStrings& WavPackImportFileHandle::GetStreamInfo()
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
