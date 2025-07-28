/*
* Audacity: A Digital Audio Editor
*/

#include <wavpack/wavpack.h>

#include "SampleFormat.h"
#include "libraries/lib-import-export/ImportPlugin.h"

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
