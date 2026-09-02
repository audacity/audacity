/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "CVSD.h"
#include "au3-import-export/Import.h"

#include "au3-tags/Tags.h"

#include "au3-wave-track/WaveTrack.h"
#include "au3-import-export/ImportPlugin.h"
#include "au3-import-export/ImportUtils.h"

class CVSDImportPlugin final : public ImportPlugin
{
public:
    CVSDImportPlugin();
    ~CVSDImportPlugin() override = default;

    wxString GetPluginStringID() override;
    TranslatableString GetPluginFormatDescription() override;
    FileExtensions GetSupportedExtensions() override;
    std::unique_ptr<ImportFileHandle> Open(
        const FilePath& Filename, AudacityProject*) override;
};

class CVSDImportFileHandle final : public ImportFileHandleEx
{
public:
    CVSDImportFileHandle(const FilePath& filename);
    ~CVSDImportFileHandle();

    TranslatableString GetFileDescription() override;
    double GetDuration() const override;
    int GetRequiredTrackCount() const override;
    ByteCount GetFileUncompressedBytes() override;
    void Import(
        ImportProgressListener& progressListener, WaveTrackFactory* trackFactory, TrackHolders& outTracks, Tags* tags,
        std::optional<LibFileFormats::AcidizerTags>& outAcidTags) override;
    wxInt32 GetStreamCount() override;
    const TranslatableStrings& GetStreamInfo() override;
    void SetStreamUsage(wxInt32 StreamID, bool Use) override;

private:
    TranslatableStrings mStreamInfo;
    int mNumChannels;
    uint32_t mSampleRate;
    int mBitsPerSample;
    int mBytesPerSample;
    int64_t mNumSamples;
    CVSD_CONFIG mDecoderConfig;
    sampleFormat mFormat;
    std::unique_ptr<wxFFile> wxCVSDFile;
};
