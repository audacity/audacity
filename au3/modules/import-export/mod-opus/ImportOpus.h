/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/Import.h"
#include "au3-import-export/ImportPlugin.h"
#include "au3-import-export/ImportProgressListener.h"
#include "au3-import-export/ImportUtils.h"

#include "au3-tags/Tags.h"

#include "au3-wave-track/WaveTrack.h"

#include "au3-string-utils/CodeConversions.h"

#include <opus/opusfile.h>
#include <opus/opus.h>

class OpusImportPlugin final : public ImportPlugin
{
public:
    OpusImportPlugin();
    ~OpusImportPlugin();

    wxString GetPluginStringID() override;
    TranslatableString GetPluginFormatDescription() override;
    std::unique_ptr<ImportFileHandle> Open(
        const FilePath& Filename, AudacityProject*) override;
};

class OpusImportFileHandle final : public ImportFileHandleEx
{
public:
    explicit OpusImportFileHandle(const FilePath& filename);
    ~OpusImportFileHandle();

    bool IsOpen() const;

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
    static int OpusReadCallback(void* stream, unsigned char* ptr, int nbytes);
    static int OpusSeekCallback(void* stream, opus_int64 offset, int whence);
    static opus_int64 OpusTellCallback(void* stream);
    static int OpusCloseCallback(void* stream);

    static TranslatableString GetOpusErrorString(int error);
    void LogOpusError(const char* method, int error);
    void NotifyImportFailed(ImportProgressListener& progressListener, int error);
    void NotifyImportFailed(ImportProgressListener& progressListener, const TranslatableString& error);

    wxFile mFile;

    OpusFileCallbacks mCallbacks;
    OggOpusFile* mOpusFile {};
    int mNumChannels {};
    int64_t mNumSamples {};

    // Opus internally uses 48kHz sample rate
    // The file header contains the sample rate of the original audio.
    // We ignore it and let Audacity resample the audio to the project sample rate.
    const double mSampleRate { 48000.0 };

    // Opus decodes to float samples internally, optionally converting them to int16.
    // We let Audacity to convert the stream to the project sample format.
    const sampleFormat mFormat { floatSample };
};
