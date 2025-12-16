/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/Import.h"
#include "au3-import-export/ImportPlugin.h"
#include "au3-import-export/ImportProgressListener.h"
#include "au3-import-export/ImportUtils.h"

#include "au3-wave-track/WaveTrack.h"

#include "au3-tags/Tags.h"

#include "FLAC++/decoder.h"

#define FLAC_HEADER "fLaC"

#define DESC XO("FLAC files")

class FLACImportFileHandle;

class MyFLACFile final : public FLAC::Decoder::File
{
public:
    MyFLACFile(FLACImportFileHandle* handle);

    bool get_was_error() const { return mWasError; }
    ImportProgressListener* mImportProgressListener { nullptr };

private:
    friend class FLACImportFileHandle;
    FLACImportFileHandle* mFile;
    bool mWasError;
    wxArrayString mComments;
protected:
    FLAC__StreamDecoderWriteStatus write_callback(const FLAC__Frame* frame, const FLAC__int32* const buffer[]) override;
    void metadata_callback(const FLAC__StreamMetadata* metadata) override;
    void error_callback(FLAC__StreamDecoderErrorStatus status) override;
};

class FLACImportPlugin final : public ImportPlugin
{
public:
    FLACImportPlugin();
    ~FLACImportPlugin() { }

    wxString GetPluginStringID() override { return wxT("libflac"); }
    TranslatableString GetPluginFormatDescription() override;
    std::unique_ptr<ImportFileHandle> Open(
        const FilePath& Filename, AudacityProject*)  override;
};

class FLACImportFileHandle final : public ImportFileHandleEx
{
    friend class MyFLACFile;
public:
    FLACImportFileHandle(const FilePath& name);
    ~FLACImportFileHandle();

    bool Init();

    TranslatableString GetFileDescription() override;
    double GetDuration() const override;
    ByteCount GetFileUncompressedBytes() override;
    void Import(
        ImportProgressListener& progressListener, WaveTrackFactory* trackFactory, TrackHolders& outTracks, Tags* tags,
        std::optional<LibFileFormats::AcidizerTags>& outAcidTags) override;

    wxInt32 GetStreamCount() override { return 1; }

    const TranslatableStrings& GetStreamInfo() override
    {
        static TranslatableStrings empty;
        return empty;
    }

    void SetStreamUsage(wxInt32 WXUNUSED(StreamID), bool WXUNUSED(Use)) override
    {}

private:
    sampleFormat mFormat;
    std::unique_ptr<MyFLACFile> mFile;
    wxFFile mHandle;
    unsigned long mSampleRate;
    unsigned long mNumChannels;
    unsigned long mBitsPerSample;
    FLAC__uint64 mNumSamples;
    FLAC__uint64 mSamplesDone;
    bool mStreamInfoDone;
    WaveTrack::Holder mTrack;
};
