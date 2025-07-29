/*
* Audacity: A Digital Audio Editor
*/

#include <mpg123.h>

#include <wx/file.h>

#include "WaveTrack.h"
#include "libraries/lib-import-export/ImportPlugin.h"

const auto exts = { wxT("mp3"), wxT("mp2"), wxT("mpa") };

class MP3ImportPlugin final : public ImportPlugin
{
public:
    MP3ImportPlugin();

    wxString GetPluginStringID() override;

    TranslatableString GetPluginFormatDescription() override;

    std::unique_ptr<ImportFileHandle> Open(const FilePath& Filename, AudacityProject*) override;
};

class MP3ImportFileHandle final : public ImportFileHandleEx
{
public:
    MP3ImportFileHandle(const FilePath& filename);
    ~MP3ImportFileHandle();

    TranslatableString GetFileDescription() override;
    ByteCount GetFileUncompressedBytes() override;
    void Import(
        ImportProgressListener& progressListener, WaveTrackFactory* trackFactory, TrackHolders& outTracks, Tags* tags,
        std::optional<LibFileFormats::AcidizerTags>& outAcidTags) override;

    bool SetupOutputFormat();

    void ReadTags(Tags* tags);

    wxInt32 GetStreamCount() override;
    const TranslatableStrings& GetStreamInfo() override;
    void SetStreamUsage(wxInt32 StreamID, bool Use) override;

private:
    bool Open();

private:
    static ptrdiff_t ReadCallback(void* handle, void* buffer, size_t size);
    static off_t SeekCallback(void* handle, off_t offset, int whence);

    wxFile mFile;
    wxFileOffset mFileLen { 0 };

    WaveTrackFactory* mTrackFactory { nullptr };
    WaveTrack::Holder mTrack;
    unsigned mNumChannels { 0 };

    mpg123_handle* mHandle { nullptr };

    bool mFloat64Output {};

    friend MP3ImportPlugin;
};
