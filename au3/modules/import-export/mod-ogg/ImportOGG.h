/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "au3-import-export/Import.h"

#include "au3-tags/Tags.h"

#include "au3-wave-track/WaveTrack.h"
#include "au3-import-export/ImportPlugin.h"
#include "au3-import-export/ImportProgressListener.h"
#include "au3-import-export/ImportUtils.h"

#include <vorbis/vorbisfile.h>

#define DESC XO("Ogg Vorbis files")

class OggImportPlugin final : public ImportPlugin
{
public:
    OggImportPlugin();
    ~OggImportPlugin() { }

    wxString GetPluginStringID() override { return wxT("liboggvorbis"); }
    TranslatableString GetPluginFormatDescription() override;
    std::unique_ptr<ImportFileHandle> Open(
        const FilePath& Filename, AudacityProject*) override;
};

class OggImportFileHandle final : public ImportFileHandleEx
{
public:
    OggImportFileHandle(const FilePath& filename, std::unique_ptr<wxFFile>&& file, std::unique_ptr<OggVorbis_File>&& vorbisFile);
    ~OggImportFileHandle();

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
    std::unique_ptr<wxFFile> mFile;
    std::unique_ptr<OggVorbis_File> mVorbisFile;

    ArrayOf<int> mStreamUsage;
    TranslatableStrings mStreamInfo;
    std::vector<TrackListHolder> mStreams;
};
