/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <libraries/lib-import-export/ImportPlugin.h>
#include "libraries/lib-file-formats/FileFormats.h"

class PCMImportPlugin final : public ImportPlugin
{
public:
    PCMImportPlugin();
    ~PCMImportPlugin();

    wxString GetPluginStringID() override;
    TranslatableString GetPluginFormatDescription() override;
    std::unique_ptr<ImportFileHandle> Open(
        const FilePath& Filename, AudacityProject*) override;
};

class PCMImportFileHandle final : public ImportFileHandleEx
{
public:
    PCMImportFileHandle(const FilePath& name, SFFile&& file, SF_INFO info);
    ~PCMImportFileHandle();

    TranslatableString GetFileDescription() override;
    ByteCount GetFileUncompressedBytes() override;
    void Import(
        ImportProgressListener& progressListener, WaveTrackFactory* trackFactory, TrackHolders& outTracks, Tags* tags,
        std::optional<LibFileFormats::AcidizerTags>& outAcidTags) override;

    wxInt32 GetStreamCount() override;

    const TranslatableStrings& GetStreamInfo() override;

    void SetStreamUsage(wxInt32 WXUNUSED(StreamID), bool WXUNUSED(Use)) override;

private:
    SFFile mFile;
    const SF_INFO mInfo;
    sampleFormat mEffectiveFormat;
    sampleFormat mFormat;
};
