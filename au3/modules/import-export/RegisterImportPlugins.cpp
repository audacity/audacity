/*
* Audacity: A Digital Audio Editor
*/

#include "au3-import-export/Import.h"
#include "RegisterImportPlugins.h"

#include "mod-wavpack/ImportWavPack.h"
#include "mod-mpg123/ImportMP3_MPG123.h"
#include "mod-pcm/ImportPCM.h"
#include "mod-ogg/ImportOGG.h"
#include "mod-flac/ImportFLAC.h"
#include "mod-opus/ImportOpus.h"
#include "mod-ffmpeg/ImportFFmpeg.h"

void RegisterImportPlugins()
{
    Importer::RegisteredImportPlugin("WavPack", std::make_unique<WavPackImportPlugin>());
    Importer::RegisteredImportPlugin("MP3", std::make_unique<MP3ImportPlugin>());
    Importer::RegisteredImportPlugin("PCM", std::make_unique<PCMImportPlugin>());
    Importer::RegisteredImportPlugin("OGG", std::make_unique<OggImportPlugin>());
    Importer::RegisteredImportPlugin("FLAC", std::make_unique<FLACImportPlugin>());
    Importer::RegisteredImportPlugin("Opus", std::make_unique<OpusImportPlugin>());
    Importer::RegisteredImportPlugin("FFmpeg", std::make_unique<FFmpegImportPlugin>());
}
