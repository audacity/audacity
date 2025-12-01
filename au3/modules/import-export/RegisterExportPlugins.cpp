/*
* Audacity: A Digital Audio Editor
*/

#include "au3-import-export/ExportPluginRegistry.h"
#include "mod-mp3/ExportMP3.h"
#include "mod-ffmpeg/ExportFFmpeg.h"
#include "mod-wavpack/ExportWavPack.h"
#include "mod-pcm/ExportPCM.h"

#include "RegisterExportPlugins.h"

void RegisterExportPlugins()
{
    static ExportPluginRegistry::RegisteredPlugin sMP3Plugin{
        "MP3",
        [] { return std::make_unique<ExportMP3>(); }
    };

    static ExportPluginRegistry::RegisteredPlugin sFFmpegPlugin{
        "FFmpeg",
        [] { return std::make_unique<ExportFFmpeg>(); }
    };

    static ExportPluginRegistry::RegisteredPlugin sWavPackPlugin{
        "WavPack",
        []{ return std::make_unique< ExportWavPack >(); }
    };

    static ExportPluginRegistry::RegisteredPlugin sPCMPlugin{
        "PCM",
        []{ return std::make_unique< ExportPCM >(); }
    };
}
