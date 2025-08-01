/*
* Audacity: A Digital Audio Editor
*/

#include "libraries/lib-import-export/ExportPluginRegistry.h"
#include "modules/import-export/mod-mp3/ExportMP3.h"
#include "modules/import-export/mod-ffmpeg/ExportFFmpeg.h"
#include "modules/import-export/mod-wavpack/ExportWavPack.h"
#include "modules/import-export/mod-pcm/ExportPCM.h"

#include "RegisterExportPlugins.h"

void RegisterExportPlugins()
{
    static ExportPluginRegistry::RegisteredPlugin sMP3Plugin{
        "MP3",
        [] { return std::make_unique<ExportMP3>(); }
    };

#ifdef AU_USE_FFMPEG
    static ExportPluginRegistry::RegisteredPlugin sFFmpegPlugin{
        "FFmpeg",
        [] { return std::make_unique<ExportFFmpeg>(); }
    };
#endif

    static ExportPluginRegistry::RegisteredPlugin sWavPackPlugin{
        "WavPack",
        []{ return std::make_unique< ExportWavPack >(); }
    };

    static ExportPluginRegistry::RegisteredPlugin sPCMPlugin{
        "PCM",
        []{ return std::make_unique< ExportPCM >(); }
    };
}
