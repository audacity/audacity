/*
* Audacity: A Digital Audio Editor
*/

#include "modules/import-export/mod-mp3/ExportMP3.h"
#include "ExportFFmpeg.h"
#include "libraries/lib-import-export/ExportPluginRegistry.h"

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
}
