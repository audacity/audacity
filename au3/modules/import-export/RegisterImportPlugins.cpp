/*
* Audacity: A Digital Audio Editor
*/

#include "au3-import-export/Import.h"
#include "RegisterImportPlugins.h"

#include "mod-wavpack/ImportWavPack.h"
#include "mod-mpg123/ImportMP3_MPG123.h"
#include "mod-pcm/ImportPCM.h"

void RegisterImportPlugins()
{
    static Importer::RegisteredImportPlugin sWavPackImportPlugin{
        "WavPack",
        std::make_unique< WavPackImportPlugin >()
    };

    static Importer::RegisteredImportPlugin sMP3ImportPlugin{
        "MP3",
        std::make_unique<MP3ImportPlugin>()
    };

    static Importer::RegisteredImportPlugin sPCMImportPlugin{
        "PCM",
        std::make_unique< PCMImportPlugin >()
    };
}
