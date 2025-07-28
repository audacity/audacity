/*
* Audacity: A Digital Audio Editor
*/

#include "libraries/lib-import-export/Import.h"
#include "RegisterImportPlugins.h"

#include "modules/import-export/mod-wavpack/ImportWavPack.h"

void RegisterImportPlugins()
{
    static Importer::RegisteredImportPlugin sWavPackImportPlugin{
        "WavPack",
        std::make_unique< WavPackImportPlugin >()
    };
}
