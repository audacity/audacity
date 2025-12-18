/*
* Audacity: A Digital Audio Editor
*/

#include "importconfiguration.h"

#include "global/settings.h"

using namespace au::importexport;

static const std::string module_name("importer");

static const muse::Settings::Key IMPORT_LABELS_DIRECTORY_PATH(module_name, "importexport/labelsDirectoryPath");

void ImportConfiguration::init()
{
    muse::settings()->setDefaultValue(IMPORT_LABELS_DIRECTORY_PATH, muse::Val(globalConfiguration()->userDataPath()));
}

muse::io::path_t ImportConfiguration::labelsDirectoryPath() const
{
    return muse::settings()->value(IMPORT_LABELS_DIRECTORY_PATH).toString();
}

void ImportConfiguration::setLabelsDirectoryPath(const muse::io::path_t& path)
{
    muse::settings()->setSharedValue(IMPORT_LABELS_DIRECTORY_PATH, muse::Val(path));
}
