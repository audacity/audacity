/*
* Audacity: A Digital Audio Editor
*/

#include "labelsconfiguration.h"

#include "global/settings.h"

using namespace au::importexport;

static const std::string module_name("iex_labels");

static const muse::Settings::Key LABELS_DIRECTORY_PATH(module_name, "importexport/labelsDirectoryPath");

void LabelsConfiguration::init()
{
    muse::settings()->setDefaultValue(LABELS_DIRECTORY_PATH, muse::Val(globalConfiguration()->userDataPath()));
}

muse::io::path_t LabelsConfiguration::labelsDirectoryPath() const
{
    return muse::settings()->value(LABELS_DIRECTORY_PATH).toString();
}

void LabelsConfiguration::setLabelsDirectoryPath(const muse::io::path_t& path)
{
    muse::settings()->setSharedValue(LABELS_DIRECTORY_PATH, muse::Val(path));
}
