/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "global/iglobalconfiguration.h"

#include "../ilabelsconfiguration.h"

namespace au::importexport {
class LabelsConfiguration : public ILabelsConfiguration
{
    muse::Inject<muse::IGlobalConfiguration> globalConfiguration;

public:
    LabelsConfiguration() = default;

    void init();

    muse::io::path_t labelsDirectoryPath() const override;
    void setLabelsDirectoryPath(const muse::io::path_t& path) override;
};
}
