/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "global/iglobalconfiguration.h"

#include "../iimportconfiguration.h"

namespace au::importexport {
class ImportConfiguration : public IImportConfiguration
{
    muse::Inject<muse::IGlobalConfiguration> globalConfiguration;

public:
    ImportConfiguration() = default;

    void init();

    muse::io::path_t labelsDirectoryPath() const override;
    void setLabelsDirectoryPath(const muse::io::path_t& path) override;

private:
};

}

