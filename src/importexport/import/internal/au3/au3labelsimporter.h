/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "../../ilabelsimporter.h"

namespace au::importexport {
class Au3LabelsImporter : public ILabelsImporter, public muse::Injectable
{
    muse::Inject<context::IGlobalContext> globalContext = { this };

public:
    Au3LabelsImporter() = default;

    muse::Ret importData(const muse::io::path_t& filePath) override;

    std::vector<std::string> fileFilter() override;
};
}

