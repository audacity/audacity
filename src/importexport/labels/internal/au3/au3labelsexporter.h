/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "../../ilabelsexporter.h"

namespace au::importexport {
class Au3LabelsExporter : public ILabelsExporter, public muse::Injectable
{
    muse::Inject<context::IGlobalContext> globalContext = { this };

public:
    Au3LabelsExporter(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

    muse::Ret exportData(const muse::io::path_t& filePath, const trackedit::TrackIdList& includedLabelTracksIds = {}) override;
};
}
