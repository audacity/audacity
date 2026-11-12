/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QFuture>

#include "modularity/imoduleinterface.h"

#include "framework/global/io/path.h"
#include "framework/global/types/retval.h"

#include "internal/projectinspector.h"
#include "internal/projectrepairer.h"

namespace au::projectrecovery {
//! Runs inspections and repairs off the main thread
class IProjectRecoveryService : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IProjectRecoveryService)

public:
    virtual ~IProjectRecoveryService() = default;

    virtual QFuture<muse::RetVal<Inspection> > inspect(const muse::io::path_t& path) = 0;
    virtual QFuture<muse::RetVal<RepairReport> > repair(const muse::io::path_t& original, const muse::io::path_t& output) = 0;
};
}
