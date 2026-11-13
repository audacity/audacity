/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QThreadPool>

#include "../iprojectrecoveryservice.h"

namespace au::projectrecovery {
class ProjectRecoveryService : public IProjectRecoveryService
{
public:
    QFuture<muse::RetVal<Inspection> > inspect(const muse::io::path_t& path) override;
    QFuture<muse::RetVal<RepairReport> > repair(const muse::io::path_t& original, const muse::io::path_t& output) override;

    //! Waits for the jobs that run
    void stop();

private:
    QThreadPool m_pool;
};
}
