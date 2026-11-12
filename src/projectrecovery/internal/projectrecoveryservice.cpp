/*
 * Audacity: A Digital Audio Editor
 */
#include "projectrecoveryservice.h"

#include <QPromise>

#include <memory>

using namespace au::projectrecovery;

namespace {
template<typename T, typename Work>
QFuture<T> run(QThreadPool& pool, Work work)
{
    auto promise = std::make_shared<QPromise<T> >();
    QFuture<T> future = promise->future();
    pool.start([promise, work = std::move(work)]() {
        promise->start();
        promise->addResult(work());
        promise->finish();
    });
    return future;
}
}

QFuture<muse::RetVal<Inspection> > ProjectRecoveryService::inspect(const muse::io::path_t& path)
{
    return run<muse::RetVal<Inspection> >(m_pool, [path]() {
        return ProjectInspector().inspect(path);
    });
}

QFuture<muse::RetVal<RepairReport> > ProjectRecoveryService::repair(const muse::io::path_t& original, const muse::io::path_t& output)
{
    return run<muse::RetVal<RepairReport> >(m_pool, [original, output]() {
        return ProjectRepairer().repair(original, output);
    });
}

void ProjectRecoveryService::stop()
{
    m_pool.waitForDone();
}
