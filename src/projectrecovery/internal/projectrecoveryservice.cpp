/*
 * Audacity: A Digital Audio Editor
 */
#include "projectrecoveryservice.h"

using namespace au::projectrecovery;

namespace {
template<typename T, typename Work>
muse::async::Promise<T> run(QThreadPool& pool, Work work)
{
    // The job starts from the event loop, after the caller has subscribed to its result
    return muse::async::Promise<T>([&pool, work](typename muse::async::Promise<T>::Resolve resolve) {
        pool.start([resolve, work]() {
            (void)resolve(work());
        });
        return muse::async::Promise<T>::dummy_result();
    }, muse::async::PromiseType::AsyncByPromise);
}
}

muse::async::Promise<muse::RetVal<Inspection> > ProjectRecoveryService::inspect(const muse::io::path_t& path)
{
    return run<muse::RetVal<Inspection> >(m_pool, [path]() {
        return ProjectInspector().inspect(path);
    });
}

muse::async::Promise<muse::RetVal<RepairReport> > ProjectRecoveryService::repair(const muse::io::path_t& original,
                                                                                 const muse::io::path_t& output)
{
    return run<muse::RetVal<RepairReport> >(m_pool, [original, output]() {
        return ProjectRepairer().repair(original, output);
    });
}

void ProjectRecoveryService::stop()
{
    m_pool.waitForDone();
}
