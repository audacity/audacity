#ifndef EXAMPLE_ASYNC_PROCESSEVENTS_H
#define EXAMPLE_ASYNC_PROCESSEVENTS_H

#include "../../async/processevents.h" //kors

namespace app::async {
inline void processEvents()
{
    kors::async::processEvents();
}

inline void onMainThreadInvoke(const std::function<void(const std::function<void()>& /*call*/, bool /*isAlwaysQueued*/)>& f)
{
    kors::async::onMainThreadInvoke(f);
}
}

#endif // EXAMPLE_ASYNC_PROCESSEVENTS_H
