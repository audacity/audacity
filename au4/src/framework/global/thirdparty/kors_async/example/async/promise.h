#ifndef EXAMPLE_ASYNC_PROMISE_H
#define EXAMPLE_ASYNC_PROMISE_H

#include "../../async/promise.h" // kors

namespace app::async {
template<typename ... T>
using Promise = kors::async::Promise<T...>;
}

#endif // EXAMPLE_ASYNC_PROMISE_H
