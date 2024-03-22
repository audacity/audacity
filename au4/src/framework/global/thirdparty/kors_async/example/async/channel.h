#ifndef EXAMPLE_ASYNC_CHANNEL_H
#define EXAMPLE_ASYNC_CHANNEL_H

#include "../../async/channel.h" // kors

namespace app::async {
template<typename ... T>
using Channel = kors::async::Channel<T...>;
}

#endif // EXAMPLE_ASYNC_CHANNEL_H
