/**********************************************************************

  Audacity: A Digital Audio Editor

  @file socket_guard.h

  @author Vitaly Sverchinsky

  Part of lib-ipc library

**********************************************************************/

#pragma once

#include <cassert>
#include "ipc-types.h"

/**
 * \brief RAII-style socket wrapper. Since socket is closed on wrapper destruction,
 * initializing multiple guards with same(valid) socket descriptor is considered UB. For
 * same reason wrapper isn't copyable.
 */
class socket_guard
{
    SOCKET mSocket{ INVALID_SOCKET };
public:
    explicit socket_guard(const socket_guard&) = delete;
    socket_guard& operator=(const socket_guard&) = delete;

    explicit socket_guard(SOCKET sock = INVALID_SOCKET) noexcept
        : mSocket(sock) { }

    explicit socket_guard(socket_guard&& rhs) noexcept
        : mSocket(rhs.mSocket)
    {
        rhs.mSocket = INVALID_SOCKET;
    }

    explicit operator bool() const noexcept
    {
        return mSocket != INVALID_SOCKET;
    }

    socket_guard& operator=(socket_guard&& rhs) noexcept
    {
        if (this == &rhs) {
            return *this;
        }

        assert(mSocket == INVALID_SOCKET || rhs.mSocket != mSocket);

        std::swap(mSocket, rhs.mSocket);
        rhs.reset();

        return *this;
    }

    ///Alias for socket_guard::get
    SOCKET operator*() const noexcept { return mSocket; }

    ///@return Underlying socket descriptor
    SOCKET get() const noexcept { return mSocket; }

    ///Returns the socket descriptor and releases ownership
    SOCKET release() noexcept
    {
        SOCKET sock { INVALID_SOCKET };
        std::swap(sock, mSocket);
        return sock;
    }

    ///Closes the socket
    void reset() noexcept
    {
        if (mSocket != INVALID_SOCKET) {
            CLOSE_SOCKET(mSocket);
            mSocket = INVALID_SOCKET;
        }
    }

    ~socket_guard() noexcept
    {
        reset();
    }
};
