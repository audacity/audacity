/**********************************************************************

  Audacity: A Digital Audio Editor

  @file IPCClient.cpp

  @author Vitaly Sverchinsky

  Part of lib-ipc library

**********************************************************************/

#include "IPCClient.h"
#include "IPCChannel.h"

#include <cstdint>
#include <thread>
#include <stdexcept>

#include "internal/ipc-types.h"
#include "internal/socket_guard.h"
#include "internal/BufferedIPCChannel.h"

class IPCClient::Impl final
{
    std::unique_ptr<BufferedIPCChannel> mChannel;
public:

    Impl(int port, IPCChannelStatusCallback& callback)
    {
        auto fd = socket_guard { socket(AF_INET, SOCK_STREAM, IPPROTO_TCP) };
        if (!fd) {
            throw std::runtime_error("cannot create socket");
        }

#if defined(__unix__) || defined(__APPLE__)
        auto fdFlags = fcntl(*fd, F_GETFD, 0);
        if (fdFlags != -1) {
            fcntl(*fd, F_SETFD, fdFlags | FD_CLOEXEC);
        }
#endif

        sockaddr_in addrin {};
        addrin.sin_family = AF_INET;
        addrin.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        addrin.sin_port = htons(static_cast<uint16_t>(port));

        if (connect(*fd, reinterpret_cast<const sockaddr*>(&addrin), sizeof(addrin)) == SOCKET_ERROR) {
            callback.OnConnectionError();
            return;
        }

        mChannel = std::make_unique<BufferedIPCChannel>();
        mChannel->StartConversation(fd.release(), callback);
    }
};

IPCClient::IPCClient(int port, IPCChannelStatusCallback& callback)
{
#ifdef _WIN32
    WSADATA wsaData;
    auto result = WSAStartup(MAKEWORD(2, 2), &wsaData);
    if (result != NO_ERROR) {
        throw std::runtime_error("WSAStartup failed");
    }
#endif
    mImpl = std::make_unique<Impl>(port, callback);
}

IPCClient::~IPCClient() = default;
