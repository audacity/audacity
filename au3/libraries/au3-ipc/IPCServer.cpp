/**********************************************************************

  Audacity: A Digital Audio Editor

  @file IPCServer.cpp

  @author Vitaly Sverchinsky

  Part of lib-ipc library

**********************************************************************/

#include "IPCServer.h"
#include "IPCChannel.h"

#include <thread>
#include <mutex>
#include <stdexcept>

#include "internal/ipc-types.h"
#include "internal/socket_guard.h"
#include "internal/BufferedIPCChannel.h"

class IPCServer::Impl
{
    bool mTryConnect{ true };
    std::mutex mSync;
    std::unique_ptr<BufferedIPCChannel> mChannel;
    std::unique_ptr<std::thread> mConnectionRoutine;
    int mConnectPort{ 0 };

    socket_guard mListenSocket;
public:

    Impl(IPCChannelStatusCallback& callback)
    {
        mListenSocket = socket_guard { socket(AF_INET, SOCK_STREAM, IPPROTO_TCP) };
        if (!mListenSocket) {
            throw std::runtime_error("cannot create socket");
        }

        sockaddr_in addrhint{};
        addrhint.sin_family = AF_INET;
        addrhint.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        addrhint.sin_port = htons(INADDR_ANY);

        static const int yes { 1 };
        if (setsockopt(*mListenSocket, SOL_SOCKET, SO_REUSEADDR, reinterpret_cast<const char*>(&yes), sizeof(yes)) == SOCKET_ERROR) {
            throw std::runtime_error("cannot configure listen socket");
        }

        if (bind(*mListenSocket, reinterpret_cast<const sockaddr*>(&addrhint), sizeof(addrhint)) == SOCKET_ERROR) {
            throw std::runtime_error("socket bind error");
        }

        if (listen(*mListenSocket, 1) == SOCKET_ERROR) {
            throw std::runtime_error("socket listen error");
        }

        sockaddr_in addr{};
        socklen_t addr_len { sizeof(addr) };
        if (getsockname(*mListenSocket, reinterpret_cast<sockaddr*>(&addr), &addr_len) == SOCKET_ERROR) {
            throw std::runtime_error("failed to get socket name");
        }

        mConnectPort = ntohs(addr.sin_port);

        mChannel = std::make_unique<BufferedIPCChannel>();
        mConnectionRoutine = std::make_unique<std::thread>([this, &callback]
        {
            socket_guard connfd;

            while (true)
            {
                {
                    //combine flag check and internal state initialization within a single lock...

                    std::lock_guard lck { mSync };
                    if (!mTryConnect) {
                        return;
                    }

                    if (connfd) {
                        mListenSocket.reset();//do not need that any more
                        try
                        {
                            mChannel->StartConversation(connfd.release(), callback);
                        }
                        catch (...)
                        {
                            callback.OnConnectionError();
                        }
                        //initialization finished, we can leave now
                        break;
                    }
                }

                fd_set readfds, exceptfds;
                FD_ZERO(&readfds);
                FD_ZERO(&exceptfds);
                FD_SET(*mListenSocket, &readfds);
                FD_SET(*mListenSocket, &exceptfds);

                auto ret = select(NFDS(*mListenSocket), &readfds, nullptr, &exceptfds, nullptr);
                if (ret == 1) {
                    connfd = socket_guard { accept(*mListenSocket, nullptr, nullptr) };
                    if (!connfd) {
                        callback.OnConnectionError();
                        break;
                    }
                    //connection created, finish initialization during next loop iteration under guarded section
                } else {//SOCKET_ERROR
                    callback.OnConnectionError();
                    break;
                }
            }
        });
    }

    int GetConnectPort() const noexcept { return mConnectPort; }

    ~Impl()
    {
        {
            std::lock_guard lck{ mSync };
            mTryConnect = false;
            //this will also interrupt select in connection thread
            mListenSocket.reset();
            mChannel.reset();
        }
        if (mConnectionRoutine) {
            mConnectionRoutine->join();
        }
    }
};

IPCServer::IPCServer(IPCChannelStatusCallback& callback)
{
#ifdef _WIN32
    WSADATA wsaData;
    auto result = WSAStartup(MAKEWORD(2, 2), &wsaData);
    if (result != NO_ERROR) {
        throw std::runtime_error("WSAStartup failed");
    }
#endif
    mImpl = std::make_unique<Impl>(callback);
}

IPCServer::~IPCServer() = default;

int IPCServer::GetConnectPort() const noexcept
{
    return mImpl->GetConnectPort();
}
