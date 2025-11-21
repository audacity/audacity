/**********************************************************************

  Audacity: A Digital Audio Editor

  @file BufferedIPCChannel.cpp

  @author Vitaly Sverchinsky

  Part of lib-ipc library

**********************************************************************/

#include "BufferedIPCChannel.h"

#include <cassert>
#include <cstring>
#include <mutex>
#include "MemoryX.h"
#include "socket_guard.h"

BufferedIPCChannel::BufferedIPCChannel()
{
    mOutputBuffer.reserve(DefaultOutputBufferCapacity);
}

BufferedIPCChannel::~BufferedIPCChannel()
{
    if (mSocket != INVALID_SOCKET) {
        //Shut down connection and wake up select
        //No need to check possible error codes set by this call
#ifdef _WIN32
        shutdown(mSocket, SD_BOTH);
#else
        shutdown(mSocket, SHUT_RDWR);
#endif
        //Make sure all socket IO operations complete before close
        if (mSendRoutine) {
            mSendRoutine->join();
        }
        if (mRecvRoutine) {
            mRecvRoutine->join();
        }

        CLOSE_SOCKET(mSocket);
    }
}

void BufferedIPCChannel::Send(const void* bytes, size_t length)
{
    assert(length > 0);
    if (length == 0) {
        return;
    }

    {
        std::lock_guard lck(mSocketSync);

        auto offset = mOutputBuffer.size();
        mOutputBuffer.resize(offset + length);
        std::memcpy(mOutputBuffer.data() + offset, bytes, length);
    }
    mSendCondition.notify_one();
}

void BufferedIPCChannel::StartConversation(SOCKET socket, IPCChannelStatusCallback& callback)
{
    assert(socket != INVALID_SOCKET);
    assert(mSocket == INVALID_SOCKET && !mSendRoutine && !mRecvRoutine);
    mSocket = socket;

    //create "sending" thread first, it should be blocked
    //on condition until both IPCChannelStatusCallback::OnConnect
    //and IPCChannel::Send are called (in that sequence)
    mSendRoutine = std::make_unique<std::thread>([this]
    {
        std::vector<char> secondaryOutputBuffer;
        secondaryOutputBuffer.reserve(DefaultOutputBufferCapacity);

        while (true)
        {
            std::unique_lock lck(mSocketSync);
            mSendCondition.wait(lck, [this]{ return !mAlive || !mOutputBuffer.empty(); });

            if (!mAlive) {
                return;
            }

            std::swap(secondaryOutputBuffer, mOutputBuffer);
            mOutputBuffer.clear();

            lck.unlock();

            {
                int offset = 0;
                while (offset < static_cast<int>(secondaryOutputBuffer.size()))
                {
                    fd_set writefds, exceptfds;
                    FD_ZERO(&writefds);
                    FD_ZERO(&exceptfds);
                    FD_SET(mSocket, &writefds);
                    FD_SET(mSocket, &exceptfds);
                    auto ret = select(NFDS(mSocket), nullptr, &writefds, &exceptfds, nullptr);
                    if (ret == 1) {
                        //try to send data...
                        ret = send(
                            mSocket,
                            secondaryOutputBuffer.data() + offset,
                            static_cast<int>(secondaryOutputBuffer.size()) - offset,
                            0);
                        if (ret > 0) {
                            offset += ret;
                        } else if (ret == SOCKET_ERROR) {
                            break;//error
                        }
                    } else {
                        break;//error
                    }
                }
            }
        }
    });

    mRecvRoutine = std::make_unique<std::thread>([this, &callback]{
        //such order guarantees that IPCStatusCallback::OnConnect will be called
        //only if both routines have started successfully
        callback.OnConnect(*this);

        auto terminate = finally([this, &callback]
        {
            {
                //Let "sending" thread know that we're done
                std::lock_guard lck(mSocketSync);
                mAlive = false;
            }
            mSendCondition.notify_one();

            //It may happen so, that "sending" thread sends some data while
            //"reading" thread notifies callback about disconnection. It's
            //not a big deal since a) sending may fail b) from the user code
            //perspective IPCChannel::Send was called before it receives OnDisconnect
            callback.OnDisconnect();
        });

        std::vector<char> buffer(DefaultInputBufferSize);
        while (true)
        {
            fd_set readfds, exceptfds;
            FD_ZERO(&readfds);
            FD_ZERO(&exceptfds);
            FD_SET(mSocket, &readfds);
            FD_SET(mSocket, &exceptfds);

            auto ret = select(NFDS(mSocket), &readfds, nullptr, &exceptfds, nullptr);
            if (ret == 1) {
                //try fetch data...
                ret = recv(
                    mSocket,
                    buffer.data(),
                    static_cast<int>(buffer.size()),
                    0);
                if (ret > 0) {
                    //success
                    callback.OnDataAvailable(buffer.data(), ret);
                } else if (ret == SOCKET_ERROR) {
#ifdef _WIN32
                    auto err = WSAGetLastError();
                    if (err != WSAEWOULDBLOCK && err != EAGAIN) {
                        break;
                    }
#else
                    if (errno != EWOULDBLOCK && errno != EAGAIN) {
                        break;
                    }
#endif
                } else {
                    break;//closed by remote(0)
                }
            } else {//SOCKET_ERROR
                break;
            }
        }
    });
}
