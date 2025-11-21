/**********************************************************************

  Audacity: A Digital Audio Editor

  @file BufferedIPCChannel.h

  @author Vitaly Sverchinsky

  Part of lib-ipc library

**********************************************************************/

#pragma once

#include <mutex>
#include <condition_variable>
#include <memory>
#include <vector>
#include <thread>

#include "ipc-types.h"
#include "IPCChannel.h"

class IPCChannelStatusCallback;

/**
 * \brief Socket-based implementation of IPCChannel that uses intermediate
 * buffer for data exchange between client and server.
 */
class BufferedIPCChannel final : public IPCChannel
{
    static constexpr int DefaultOutputBufferCapacity  { 2048 };
    static constexpr int DefaultInputBufferSize { 2048 };

    bool mAlive { true };
    std::mutex mSocketSync;
    std::condition_variable mSendCondition;

    std::unique_ptr<std::thread> mRecvRoutine;
    std::unique_ptr<std::thread> mSendRoutine;

    SOCKET mSocket { INVALID_SOCKET };

    std::vector<char> mOutputBuffer;

public:

    BufferedIPCChannel();

    /**
     * \brief Destroys channel and stops any data exchange
     */
    ~BufferedIPCChannel() override;

    /**
     * \brief Thread-safe
     */
    void Send(const void* bytes, size_t length) override;

    /**
     * \brief Allowed to be called only once during object lifetime.
     * Takes ownership over a socket. Callback should be guaranteed to be alive
     * between IPCChannelStatusCallback::OnConnect and IPCChannelStatusCallback::OnDisconnect,
     * and will be accessed from multiple threads.
     * \param socket A valid socket on which data exchange should happen
     * \param callback Used to send status updates
     */
    void StartConversation(SOCKET socket, IPCChannelStatusCallback& callback);
};
