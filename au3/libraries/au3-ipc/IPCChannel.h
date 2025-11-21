/**********************************************************************

  Audacity: A Digital Audio Editor

  @file IPCChannel.h

  @author Vitaly Sverchinsky

  Part of lib-ipc library

**********************************************************************/

#pragma once

#include <cstddef>

/**
 * \brief Interface for sending data from client to server or vice versa,
 * complemented by IPCChannelStatusCallback
 */
class IPC_API IPCChannel
{
public:
    virtual ~IPCChannel();
    /**
     * \brief Write data to the channel
     * \param bytes Pointer to the data
     * \param length Number of bytes to send
     */
    virtual void Send(const void* bytes, size_t length) = 0;
};

/**
 * \brief Interface for listening connection status changes
 */
class IPC_API IPCChannelStatusCallback
{
public:
    virtual ~IPCChannelStatusCallback();

    /**
     * \brief Called when connection attempt fails.
     */
    virtual void OnConnectionError() noexcept = 0;
    /**
     * \brief Called when connection established.
     * \param channel Using this channel client or server can send data to the other side.
     */
    virtual void OnConnect(IPCChannel& channel) noexcept = 0;
    /**
     * \brief Invalidates IPCChannel passed as argument in OnConnect.
     */
    virtual void OnDisconnect() noexcept = 0;
    /**
     * \brief Called when data chunk received as a result of a preceding call to IPCChannel::Send.
     * Generally, data pointer should not be accessed outside this method, copied if necessary.
     * \param data Pointer to the chunk
     * \param size Size of the chunk
     */
    virtual void OnDataAvailable(const void* data, size_t size) noexcept = 0;
};
