/**********************************************************************

  Audacity: A Digital Audio Editor

  @file IPCServer.h

  @author Vitaly Sverchinsky

  Part of lib-ipc library

**********************************************************************/

#pragma once

#include <memory>

class IPCChannel;
class IPCChannelStatusCallback;

/**
 * \brief Simple TCP socket based ipc server. When created
 * server starts to listen for incoming connection (see IPCClient).
 */
class IPC_API IPCServer final
{
    class Impl;
    std::unique_ptr<Impl> mImpl;
public:
    /**
     * \brief May fail with exception or with call to
     * IPCChannelStatusCallback::OnConnectionError.
     * Callback should be guaranteed to be alive
     * until either IPCChannelStatusCallback::OnDisconnect
     * or IPCChannelStatusCallback::OnConnectionError is called.
     * \param callback Channel status callback. May be accessed from working threads.
     */
    IPCServer(IPCChannelStatusCallback& callback);
    /**
     * \brief Closes connection if any.
     */
    ~IPCServer();

    ///Returns port number to connect to.
    ///Valid until connection is established.
    int GetConnectPort() const noexcept;
};
