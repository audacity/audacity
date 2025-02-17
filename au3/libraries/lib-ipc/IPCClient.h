/**********************************************************************

  Audacity: A Digital Audio Editor

  @file IPCClient.h

  @author Vitaly Sverchinsky

  Part of lib-ipc library

**********************************************************************/

#pragma once

#include <memory>

class IPCChannelStatusCallback;

/**
 * \brief Simple TCP socket based ipc client. When created
 * attempts to connect to existing server (see IPCServer).
 */
class IPC_API IPCClient final
{
    class Impl;
    std::unique_ptr<Impl> mImpl;
public:
    /**
     * \brief Attempts to connect to a server, may fail with exception
     * or with call to IPCChannelStatusCallback::OnConnectionError.
     * Callback should be guaranteed to be alive
     * until either IPCChannelStatusCallback::OnDisconnect
     * or IPCChannelStatusCallback::OnConnectionError is called.
     * \param port TCP port number used to connect to server
     * \param callback Channel status callback. May be accessed from working threads.
     */
    IPCClient(int port, IPCChannelStatusCallback& callback);
    /**
     * \brief Closes connection if any.
     */
    ~IPCClient();
};
