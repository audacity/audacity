/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PluginHost.h

  @author Vitaly Sverchinsky

  Part of lib-module-manager library

**********************************************************************/

#pragma once

#include "IPCChannel.h"
#include <memory>
#include <mutex>
#include <optional>
#include <condition_variable>
#include <wx/string.h>

#include "IPCClient.h"
#include "PluginIPCUtils.h"

/**
 * \brief Internal class, processes plugin validation requests
 * from the main app. Request is a simple string formatted by
 * detail::MakeRequestString. After connection is established
 * host starts to wait for a request from server. Once request
 * is successfully processed host sends a reply. Host is capable
 * to handle only one request at a time, so it's not allowed to
 * send another request until host hasn't finish processing
 * previous request.
 */
class MODULE_MANAGER_API PluginHost final : public IPCChannelStatusCallback
{
    static constexpr auto HostArgument =  "--host";

    std::unique_ptr<IPCClient> mClient;
    IPCChannel* mChannel{ nullptr };
    detail::InputMessageReader mInputMessageReader;
    std::mutex mSync;
    std::condition_variable mRequestCondition;
    std::optional<wxString> mRequest;

    bool mRunning{ true };

    void Stop() noexcept;

public:
    /**
     * \brief Attempts to start a host application (should be called from
     * the main application)
     * \return true if host has started successfully
     */
    static bool Start(int connectPort);

    ///Returns true if current process is considered to be a plugin host process
    static bool IsHostProcess();

    explicit PluginHost(int connectPort);

    void OnConnect(IPCChannel& channel) noexcept override;
    void OnDisconnect() noexcept override;
    void OnConnectionError() noexcept override;
    void OnDataAvailable(const void* data, size_t size) noexcept override;

    bool Serve();
};
