/**********************************************************************

  Audacity: A Digital Audio Editor

  @file AsyncPluginValidation.cpp

  @author Vitaly Sverchinsky

  Part of lib-module-manager library

**********************************************************************/

#include "AsyncPluginValidator.h"

#include <optional>
#include <mutex>

#include "BasicUI.h"
#include "IPCChannel.h"
#include "IPCServer.h"
#include "spinlock.h"
#include "XMLTagHandler.h"

#include "PluginIPCUtils.h"
#include "PluginDescriptor.h"
#include "PluginHost.h"
#include "XMLFileReader.h"

AsyncPluginValidator::Delegate::~Delegate() = default;

class AsyncPluginValidator::Impl final : public IPCChannelStatusCallback, public std::enable_shared_from_this<Impl>
{
    //Variables below are accessed from any thread

    IPCChannel* mChannel{ nullptr };
    std::optional<wxString> mRequest;
    std::atomic<std::chrono::system_clock::duration::rep> mLastTimeActive;

    //The main reason to use spinlock instead of std::mutex here
    //is that in most cases there will be no attempts for simultaneous
    //data access (except OnConnect/Disconnect cases), because of
    //protocol design
    spinlock mSync;

    //Variable below is accessed only from the main/UI thread

    Delegate* mDelegate{ nullptr };
    std::unique_ptr<IPCServer> mServer;

    //Variables below is accessed only from worker threads

    detail::InputMessageReader mMessageReader;

    //called on main thread only!
    void StartHost()
    {
        auto server = std::make_unique<IPCServer>(*this);
        if (!PluginHost::Start(server->GetConnectPort())) {
            throw std::runtime_error("cannot start plugin host process");
        }
        mLastTimeActive = std::chrono::system_clock::now().time_since_epoch().count();
        mServer = std::move(server);
    }

    void HandleInternalError(const wxString& msg) noexcept
    {
        try
        {
            BasicUI::CallAfter([wptr = weak_from_this(), msg]
            {
                if (auto self = wptr.lock(); self && self->mDelegate != nullptr) {
                    self->mDelegate->OnInternalError(msg);
                }
            });
        }
        catch (...)
        {
            //no way to report about a problem, though shouldn't be the case...
        }
    }

    void HandleResult(detail::PluginValidationResult&& result) noexcept
    {
        try
        {
            BasicUI::CallAfter([wptr = weak_from_this(), result = detail::PluginValidationResult { result }]
            {
                if (auto self = wptr.lock()) {
                    if (self->mDelegate == nullptr) {
                        return;
                    }

                    //Release the current value to keep state invariant
                    //Caller is free to use Validate now
                    std::optional<wxString> request;
                    {
                        std::lock_guard lck_sync(self->mSync);
                        self->mRequest.swap(request);
                    }

                    if (!request.has_value()) {
                        //Invalid state error
                        self->mDelegate->OnInternalError(result.GetErrorMessage());
                        return;
                    }

                    if (result.IsValid()) {
                        for (auto& desc : result.GetDescriptors()) {
                            self->mDelegate->OnPluginFound(PluginDescriptor { desc });
                        }
                    } else {
                        wxString providerId;
                        wxString pluginPath;
                        detail::ParseRequestString(*request, providerId, pluginPath);

                        self->mDelegate->OnPluginValidationFailed(providerId, pluginPath);
                    }
                    self->mDelegate->OnValidationFinished();
                }
            });
        }
        catch (...)
        {
            ///no way to report about a problem, though shouldn't be the case...
        }
    }

public:

    Impl(Impl&) = delete;
    Impl(Impl&&) = delete;
    Impl& operator=(Impl&) = delete;
    Impl& operator=(Impl&&) = delete;

    Impl(Delegate& delegate)
        : mDelegate(&delegate) { }

    ~Impl() override
    {
        //important to reset delegate before IPCChannelStatusCallback::OnDisconnect
        //is called by server
        mDelegate = nullptr;
        mServer.reset();
    }

    void SetDelegate(Delegate* delegate)
    {
        mDelegate = delegate;
    }

    std::chrono::system_clock::time_point InactiveSince() const noexcept
    {
        using std::chrono::system_clock;

        return system_clock::time_point { system_clock::duration{ mLastTimeActive.load() } };
    }

    void OnConnect(IPCChannel& channel) noexcept override
    {
        std::lock_guard lck(mSync);

        mChannel = &channel;
        if (mRequest) {
            try
            {
                detail::PutMessage(channel, *mRequest);
            }
            catch (...)
            {
                HandleInternalError("Can't send message to host");
            }
        }
    }

    void OnDisconnect() noexcept override
    {
        {
            std::lock_guard lck(mSync);
            mChannel = nullptr;
        }
        detail::PluginValidationResult result;
        result.SetError("Disconnect");
        HandleResult(std::move(result));
    }

    void OnConnectionError() noexcept override
    {
        HandleInternalError("Can't connect");
    }

    void OnDataAvailable(const void* data, size_t size) noexcept override
    {
        try
        {
            mMessageReader.ConsumeBytes(data, size);
            mLastTimeActive = std::chrono::system_clock::now().time_since_epoch().count();
            while (mMessageReader.CanPop())
            {
                auto message = mMessageReader.Pop();
                if (message.IsEmpty()) {
                    continue;
                }

                detail::PluginValidationResult result;
                XMLFileReader xmlReader;
                xmlReader.ParseString(&result, message);

                HandleResult(std::move(result));
            }
        }
        catch (...)
        {
            HandleInternalError("Can't process response from the host");
        }
    }

    void Validate(const wxString& providerId, const wxString& pluginPath)
    {
        std::lock_guard lck(mSync);

        //one request at a time
        assert(!mRequest.has_value());

        mRequest = detail::MakeRequestString(providerId, pluginPath);
        if (mChannel) {
            detail::PutMessage(*mChannel, *mRequest);
        } else {
            //create host process on demand
            StartHost();
        }
    }
};

AsyncPluginValidator::AsyncPluginValidator(Delegate& delegate)
{
    mImpl = std::make_unique<Impl>(delegate);
}

AsyncPluginValidator::~AsyncPluginValidator() = default;

void AsyncPluginValidator::Validate(const wxString& providerId, const wxString& pluginPath)
{
    mImpl->Validate(providerId, pluginPath);
}

void AsyncPluginValidator::SetDelegate(Delegate* delegate)
{
    mImpl->SetDelegate(delegate);
}

std::chrono::system_clock::time_point AsyncPluginValidator::InactiveSince() const noexcept
{
    return mImpl->InactiveSince();
}
