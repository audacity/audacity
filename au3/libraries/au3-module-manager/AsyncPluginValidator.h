/**********************************************************************

  Audacity: A Digital Audio Editor

  @file AsyncPluginValidator.h

  @author Vitaly Sverchinsky

  Part of lib-module-manager library

**********************************************************************/

#pragma once

#include <chrono>
#include <memory>

#include <wx/string.h>

class PluginDescriptor;

/**
 * \brief Starts and communicates with a dedicated process to perform
 * plugin validation. Once instantiated, client should call
 * AsyncPluginValidation::Validate for plugin that needs to be validated.
 * When done, AsyncPluginValidation will notify caller via Delegate on the
 * UI thread (requires event loop). After Delegate::OnValidationFinished
 * is called procedure can be repeated with another plugin id.
 */
class MODULE_MANAGER_API AsyncPluginValidator final
{
    class Impl;
    std::shared_ptr<Impl> mImpl;
public:
    /**
     * \brief Used to talk back to calling side
     */
    class MODULE_MANAGER_API Delegate
    {
    public:
        virtual ~Delegate();

        ///Called for each plugin instance found inside module
        virtual void OnPluginFound(const PluginDescriptor& plugin) = 0;
        virtual void OnPluginValidationFailed(const wxString& providerId, const wxString& path) = 0;
        ///Called when module processing finished
        virtual void OnValidationFinished() = 0;
        ///Called on error, further processing is not possible.
        virtual void OnInternalError(const wxString& msg) = 0;
    };

    AsyncPluginValidator(AsyncPluginValidator&) = delete;
    AsyncPluginValidator(AsyncPluginValidator&&) = delete;
    AsyncPluginValidator& operator=(AsyncPluginValidator&) = delete;
    AsyncPluginValidator& operator=(AsyncPluginValidator&&) = delete;

    explicit AsyncPluginValidator(Delegate& delegate);
    ~AsyncPluginValidator();

    void SetDelegate(Delegate* delegate);

    std::chrono::system_clock::time_point InactiveSince() const noexcept;

    /**
     * \brief Each call to Validate should result in appropriate call
     * OnValidationFinished, until then it's not allowed to call this
     * method again. May fail with exception.
     * \param providerId ID of the provider that should be used for validation
     * \param pluginPath path to the plugin module
     */
    void Validate(const wxString& providerId, const wxString& pluginPath);
};
