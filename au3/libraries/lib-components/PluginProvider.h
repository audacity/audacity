/*!*******************************************************************

   Audacity: A Digital Audio Editor

   @file PluginProvider.h
   @brief Generalized interface for discovery of plug-ins for one protocol

   Leland Lucius

   Copyright (c) 2014, Audacity Team
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

   3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
   COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
   BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE.

**********************************************************************/

#ifndef __AUDACITY_PLUGINPROVIDER_H__
#define __AUDACITY_PLUGINPROVIDER_H__

#include <functional>
#include <memory>
#include "Identifier.h"
#include "ComponentInterface.h"
#include "ComponentInterfaceSymbol.h"

using PluginID = wxString;
using PluginIDs = wxArrayString;
// Incomplete type not defined in libraries -- TODO clean that up:
class PluginManagerInterface;

// ============================================================================
//
// Don't even think about adding provider types, like effect, importer, etc. in
// here.  The provider interface should not have to change when new types of
// plug-ins are added to Audacity.
//
// In addition a single provider may want to provide multiple plug-in types.
// ============================================================================

// ============================================================================
///
/// PluginProvider is an interface for discovering dynamically loaded libraries
/// according to some third party protocol, but also used in registering
/// the built-in effects and commands.
/// Classes derived from it can handle more specific plug-in protocols.
///
// ============================================================================

//! Interface for host side of one plug-in ("component") protocol that may
//! locate shared libraries with client-side code.  A factory of plug-in objects
//! that represent those clients.  One library may contain multiple plug-ins.
class COMPONENTS_API PluginProvider /* not final */ : public ComponentInterface
{
public:

    //![Optional] Implementation may provide plugin validator
    //!that may be used to perform additional checks. It's expected
    //!that validation does actions that possibly can crash or throw,
    //!for that reason it's not supposed to run within a main process.
    //TODO: it may seem reasonable to require providers to perform that check
    //in DiscoverPluginsAtPath, but some plugin types can safely provide meta
    //data, which is a good reason to ask to avoid such checks during plugin
    //discovery...
    class COMPONENTS_API Validator
    {
    public:
        //!\param pluginInterface loaded plugin
        virtual void Validate(ComponentInterface& pluginInterface) = 0;
        virtual ~Validator();
    };

    virtual ~PluginProvider();

    //! Called immediately after creation. Let provider initialize
    /*! @return "true" if initialization was successful */
    virtual bool Initialize() = 0;

    //! Called just prior to deletion to allow releasing any resources
    virtual void Terminate() = 0;

    //! A symbol identifying the family of plug-ins provided by this
    /*!
     If it is not empty, then the family as a whole can be enabled or
     disabled by the user in Preferences
     */
    virtual EffectFamilySymbol GetOptionalFamilySymbol() = 0;

    //! File types associated with this protocol
    /*!
     "Paths" returned by FindModulePaths() and passed back to
     DiscoverPluginsAtPath() have provider-specific meaning. They are not
     necessarily file system paths to existent files that could be placed in any
     folder and queried for plug-in information.

     This function returns nonempty only when that is the case, and lists the
     possible extensions of such files (an empty string in a nonempty array means
     any file is a candidate).
     */
    virtual const FileExtensions& GetFileExtensions() = 0;

    //! Where plug-in files should be copied to install them
    /*!
     @return may be empty. Drag-and-drop is supported only if GetFileExtensions()
     returns nonempty and this function returns nonempty.
     */
    virtual FilePath InstallPath() = 0;

    //! Called so that a provider of a static set of plug-ins can register them.
    virtual void AutoRegisterPlugins(PluginManagerInterface& pluginManager) = 0;

    virtual bool SupportsCustomModulePaths() const;

    //! Find available "paths", which may each be presented to the user, and
    //! then reexamined (possibly loading libraries) to find one or more plug-ins
    /*!
     Paths are not necessarily file system paths.  Only the provider reinterprets
     the paths.

     Modules may be associated with plug-ins, one-to-many.
     @see GetFileExtensions DiscoverPluginsAtPath
     */
    virtual PluginPaths
    FindModulePaths(PluginManagerInterface& pluginManager) = 0;

    //! Further expand a path reported by FindModulePaths
    /*!
     Once the user selects desired paths from FindModulePaths(), a call to
     DiscoverPluginsAtPath() will be made to request registration of one or more
     plug-ins.

     If the provider must create an instance of the plug-in to register it, the
     instance should be deleted after registration.

     May discover more than one plug-in at the path, and may call-back with
     a ComponentInterface that reports a path not equal to the given path
     (perhaps appending other information to it).

     @param errMsg message does not need to mention the path and may be nonempty
     even if some plug-ins are also discovered successfully.

     @return the number of plug-ins found
    */
    using RegistrationCallback = std::function<
        const PluginID& (PluginProvider*, ComponentInterface*) >;
    virtual unsigned DiscoverPluginsAtPath(
        const PluginPath& path, TranslatableString& errMsg, const RegistrationCallback& callback)
    = 0;

    /**
     * \brief Performs plugin/module existence check, still plugin may fail to load.
     * Implementation should avoid loading plugins during this check.
     * \param path Internal plugin path/ID discovered via DiscoverPluginsAtPath
     * or module path returned by FindModulePaths
     */
    virtual bool CheckPluginExist(const PluginPath& path) const = 0;

    //! Implementation can provide plugin specific checks to the plugin instances.
    //! By default returns null.
    virtual std::unique_ptr<Validator> MakeValidator() const;

    //! Load the plug-in at a path reported by DiscoverPluginsAtPath
    /*!
     @return smart pointer managing the later unloading
     */
    virtual std::unique_ptr<ComponentInterface>
    LoadPlugin(const PluginPath& path) = 0;
};

// ----------------------------------------------------------------------------
// Since there may be multiple embedded providers, the entry function will
// be declared static so as not to interfere with other providers during link.
// ----------------------------------------------------------------------------
#define DECLARE_PROVIDER_ENTRY(name)                  \
    static std::unique_ptr<PluginProvider> name()

// ----------------------------------------------------------------------------
// This will create a class and instance that will register the provider entry
// point during Audacity startup.  At the appropriate time, the entry point
// will be called to create the provider instance.
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// Provides the base for embedded provider registration.  If used, a Register()
// method must be supplied explicitly.
// ----------------------------------------------------------------------------

#define DECLARE_BUILTIN_PROVIDER_BASE(name)           \
    class name                                            \
    {                                                     \
    public:                                               \
        name() { Register(); }                               \
        ~name() { Unregister(); }                            \
        void Register();                                   \
        void Unregister();                                 \
    };                                                    \
    static name name##_instance;

// ----------------------------------------------------------------------------
// Provides the full embedded provider registration process.  Nothing further is
// required (other than supplying the provider entry point function).
// ----------------------------------------------------------------------------
#define DECLARE_BUILTIN_PROVIDER(name)                \
    DECLARE_BUILTIN_PROVIDER_BASE(name)                   \
    void name::Register()                                 \
    {                                                     \
        RegisterProviderFactory(AudacityModule);                  \
    }                                                     \
    void name::Unregister()                               \
    {                                                     \
        UnregisterProviderFactory(AudacityModule);                \
    }

#endif // __AUDACITY_MODULEINTERFACE_H__
