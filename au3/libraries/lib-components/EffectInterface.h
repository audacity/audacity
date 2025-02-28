/*!********************************************************************

   Audacity: A Digital Audio Editor

   @file EffectInterface.h

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

#ifndef __AUDACITY_EFFECTINTERFACE_H__
#define __AUDACITY_EFFECTINTERFACE_H__

#include "ComponentInterface.h"
#include "ComponentInterfaceSymbol.h"
#include "EffectAutomationParameters.h"

#include "TypedAny.h"
#include <cstdint>
#include <memory>
#include <optional>
#include <type_traits>

#include "Observer.h"

class ShuttleGui;
template<bool Const> class SettingsVisitorBase;
using SettingsVisitor = SettingsVisitorBase<false>;
using ConstSettingsVisitor = SettingsVisitorBase<true>;

typedef enum EffectType : int
{
    EffectTypeNone,
    EffectTypeHidden,
    EffectTypeGenerate,
    EffectTypeProcess,
    EffectTypeAnalyze,
    EffectTypeTool,
} EffectType;

using EffectFamilySymbol = ComponentInterfaceSymbol;

//! Non-polymorphic package of settings values common to many effects
class COMPONENTS_API EffectSettingsExtra final
{
public:
    static const RegistryPath& DurationKey();
    const NumericFormatID& GetDurationFormat() const
    { return mDurationFormat; }
    void SetDurationFormat(const NumericFormatID& durationFormat)
    { mDurationFormat = durationFormat; }

    //! @return value is not negative
    double GetDuration() const { return mDuration; }
    void SetDuration(double value) { mDuration = std::max(0.0, value); }

    bool GetActive() const { return mActive; }
    void SetActive(bool value) { mActive = value; }
private:
    NumericFormatID mDurationFormat{};
    double mDuration{}; //!< @invariant non-negative
    bool mActive{ true };
};

//! Externalized state of a plug-in
struct EffectSettings : audacity::TypedAny<EffectSettings> {
    using TypedAny::TypedAny;
    EffectSettingsExtra extra;

    void swap(EffectSettings& other)
    {
        TypedAny::swap(other);
        std::swap(extra, other.extra);
    }

    //! Like make_any but a static member function
    template<typename T, typename ... Args>
    static EffectSettings Make(Args&&... args)
    {
        return EffectSettings(std::in_place_type<T>, std::forward<Args>(args)...);
    }

    //! Convenience for defining overrides of
    //! EffectDefinitionInterface::CopySettingsContents
    template<typename T>
    static bool Copy(const EffectSettings& src, EffectSettings& dst)
    {
        const T* pSrc = src.cast<T>();
        T* pDst = dst.cast<T>();
        if (pSrc && pDst) {
            *pDst = *pSrc;
            return true;
        }
        return false;
    }
};

//! Hold values to send to effect output meters
class COMPONENTS_API EffectOutputs
{
public:
    virtual ~EffectOutputs();
    virtual std::unique_ptr<EffectOutputs> Clone() const = 0;

    //! Update one Outputs object from another
    /*!
     This may run in a worker thread, and should avoid allocating and freeing.
     Even on the main thread, it must avoid relocation of members of containers.
     Therefore do not grow or clear any containers, but assign the preallocated
     contents of one container from another.

     @param src settings to copy from; assume it comes from the same
     EffectSettingsManager as *this
     */
    virtual void Assign(EffectOutputs&& src) = 0;
};

//! Interface for accessing an EffectSettings that may change asynchronously in
//! another thread; to be used in the main thread, only.
/*! Updates are communicated atomically both ways.  The address of Get() should
 not be relied on as unchanging between calls. */
class COMPONENTS_API EffectSettingsAccess : public std::enable_shared_from_this<EffectSettingsAccess>
{
public:
    //! Type of messages to send from main thread to processing
    class COMPONENTS_API Message
    {
    public:
        virtual ~Message();
        virtual std::unique_ptr<Message> Clone() const = 0;

        //! Update one Message object from another, which is then left "empty"
        /*!
         This may run in a worker thread, and should avoid allocating and freeing.
         Therefore do not copy, grow or clear any containers in it, but assign the
         preallocated contents of *this from another, which then should be
         reassigned to an initial state.

         Assume that src and *this come from the same EffectInstance.

         @param src settings to copy from
         */
        virtual void Assign(Message&& src) = 0;

        //! Combine one Message object with another, which is then left "empty"
        /*!
         This runs in the main thread.
         Combine the contents of one message into *this, and then the other
         should be reassigned to an initial state.

         Assume that src and *this come from the same EffectInstance.

         @param src settings to copy from
         */
        virtual void Merge(Message&& src) = 0;
    };

    virtual ~EffectSettingsAccess();
    virtual const EffectSettings& Get() = 0;
    virtual void Set(EffectSettings&& settings, std::unique_ptr<Message> pMessage = nullptr) = 0;
    //! Message-only overload of Set().  In future, this should be the only one.
    virtual void Set(std::unique_ptr<Message> pMessage = nullptr) = 0;

    //! Make the last `Set` changes "persistent" in underlying storage
    /*!
     @pre called on the main thread only
     */
    virtual void Flush() = 0;

    //! @return whether this and the other give access to the same settings
    virtual bool IsSameAs(const EffectSettingsAccess& other) const = 0;

    //! Do a correct read-modify-write of settings
    /*!
     @param function takes EffectSettings & and its return is a unique pointer
     to Message, possibly null.
     If it throws an exception, then the settings will not be updated.
     Thus, a strong exception safety guarantee.
     */
    template<typename Function>
    void ModifySettings(Function&& function)
    {
        auto settings = this->Get();
        auto result = std::forward<Function>(function)(settings);
        this->Set(std::move(settings), std::move(result));
    }
};

//! Implementation of EffectSettings for cases where there is only one thread.
class COMPONENTS_API SimpleEffectSettingsAccess final : public EffectSettingsAccess
{
public:
    explicit SimpleEffectSettingsAccess(EffectSettings& settings)
        : mSettings{settings} {}
    ~SimpleEffectSettingsAccess() override;
    const EffectSettings& Get() override;
    void Set(EffectSettings&& settings, std::unique_ptr<Message> pMessage) override;
    void Set(std::unique_ptr<Message> pMessage) override;
    void Flush() override;
    bool IsSameAs(const EffectSettingsAccess& other) const override;
private:
    EffectSettings& mSettings;
};

/*************************************************************************************//**

\class EffectDefinitionInterface

\brief EffectDefinitionInterface is a ComponentInterface that adds some basic
read-only information about effect properties, and getting and setting of
parameters.

*******************************************************************************************/
class COMPONENTS_API EffectDefinitionInterface /* not final */ : public ComponentInterface
{
public:
    //! A utility that strips spaces and CamelCases a name.
    static Identifier GetSquashedName(const Identifier& ident);

    virtual ~EffectDefinitionInterface();

    //! Type determines how it behaves.
    virtual EffectType GetType() const = 0;

    //! Determines which menu it appears in; default same as GetType().
    virtual EffectType GetClassification() const;

    //! Report identifier and user-visible name of the effect protocol
    virtual EffectFamilySymbol GetFamily() const = 0;

    //! Whether the effect needs a dialog for entry of settings
    virtual bool IsInteractive() const = 0;

    //! Whether the effect sorts "above the line" in the menus
    virtual bool IsDefault() const = 0;

    //! In which versions of Audacity was an effect realtime capable?
    enum class RealtimeSince : unsigned {
        Never,
        // For built-in effects that became realtime in 3.2.x or a later version
        // but were non-realtime in an earlier version; must also increase
        // REGVERCUR in any release with such a change
        After_3_1,
        Always,
    };

    //! Since which version of Audacity has the effect supported realtime?
    virtual RealtimeSince RealtimeSupport() const = 0;

    //! Whether the effect supports realtime previewing (while audio is playing).
    //! non-virtual
    bool SupportsRealtime() const
    { return RealtimeSupport() != RealtimeSince::Never; }

    //! Whether the effect has any automatable controls.
    virtual bool SupportsAutomation() const = 0;

    //! Whether the effect dialog should have a Debug button; default, always false.
    virtual bool EnablesDebug() const;

    //! Name of a page in the Audacity alpha manual, default is empty
    virtual ManualPageID ManualPage() const;

    //! Fully qualified local help file name, default is empty
    virtual FilePath HelpPage() const;

    //! Default is false
    virtual bool IsHiddenFromMenus() const;
};

using OptionalMessage
    =std::optional<std::unique_ptr<EffectSettingsAccess::Message> >;

/*************************************************************************************//**

\class EffectSettingsManager

\brief EffectSettingsManager is an EffectDefinitionInterface that adds a
factory function for EffectSettings, and const functions for manipulating those
settings.  This externalizes certain effect state.

*******************************************************************************************/
class COMPONENTS_API EffectSettingsManager /* not final */ : public EffectDefinitionInterface
{
public:
    virtual ~EffectSettingsManager();

    /*! @name settings
     Interface for saving and loading externalized settings.
     All methods are const!
     */
    //! @{
    //! Produce an object holding new, independent settings
    /*!
     Default implementation returns an empty `any`
     */
    virtual EffectSettings MakeSettings() const;

    //! Update one settings object from another
    /*!
     This may run in a worker thread, and should avoid memory allocations.
     Therefore do not copy the underlying std::any, but copy the contents of the
     contained objects.

     Assume that src and dst were created and previously modified only by `this`

     Default implementation does nothing and returns true

     @param src settings to copy from
     @param dst settings to copy into
     @param copyDirection direction in which copy is performed
     @return success
     */
    virtual bool CopySettingsContents(
        const EffectSettings& src, EffectSettings& dst) const;

    //! Store settings as keys and values
    /*!
     The override may assume `parms` is initially empty
     @return true on success
     */
    virtual bool SaveSettings(
        const EffectSettings& settings, CommandParameters& parms) const = 0;

    //! Restore settings from keys and values
    /*!
     @return true on success
     */
    virtual bool LoadSettings(
        const CommandParameters& parms, EffectSettings& settings) const = 0;

    //! Report names of factory presets
    virtual RegistryPaths GetFactoryPresets() const = 0;

    //! Change settings to a user-named preset
    //! @return nullopt for failure
    [[nodiscard]] virtual OptionalMessage LoadUserPreset(
        const RegistryPath& name, EffectSettings& settings) const = 0;
    //! Save settings in the configuration file as a user-named preset
    virtual bool SaveUserPreset(
        const RegistryPath& name, const EffectSettings& settings) const = 0;

    //! Change settings to the preset whose name is `GetFactoryPresets()[id]`
    //! @return nullopt for failure
    [[nodiscard]] virtual OptionalMessage LoadFactoryPreset(
        int id, EffectSettings& settings) const = 0;
    //! Change settings back to "factory default"
    //! @return nullopt for failure
    [[nodiscard]] virtual OptionalMessage LoadFactoryDefaults(
        EffectSettings& settings) const = 0;
    //! @}

    //! Visit settings (and maybe change them), if defined.
    //! false means no defined settings.
    //! Default implementation returns false
    virtual bool VisitSettings(
        SettingsVisitor& visitor, EffectSettings& settings); // TODO const

    //! Visit settings (read-only), if defined.
    //! false means no defined settings.
    //! Default implementation returns false
    virtual bool VisitSettings(
        ConstSettingsVisitor& visitor, const EffectSettings& settings) const;

    /*! @name outputs
     @{
     */
    //! Produce an object to hold values to send to effect output meters
    /*!
     Default implementation returns nullptr
     */
    virtual std::unique_ptr<EffectOutputs> MakeOutputs() const;
    //! @}
};

// ----------------------------------------------------------------------------
// Supported channel assignments
// ----------------------------------------------------------------------------

enum ChannelName : int {
    // Use to mark end of list
    ChannelNameEOL = -1,
    // The default channel assignment
    ChannelNameMono,
    // From this point, the channels follow the 22.2 surround sound format
    ChannelNameFrontLeft,
    ChannelNameFrontRight,
    ChannelNameFrontCenter,
    ChannelNameLowFrequency1,
    ChannelNameBackLeft,
    ChannelNameBackRight,
    ChannelNameFrontLeftCenter,
    ChannelNameFrontRightCenter,
    ChannelNameBackCenter,
    ChannelNameLowFrequency2,
    ChannelNameSideLeft,
    ChannelNameSideRight,
    ChannelNameTopFrontLeft,
    ChannelNameTopFrontRight,
    ChannelNameTopFrontCenter,
    ChannelNameTopCenter,
    ChannelNameTopBackLeft,
    ChannelNameTopBackRight,
    ChannelNameTopSideLeft,
    ChannelNameTopSideRight,
    ChannelNameTopBackCenter,
    ChannelNameBottomFrontCenter,
    ChannelNameBottomFrontLeft,
    ChannelNameBottomFrontRight,
};
using ChannelNames = const ChannelName*;

/***************************************************************************//**
\class EffectInstance
@brief Performs effect computation
*******************************************************************************/
class COMPONENTS_API EffectInstance : public std::enable_shared_from_this<EffectInstance>
{
public:
    EffectInstance();
    virtual ~EffectInstance();

    int id() const;

    virtual size_t GetBlockSize() const = 0;

    // Suggest a block size, but the return is the size that was really set:
    virtual size_t SetBlockSize(size_t maxBlockSize) = 0;

    //! How many input buffers to allocate at once
    /*!
     If the instance processes channels independently, this can return 1
     The result is not necessarily well defined before `RealtimeInitialize`
     */
    virtual unsigned GetAudioInCount() const = 0;

    //! How many output buffers to allocate at once
    /*!
     The result is not necessarily well defined before `RealtimeInitialize`
     */
    virtual unsigned GetAudioOutCount() const = 0;

    /*!
     @return success
     @post `GetAudioInCount()` and `GetAudioOutCount()` are well defined

     Default implementation does nothing, returns false (so assume realtime is
     not supported).
     Other member functions related to realtime return true or zero, but will not
     be called, unless a derived class overrides RealtimeInitialize.
     */
    virtual bool RealtimeInitialize(EffectSettings& settings, double sampleRate);

    /*!
     @return success
     Default implementation does nothing, returns true
     */
    virtual bool RealtimeAddProcessor(
        EffectSettings& settings, EffectOutputs* pOutputs, unsigned numChannels, float sampleRate);

    /*!
     @return success
     Default implementation does nothing, returns true
     */
    virtual bool RealtimeSuspend();

    /*!
     @return success
     Default implementation does nothing, returns true
     */
    virtual bool RealtimeResume();

    //! Type of messages to send from main thread to processing, which can
    //! describe the transitions of settings (instead of their states)
    using Message = EffectSettingsAccess::Message;

    //! Called on the main thread, in which the result may be cloned
    /*! Default implementation returns a null */
    virtual std::unique_ptr<Message> MakeMessage() const;

    // TODO make it just an alias for Message *
    struct MessagePackage {
        EffectSettings& settings;
        Message* pMessage{};
    };

    //! If true, the effect makes no use EffectSettings for inter-thread
    //! comminication
    /*!
     Default implementation returns false.  In future, all effects should be
     rewritten to use messages and this function will be removed.
     */
    virtual bool UsesMessages() const noexcept;

    //! settings are possibly changed, since last call, by an asynchronous dialog
    /*!
     @return success
     Default implementation does nothing, returns true
     */
    virtual bool RealtimeProcessStart(MessagePackage& package);

    /*!
     @return success
     Default implementation does nothing, returns 0
     */
    virtual size_t RealtimeProcess(size_t group, EffectSettings& settings, const float* const* inBuf, float* const* outBuf,
                                   size_t numSamples);

    /*!
     \brief Called instead of `RealtimeProcess` when the effect is bypassed.
     Default implementation does nothing.
     */
    virtual void RealtimePassThrough(
        size_t group, EffectSettings& settings, const float* const* inBuf, size_t numSamples);

    //! settings can be updated to let a dialog change appearance at idle
    /*!
     @return success
     Default implementation does nothing, returns true
     */
    virtual bool RealtimeProcessEnd(EffectSettings& settings) noexcept;

    /*!
     @return success
     Default implementation does nothing, returns true
     */
    virtual bool RealtimeFinalize(EffectSettings& settings) noexcept;

    //! Function that has not yet found a use
    //! Correct definitions of it will likely depend on settings and state
    virtual size_t GetTailSize() const;

    using SampleCount = uint64_t;

    /*!
     Default implementation returns 0
     */
    virtual SampleCount GetLatency(
        const EffectSettings& settings, double sampleRate) const;

    /*! If true (default result), then results require dither if later rendered
     to a narrower sample format */
    virtual bool NeedsDither() const;

    //! Called at start of destructive processing, for each (mono/stereo) track
    //! Default implementation does nothing, returns true
    /*!
     @param chanMap null or array terminated with ChannelNameEOL.  Do not retain
        the pointer
     @post `GetAudioInCount()` and `GetAudioOutCount()` are well defined
     */
    virtual bool ProcessInitialize(EffectSettings& settings, double sampleRate, ChannelNames chanMap) = 0;

    //! Called at end of destructive processing, for each (mono/stereo) track
    //! Default implementation does nothing, returns true
    //! This may be called during stack unwinding:
    virtual bool ProcessFinalize() noexcept = 0;

    //! Called for destructive effect computation
    virtual size_t ProcessBlock(EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen)
    = 0;

private:
    int m_id = -1;
};

//! Inherit to add a state variable to an EffectInstance subclass
class COMPONENTS_API EffectInstanceWithBlockSize : public virtual EffectInstance
{
public:
    ~EffectInstanceWithBlockSize() override;
    size_t GetBlockSize() const override;
    size_t SetBlockSize(size_t maxBlockSize) override;
protected:
    size_t mBlockSize{ 0 };
};

/***************************************************************************//**
\class EffectInstanceFactory
*******************************************************************************/
class COMPONENTS_API EffectInstanceFactory : public EffectSettingsManager
{
public:
    virtual ~EffectInstanceFactory();

    //! Make an object maintaining short-term state of an Effect
    /*!
     One effect may have multiple instances extant simultaneously.
     Instances have state, may be implemented in foreign code, and are temporary,
     whereas EffectSettings represents persistent effect state that can be saved
     and reloaded from files.

     @param settings may be assumed to have a lifetime enclosing the instance's

     @post `true` (no promises that the result isn't null)
     */
    virtual std::shared_ptr<EffectInstance> MakeInstance() const = 0;
};

//! Component of a configuration key path, for last-used destructive settings
COMPONENTS_API const RegistryPath& CurrentSettingsGroup();

//! Component of a configuration key path, for default state of MakeSettings()
COMPONENTS_API const RegistryPath& FactoryDefaultsGroup();

//! Compute part of a registry path, given a name which may be empty
COMPONENTS_API RegistryPath UserPresetsGroup(const RegistryPath& name);

#endif // __AUDACITY_EFFECTINTERFACE_H__
