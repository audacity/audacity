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
#include "EffectAutomationParameters.h" // for command automation

#include "TypedAny.h"
#include <memory>

class ShuttleGui;

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
class EffectSettingsExtra final {
public:
   const NumericFormatSymbol& GetDurationFormat() const
      { return mDurationFormat; }
   void SetDurationFormat(const NumericFormatSymbol &durationFormat)
      { mDurationFormat = durationFormat; }
private:
   NumericFormatSymbol mDurationFormat{};
};

//! Externalized state of a plug-in
struct EffectSettings : audacity::TypedAny<EffectSettings> {
   using TypedAny::TypedAny;
   EffectSettingsExtra extra;

   void swap(EffectSettings &other)
   {
      TypedAny::swap(other);
      std::swap(extra, other.extra);
   }
};

//! Interface for accessing an EffectSettings that may change asynchronously in
//! another thread; to be used in the main thread, only.
/*! Updates are communicated atomically both ways.  The address of Get() should
 not be relied on as unchanging between calls. */
class COMPONENTS_API EffectSettingsAccess
   : public std::enable_shared_from_this<EffectSettingsAccess> {
public:
   virtual ~EffectSettingsAccess();
   virtual const EffectSettings &Get() = 0;
   virtual void Set(EffectSettings &&settings) = 0;

   //! Do a correct read-modify-write of settings
   /*!
    @param function takes EffectSettings & and its return is ignored.
    If it throws an exception, then the settings will not be updated.
    Thus, a strong exception safety guarantee.
    */
   template<typename Function>
   void ModifySettings(Function &&function) {
      auto settings = this->Get();
      std::forward<Function>(function)(settings);
      this->Set(std::move(settings));
   }
};

//! Implementation of EffectSettings for cases where there is only one thread.
class COMPONENTS_API SimpleEffectSettingsAccess final
   : public EffectSettingsAccess {
public:
   explicit SimpleEffectSettingsAccess(EffectSettings &settings)
      : mSettings{settings} {}
   ~SimpleEffectSettingsAccess() override;
   const EffectSettings &Get() override;
   void Set(EffectSettings &&settings) override;
private:
   EffectSettings &mSettings;
};

/*************************************************************************************//**

\class EffectDefinitionInterface 

\brief EffectDefinitionInterface is a ComponentInterface that adds some basic
read-only information about effect properties, and getting and setting of
parameters.

*******************************************************************************************/
class COMPONENTS_API EffectDefinitionInterface  /* not final */ : public ComponentInterface
{
public:
   using Settings = EffectSettings;

   //! A utility that strips spaces and CamelCases a name.
   static Identifier GetSquashedName(const Identifier &ident);

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

   //! Whether the effect supports realtime previewing (while audio is playing).
   virtual bool SupportsRealtime() const = 0;

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

   // Some effects will use define params to define what parameters they take.
   // If they do, they won't need to implement Get or SetAutomation parameters.
   // since the Effect class can do it.  Or at least that is how things happen
   // in AudacityCommand.  IF we do the same in class Effect, then Effect maybe
   // should derive by some route from AudacityCommand to pick up that
   // functionality.
   //virtual bool DefineParams( ShuttleParams & S);

   /*! @name settings
    Interface for saving and loading externalized settings.
    All methods are const!
    */
   //! @{
   //! Produce an object holding new, independent settings
   virtual Settings MakeSettings() const = 0;

   //! Update one settings object from another
   /*!
    This may run in a worker thread, and should avoid memory allocations.
    Therefore do not copy the underlying std::any, but copy the contents of the
    contained objects.

    Assume that src and dst were created and previously modified only by `this`

    @param src settings to copy from
    @param dst settings to copy into
    @return success
    */
   virtual bool CopySettingsContents(
      const EffectSettings &src, EffectSettings &dst) const = 0;

   //! Store settings as keys and values
   /*!
    @return true on success
    */
   virtual bool SaveSettings(
      const Settings &settings, CommandParameters & parms) const = 0;

   //! Restore settings from keys and values
   /*!
    @return true on success
    */
   virtual bool LoadSettings(
      CommandParameters & parms, Settings &settings) const = 0;

   //! Report names of factory presets
   virtual RegistryPaths GetFactoryPresets() const = 0;

   //! Change settings to a user-named preset
   virtual bool LoadUserPreset(
      const RegistryPath & name, Settings &settings) const = 0;
   //! Save settings in the configuration file as a user-named preset
   virtual bool SaveUserPreset(
      const RegistryPath & name, const Settings &settings) const = 0;

   //! Change settings to the preset whose name is `GetFactoryPresets()[id]`
   virtual bool LoadFactoryPreset(int id, Settings &settings) const = 0;
   //! Change settings back to "factory default"
   virtual bool LoadFactoryDefaults(Settings &settings) const = 0;
   //! @}
};

//! Extension of EffectDefinitionInterface with old system for settings
/*!
 (Default implementations of EffectDefinitionInterface methods for settings call
 through to the old interface, violating const correctness.  This is meant to be
 transitional only.)

 This interface is not used by the EffectUIHost dialog.
 */
class COMPONENTS_API EffectDefinitionInterfaceEx  /* not final */
   : public EffectDefinitionInterface
{
public:
   /*! @name Old settings interface
    Old interface for saving and loading non-externalized settings
    */
   //! @{
   //! Save current settings into parms
   virtual bool GetAutomationParameters(CommandParameters & parms) = 0;
   //! Change settings to those stored in parms
   virtual bool SetAutomationParameters(CommandParameters & parms) = 0;

   //! Change settings to a user-named preset
   virtual bool LoadUserPreset(const RegistryPath & name) = 0;
   //! Save current settings as a user-named preset
   virtual bool SaveUserPreset(const RegistryPath & name) = 0;

   //! Change settings to the preset whose name is `GetFactoryPresets()[id]`
   virtual bool LoadFactoryPreset(int id) = 0;
   //! Change settings back to "factory default"
   virtual bool LoadFactoryDefaults() = 0;
   //! @}

   /*! @name settings
    Default implementation of the nominally const methods call through to the
    old non-const interface
    */
   //! @{
   Settings MakeSettings() const override;
   bool CopySettingsContents(
      const EffectSettings &src, EffectSettings &dst) const override;
   bool SaveSettings(
      const Settings &settings, CommandParameters & parms) const override;
   bool LoadSettings(
      CommandParameters & parms, Settings &settings) const override;

   bool LoadUserPreset(
      const RegistryPath & name, Settings &settings) const override;
   bool SaveUserPreset(
      const RegistryPath & name, const Settings &settings) const override;
   bool LoadFactoryPreset(int id, Settings &settings) const override;
   bool LoadFactoryDefaults(Settings &settings) const override;
   //! @}

private:
   EffectDefinitionInterfaceEx *FindMe(const Settings &settings) const;
};

class wxDialog;
class wxWindow;

class EffectUIClientInterface;

// Incomplete type not defined in libraries -- TODO clean that up:
class EffectHostInterface;

class sampleCount;

// ----------------------------------------------------------------------------
// Supported channel assignments
// ----------------------------------------------------------------------------

typedef enum
{
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
} ChannelName, *ChannelNames;

/*************************************************************************************//**

\class EffectProcessor 

\brief provides the ident interface to Effect, and is what makes
Effect into a plug-in command.  It has functions for effect calculations that are not part of
AudacityCommand.

*******************************************************************************************/
class COMPONENTS_API EffectProcessor  /* not final */
   : public EffectDefinitionInterfaceEx
{
public:
   virtual ~EffectProcessor();

   virtual unsigned GetAudioInCount() const = 0;
   virtual unsigned GetAudioOutCount() const = 0;

   virtual int GetMidiInCount() = 0;
   virtual int GetMidiOutCount() = 0;

   virtual void SetSampleRate(double rate) = 0;
   // Suggest a block size, but the return is the size that was really set:
   virtual size_t SetBlockSize(size_t maxBlockSize) = 0;
   virtual size_t GetBlockSize() const = 0;

   //! Called for destructive, non-realtime effect computation
   virtual sampleCount GetLatency() = 0;
   virtual size_t GetTailSize() = 0;

   //! Called for destructive, non-realtime effect computation
   virtual bool ProcessInitialize(EffectSettings &settings,
      sampleCount totalLen, ChannelNames chanMap = nullptr) = 0;

   //! Called for destructive, non-realtime effect computation
   // This may be called during stack unwinding:
   virtual bool ProcessFinalize() /* noexcept */ = 0;

   //! Called for destructive, non-realtime effect computation
   virtual size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen) = 0;

   virtual bool RealtimeInitialize(EffectSettings &settings) = 0;
   virtual bool RealtimeAddProcessor(
      EffectSettings &settings, unsigned numChannels, float sampleRate) = 0;
   virtual bool RealtimeFinalize(EffectSettings &settings) noexcept = 0;
   virtual bool RealtimeSuspend() = 0;
   virtual bool RealtimeResume() noexcept = 0;
   //! settings are possibly changed, since last call, by an asynchronous dialog
   virtual bool RealtimeProcessStart(EffectSettings &settings) = 0;
   virtual size_t RealtimeProcess(int group, EffectSettings &settings,
      const float *const *inBuf, float *const *outBuf, size_t numSamples) = 0;
   //! settings can be updated to let a dialog change appearance at idle
   virtual bool RealtimeProcessEnd(EffectSettings &settings) noexcept = 0;
};

/*************************************************************************************//**

\class EffectUIValidator

\brief Interface for transferring values from a panel of effect controls

*******************************************************************************************/
class COMPONENTS_API EffectUIValidator /* not final */
{
public:
   virtual ~EffectUIValidator();
   //! Get settings data from the panel; may make error dialogs and return false
   /*!
    @return true only if panel settings are acceptable
    */
   virtual bool Validate() = 0;
};

/*************************************************************************************//**

\class DefaultEffectUIValidator

\brief Default implementation of EffectUIValidator invokes ValidateUI and CloseUI
   methods of an EffectUIClientInterface

 This is a transitional class; it should be eliminated when all effect classes
 define their own associated subclasses of EffectUIValidator, which can hold
 state only for the lifetime of a dialog, so the effect object need not hold it

*******************************************************************************************/
class COMPONENTS_API DefaultEffectUIValidator final : public EffectUIValidator
{
public:
   DefaultEffectUIValidator(
      EffectUIClientInterface &effect, EffectSettingsAccess &access);
   ~DefaultEffectUIValidator() override;
   bool Validate() override;
private:
   EffectUIClientInterface &mEffect;
   EffectSettingsAccess &mAccess;
};

/*************************************************************************************//**

\class EffectUIClientInterface

\brief EffectUIClientInterface is an abstract base class to populate a UI and validate UI
values.  It can import and export presets.

*******************************************************************************************/
class COMPONENTS_API EffectUIClientInterface /* not final */
   : public EffectProcessor
{
public:
   virtual ~EffectUIClientInterface();

   /*!
    @return 0 if destructive effect processing should not proceed (and there
    may be a non-modal dialog still opened); otherwise, modal dialog return code
    */
   virtual int ShowClientInterface(
      wxWindow &parent, wxDialog &dialog, bool forceModal = false
   ) = 0;

   /*!
    @return true if successful
    */
   virtual bool InitializeInstance(EffectHostInterface *host) = 0;

   virtual bool IsGraphicalUI() = 0;

   //! Adds controls to a panel that is given as the parent window of `S`
   /*!
    @param S interface for adding controls to a panel in a dialog
    @param access guaranteed to have a lifetime containing that of the returned
    object

    @return null for failure; else an object invoked to retrieve values of UI
    controls; it might also hold some state needed to implement event handlers
    of the controls; it will exist only while the dialog continues to exist
    */
   virtual std::unique_ptr<EffectUIValidator> PopulateUI(
      ShuttleGui &S, EffectSettingsAccess &access) = 0;

   virtual bool CanExportPresets() = 0;
   virtual void ExportPresets() = 0;
   virtual void ImportPresets() = 0;

   virtual bool HasOptions() = 0;
   virtual void ShowOptions() = 0;

protected:
   friend DefaultEffectUIValidator;
   virtual bool ValidateUI(EffectSettings &settings) = 0;
   virtual bool CloseUI() = 0;
};

//! Component of a configuration key path
COMPONENTS_API const RegistryPath &CurrentSettingsGroup();

//! Component of a configuration key path
COMPONENTS_API const RegistryPath &FactoryDefaultsGroup();

//! Compute part of a registry path, given a name which may be empty
COMPONENTS_API RegistryPath UserPresetsGroup(const RegistryPath & name);

#endif // __AUDACITY_EFFECTINTERFACE_H__

