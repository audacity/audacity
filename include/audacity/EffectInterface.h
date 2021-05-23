/**********************************************************************

   Audacity: A Digital Audio Editor

   EffectInterface.h

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

#include <functional>

#include "Identifier.h"
#include "audacity/ComponentInterface.h"
#include "audacity/ConfigInterface.h"
#include "audacity/EffectAutomationParameters.h" // for command automation

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


/*************************************************************************************//**

\class EffectDefinitionInterface 

\brief EffectDefinitionInterface is a ComponentInterface that additionally tracks
flag-functions for interactivity, play-preview and whether the effect can run without a GUI.

*******************************************************************************************/
class AUDACITY_DLL_API EffectDefinitionInterface  /* not final */ : public ComponentInterface
{
public:
   virtual ~EffectDefinitionInterface() {};

   // Type determines how it behaves.
   virtual EffectType GetType() = 0;
   // Classification determines which menu it appears in.
   virtual EffectType GetClassification() { return GetType();};

   virtual EffectFamilySymbol GetFamily() = 0;

   // These should move to the "EffectClientInterface" class once all
   // effects have been converted.
   virtual bool IsInteractive() = 0;

   // I don't really like this, but couldn't think of a better way to force the
   // effect to appear "above the line" in the menus.
   virtual bool IsDefault() = 0;

   // This will go away when all Effects have been updated to the new
   // interface.
   virtual bool IsLegacy() = 0;

   // Whether the effect supports realtime previewing (while audio is playing).
   virtual bool SupportsRealtime() = 0;

   // Can the effect be used without the UI.
   virtual bool SupportsAutomation() = 0;
};

class wxDialog;
class wxWindow;
class EffectUIHostInterface;
class EffectUIClientInterface;

/*************************************************************************************//**

\class EffectHostInterface 

\brief EffectHostInterface is a decorator of a EffectUIClientInterface.  It adds 
virtual (abstract) functions to get presets and actually apply the effect.  It uses
ConfigClientInterface to add Getters/setters for private and shared configs. 

*******************************************************************************************/
class AUDACITY_DLL_API EffectHostInterface  /* not final */ : public ConfigClientInterface
{
public:
   virtual ~EffectHostInterface() {};

   virtual double GetDefaultDuration() = 0;
   virtual double GetDuration() = 0;
   virtual NumericFormatSymbol GetDurationFormat() = 0;
   virtual void SetDuration(double seconds) = 0;

   // Preset handling
   virtual RegistryPath GetUserPresetsGroup(const RegistryPath & name) = 0;
   virtual RegistryPath GetCurrentSettingsGroup() = 0;
   virtual RegistryPath GetFactoryDefaultsGroup() = 0;
};

/*************************************************************************************//**

\class EffectClientInterface 

\brief EffectClientInterface provides the ident interface to Effect, and is what makes
Effect into a plug-in command.  It has functions for realtime that are not part of 
AudacityCommand.

*******************************************************************************************/
class AUDACITY_DLL_API EffectClientInterface  /* not final */ : public EffectDefinitionInterface
{
public:
   using EffectDialogFactory = std::function<
      wxDialog* ( wxWindow &parent,
         EffectHostInterface*, EffectUIClientInterface* )
   >;

   virtual ~EffectClientInterface() {};

   virtual bool SetHost(EffectHostInterface *host) = 0;

   virtual unsigned GetAudioInCount() = 0;
   virtual unsigned GetAudioOutCount() = 0;

   virtual int GetMidiInCount() = 0;
   virtual int GetMidiOutCount() = 0;

   virtual void SetSampleRate(double rate) = 0;
   // Suggest a block size, but the return is the size that was really set:
   virtual size_t SetBlockSize(size_t maxBlockSize) = 0;
   virtual size_t GetBlockSize() const = 0;

   virtual sampleCount GetLatency() = 0;
   virtual size_t GetTailSize() = 0;

   virtual bool IsReady() = 0;
   virtual bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL) = 0;
   // This may be called during stack unwinding:
   virtual bool ProcessFinalize() /* noexcept */ = 0;
   virtual size_t ProcessBlock(float **inBlock, float **outBlock, size_t blockLen) = 0;

   virtual bool RealtimeInitialize() = 0;
   virtual bool RealtimeAddProcessor(unsigned numChannels, float sampleRate) = 0;
   virtual bool RealtimeFinalize() = 0;
   virtual bool RealtimeSuspend() = 0;
   virtual bool RealtimeResume() = 0;
   virtual bool RealtimeProcessStart() = 0;
   virtual size_t RealtimeProcess(int group, float **inBuf, float **outBuf, size_t numSamples) = 0;
   virtual bool RealtimeProcessEnd() = 0;

   virtual bool ShowInterface(
      wxWindow &parent, const EffectDialogFactory &factory,
      bool forceModal = false
   ) = 0;
   // Some effects will use define params to define what parameters they take.
   // If they do, they won't need to implement Get or SetAutomation parameters.
   // since the Effect class can do it.  Or at least that is how things happen
   // in AudacityCommand.  IF we do the same in class Effect, then Effect maybe 
   // should derive by some route from AudacityCommand to pick up that 
   // functionality.
   //virtual bool DefineParams( ShuttleParams & S){ return false;};
   virtual bool GetAutomationParameters(CommandParameters & parms) = 0;
   virtual bool SetAutomationParameters(CommandParameters & parms) = 0;

   virtual bool LoadUserPreset(const RegistryPath & name) = 0;
   virtual bool SaveUserPreset(const RegistryPath & name) = 0;

   virtual RegistryPaths GetFactoryPresets() = 0;
   virtual bool LoadFactoryPreset(int id) = 0;
   virtual bool LoadFactoryDefaults() = 0;
};

/*************************************************************************************//**

\class EffectUIHostInterface

\brief EffectUIHostInterface has nothing in it.  It is provided so that an Effect
can call SetHostUI passing in a pointer to an EffectUIHostInterface.  It contains no 
functionality and is provided, apparently, for type checking.  Since only EffectUIHost
uses it, EffectUIHost could be used instead.
*******************************************************************************************/
class AUDACITY_DLL_API EffectUIHostInterface
{
public:
   virtual ~EffectUIHostInterface() {};
};

/*************************************************************************************//**

\class EffectUIClientInterface

\brief EffectUIClientInterface is an abstract base class to populate a UI and validate UI
values.  It can import and export presets.

*******************************************************************************************/
class AUDACITY_DLL_API EffectUIClientInterface /* not final */
{
public:
   virtual ~EffectUIClientInterface() {};

   virtual void SetHostUI(EffectUIHostInterface *host) = 0;
   virtual bool IsGraphicalUI() = 0;
   virtual bool PopulateUI(ShuttleGui &S) = 0;
   virtual bool ValidateUI() = 0;
   virtual bool HideUI() = 0;
   virtual bool CloseUI() = 0;

   virtual bool CanExportPresets() = 0;
   virtual void ExportPresets() = 0;
   virtual void ImportPresets() = 0;

   virtual bool HasOptions() = 0;
   virtual void ShowOptions() = 0;
};


/*************************************************************************************//**

\class EffectManagerInterface

\brief UNUSED.  EffectManagerInterface provides a single function to find files matching 
a pattern in a list.

*******************************************************************************************/
class AUDACITY_DLL_API EffectManagerInterface
{
public:
   virtual ~EffectManagerInterface() {};

   virtual void FindFilesInPathList(const wxString & pattern,
                                    const FilePaths & pathList,
                                    FilePaths & files,
                                    int searchFlags) = 0;
};

#endif // __AUDACITY_EFFECTINTERFACE_H__
