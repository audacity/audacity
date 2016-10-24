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

#include "audacity/Types.h"
#include "audacity/IdentInterface.h"
#include "audacity/ConfigInterface.h"
#include "audacity/EffectAutomationParameters.h"

#include <wx/dialog.h>

typedef enum EffectType
{
   EffectTypeNone,
   EffectTypeHidden,
   EffectTypeGenerate,
   EffectTypeProcess,
   EffectTypeAnalyze
} EffectType;

class AUDACITY_DLL_API EffectIdentInterface  /* not final */ : public IdentInterface
{
public:
   virtual ~EffectIdentInterface() {};

   virtual EffectType GetType() = 0;
   virtual wxString GetFamily() = 0;

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

class EffectUIHostInterface;
class EffectUIClientInterface;

class AUDACITY_DLL_API EffectHostInterface  /* not final */ : public ConfigClientInterface
{
public:
   virtual ~EffectHostInterface() {};

   virtual double GetDefaultDuration() = 0;
   virtual double GetDuration() = 0;
   virtual wxString GetDurationFormat() = 0;
   virtual void SetDuration(double seconds) = 0;

   virtual bool Apply() = 0;
   virtual void Preview() = 0;

   //
   virtual wxDialog *CreateUI(wxWindow *parent, EffectUIClientInterface *client) = 0;

   // Preset handling
   virtual wxString GetUserPresetsGroup(const wxString & name) = 0;
   virtual wxString GetCurrentSettingsGroup() = 0;
   virtual wxString GetFactoryDefaultsGroup() = 0;
};

class AUDACITY_DLL_API EffectClientInterface  /* not final */ : public EffectIdentInterface
{
public:
   virtual ~EffectClientInterface() {};

   virtual bool SetHost(EffectHostInterface *host) = 0;

   virtual unsigned GetAudioInCount() = 0;
   virtual unsigned GetAudioOutCount() = 0;

   virtual int GetMidiInCount() = 0;
   virtual int GetMidiOutCount() = 0;

   virtual void SetSampleRate(double rate) = 0;
   virtual size_t SetBlockSize(size_t maxBlockSize) = 0;

   virtual sampleCount GetLatency() = 0;
   virtual size_t GetTailSize() = 0;

   virtual bool IsReady() = 0;
   virtual bool ProcessInitialize(sampleCount totalLen, ChannelNames chanMap = NULL) = 0;
   virtual bool ProcessFinalize() = 0;
   virtual size_t ProcessBlock(float **inBlock, float **outBlock, size_t blockLen) = 0;

   virtual bool RealtimeInitialize() = 0;
   virtual bool RealtimeAddProcessor(unsigned numChannels, float sampleRate) = 0;
   virtual bool RealtimeFinalize() = 0;
   virtual bool RealtimeSuspend() = 0;
   virtual bool RealtimeResume() = 0;
   virtual bool RealtimeProcessStart() = 0;
   virtual size_t RealtimeProcess(int group, float **inBuf, float **outBuf, size_t numSamples) = 0;
   virtual bool RealtimeProcessEnd() = 0;

   virtual bool ShowInterface(wxWindow *parent, bool forceModal = false) = 0;

   virtual bool GetAutomationParameters(EffectAutomationParameters & parms) = 0;
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms) = 0;

   virtual bool LoadUserPreset(const wxString & name) = 0;
   virtual bool SaveUserPreset(const wxString & name) = 0;

   virtual wxArrayString GetFactoryPresets() = 0;
   virtual bool LoadFactoryPreset(int id) = 0;
   virtual bool LoadFactoryDefaults() = 0;
};

class AUDACITY_DLL_API EffectUIHostInterface
{
public:
   virtual ~EffectUIHostInterface() {};
};

class AUDACITY_DLL_API EffectUIClientInterface /* not final */
{
public:
   virtual ~EffectUIClientInterface() {};

   virtual void SetHostUI(EffectUIHostInterface *host) = 0;
   virtual bool IsGraphicalUI() = 0;
   virtual bool PopulateUI(wxWindow *parent) = 0;
   virtual bool ValidateUI() = 0;
   virtual bool HideUI() = 0;
   virtual bool CloseUI() = 0;

   virtual bool CanExportPresets() = 0;
   virtual void ExportPresets() = 0;
   virtual void ImportPresets() = 0;

   virtual bool HasOptions() = 0;
   virtual void ShowOptions() = 0;
};

class AUDACITY_DLL_API EffectManagerInterface
{
public:
   virtual ~EffectManagerInterface() {};

   virtual void FindFilesInPathList(const wxString & pattern,
                                    const wxArrayString & pathList,
                                    wxArrayString & files,
                                    int searchFlags) = 0;
};

#endif // __AUDACITY_EFFECTINTERFACE_H__
