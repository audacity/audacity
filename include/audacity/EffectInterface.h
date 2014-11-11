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

#include <vector>

#include "audacity/Types.h"
#include "audacity/IdentInterface.h"
#include "audacity/ConfigInterface.h"

typedef enum EffectType
{
   EffectTypeNone,
   EffectTypeGenerate,
   EffectTypeProcess,
   EffectTypeAnalyze
} EffectType;

class AUDACITY_DLL_API EffectIdentInterface : public IdentInterface
{
public:
   virtual ~EffectIdentInterface() {};

   virtual EffectType GetType() = 0;
   virtual wxString GetFamily() = 0;

   // These should move to the "EffectClientInterface" class once all
   // effects have bee converted.
   virtual bool IsInteractive() = 0;

   // I don't really like this, but couldn't think of a better way to force the
   // effect to appear "above the line" in the menus.
   virtual bool IsDefault() = 0;

   // This will go away when all Effects have been updated to the new
   // interface.
   virtual bool IsLegacy() = 0;

   // Whether the effect supports realtime processing (while audio is playing).
   virtual bool IsRealtimeCapable() = 0;
};

class AUDACITY_DLL_API EffectHostInterface : 
   public EffectIdentInterface,
   public ConfigClientInterface
{
public:
   virtual ~EffectHostInterface() {};

   virtual double GetDuration() = 0;
   virtual bool SetDuration(double seconds) = 0;

   virtual bool Apply() = 0;
   virtual void Preview() = 0;
};

typedef float * pfloat;
typedef std::vector<pfloat> pvec;

class EffectClientInterface : public EffectIdentInterface
{
public:
   virtual ~EffectClientInterface() {};

   virtual void SetHost(EffectHostInterface *host) = 0;
   virtual bool Startup() = 0;
   virtual bool Shutdown() = 0;

   virtual int GetAudioInCount() = 0;
   virtual int GetAudioOutCount() = 0;

   virtual int GetMidiInCount() = 0;
   virtual int GetMidiOutCount() = 0;

   virtual void SetSampleRate(sampleCount rate) = 0;
   virtual sampleCount GetBlockSize(sampleCount maxBlockSize) = 0;

   virtual sampleCount GetLatency() = 0;
   virtual sampleCount GetTailSize() = 0;

   virtual bool IsReady() = 0;
   virtual bool ProcessInitialize() = 0;
   virtual bool ProcessFinalize() = 0;
   virtual sampleCount ProcessBlock(float **inbuf, float **outbuf, sampleCount size) = 0;

   virtual bool RealtimeInitialize() = 0;
   virtual bool RealtimeAddProcessor(int numChannels, float sampleRate) = 0;
   virtual bool RealtimeFinalize() = 0;
   virtual bool RealtimeSuspend() = 0;
   virtual bool RealtimeResume() = 0;
   virtual sampleCount RealtimeProcess(int group, float **inbuf, float **outbuf, sampleCount size) = 0;

   virtual bool ShowInterface(void *parent) = 0;
};

class EffectManagerInterface
{
public:
   virtual ~EffectManagerInterface() {};

   virtual void FindFilesInPathList(const wxString & pattern,
                                    const wxArrayString & pathList,
                                    wxArrayString & files,
                                    int searchFlags) = 0;
};

#endif // __AUDACITY_EFFECTINTERFACE_H__
