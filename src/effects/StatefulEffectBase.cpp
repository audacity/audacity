/**********************************************************************

  Audacity: A Digital Audio Editor

  StatefulEffectBase.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from Effect.cpp

**********************************************************************/


#include "StatefulEffectBase.h"

#include <algorithm>

#include <wx/defs.h>
#include <wx/sizer.h>

#include "ConfigInterface.h"
#include "../LabelTrack.h"
#include "../ProjectSettings.h"
#include "../SelectFile.h"
#include "../ShuttleAutomation.h"
#include "../ShuttleGui.h"
#include "../SyncLock.h"
#include "ViewInfo.h"
#include "../WaveTrack.h"
#include "wxFileNameWrapper.h"
#include "../widgets/ProgressDialog.h"
#include "../widgets/NumericTextCtrl.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/VetoDialogHook.h"

#include <unordered_map>

static const int kPlayID = 20102;
static const int kRewindID = 20103;
static const int kFFwdID = 20104;

using t2bHash = std::unordered_map< void*, bool >;

StatefulEffectBase::Instance::Instance(StatefulEffectBase &effect)
   : mEffect{ effect }
{
}

StatefulEffectBase::Instance::~Instance() = default;

bool StatefulEffectBase::Instance::Init()
{
   return GetEffect().Init();
}

bool StatefulEffectBase::Instance::RealtimeInitialize(
   EffectSettings &settings, double sampleRate)
{
   return GetEffect().RealtimeInitialize(settings, sampleRate);
}

bool StatefulEffectBase::Instance::RealtimeAddProcessor(EffectSettings &settings,
   unsigned numChannels, float sampleRate)
{
   return GetEffect().RealtimeAddProcessor(settings, numChannels, sampleRate);
}

bool StatefulEffectBase::Instance::RealtimeSuspend()
{
   return GetEffect().RealtimeSuspend();
}

bool StatefulEffectBase::Instance::RealtimeResume()
{
   return GetEffect().RealtimeResume();
}

bool StatefulEffectBase::Instance::RealtimeProcessStart(EffectSettings &settings)
{
   return GetEffect().RealtimeProcessStart(settings);
}

size_t StatefulEffectBase::Instance::RealtimeProcess(size_t group,
   EffectSettings &settings,
   const float *const *inBuf, float *const *outBuf, size_t numSamples)
{
   return GetEffect()
      .RealtimeProcess(group, settings, inBuf, outBuf, numSamples);
}

bool StatefulEffectBase::Instance::RealtimeProcessEnd(EffectSettings &settings) noexcept
{
   return GetEffect().RealtimeProcessEnd(settings);
}

bool StatefulEffectBase::Instance::RealtimeFinalize(EffectSettings &settings) noexcept
{
   return GetEffect().RealtimeFinalize(settings);
}

size_t StatefulEffectBase::Instance::GetBlockSize() const
{
   return GetEffect().GetBlockSize();
}

size_t StatefulEffectBase::Instance::SetBlockSize(size_t maxBlockSize)
{
   return GetEffect().SetBlockSize(maxBlockSize);
}

size_t StatefulEffectBase::SetBlockSize(size_t maxBlockSize)
{
   mEffectBlockSize = maxBlockSize;
   return mEffectBlockSize;
}

size_t StatefulEffectBase::GetBlockSize() const
{
   return mEffectBlockSize;
}

bool StatefulEffectBase::RealtimeInitialize(EffectSettings &, double)
{
   return false;
}

bool StatefulEffectBase::RealtimeAddProcessor(
   EffectSettings &settings, unsigned numChannels, float sampleRate)
{
   return true;
}

bool StatefulEffectBase::RealtimeSuspend()
{
   return true;
}

bool StatefulEffectBase::RealtimeResume()
{
   return true;
}

bool StatefulEffectBase::RealtimeProcessStart(EffectSettings &settings)
{
   return true;
}

size_t StatefulEffectBase::RealtimeProcess(size_t group,
   EffectSettings &settings,
   const float *const *inbuf, float *const *outbuf, size_t numSamples)
{
   return 0;
}

bool StatefulEffectBase::RealtimeProcessEnd(EffectSettings &settings) noexcept
{
   return true;
}

bool StatefulEffectBase::RealtimeFinalize(EffectSettings &settings) noexcept
{
   return false;
}

bool StatefulEffectBase::Init()
{
   return true;
}
