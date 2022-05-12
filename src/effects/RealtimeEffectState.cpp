/**********************************************************************

  Audacity: A Digital Audio Editor

  @file RealtimeEffectState.cpp

  Paul Licameli split from RealtimeEffectManager.cpp

 *********************************************************************/

#include "RealtimeEffectState.h"

#include "EffectInterface.h"
#include "MessageBuffer.h"
#include "PluginManager.h"

//! Mediator of two-way inter-thread communication of changes of settings
class RealtimeEffectState::AccessState : public NonInterferingBase {
public:
   AccessState(const EffectSettingsManager &effect, EffectSettings &settings)
      : mEffect{effect}
      , mSettings{settings}
   {
   }

   void Initialize(const EffectSettings &settings) {
      // Initialize each message buffer with two copies of settings
      mChannelToMain.Write(ToMainSlot{settings});
      mChannelToMain.Write(ToMainSlot{settings});
      mChannelFromMain.Write(FromMainSlot{settings});
      mChannelFromMain.Write(FromMainSlot{settings});
   }

   const EffectSettings &MainRead() {
      // Main thread clones the object in the std::any, then gives a reference
      mChannelToMain.Read<ToMainSlot::Reader>(mMainThreadCache);
      return mMainThreadCache;
   }
   void MainWrite(EffectSettings &&settings) {
      // Main thread may simply swap new content into place
      mChannelFromMain.Write(std::move(settings));
   }

   struct EffectAndSettings{
      const EffectSettingsManager &effect; const EffectSettings &settings; };
   void WorkerRead() {
      // Worker thread avoids memory allocation.  It copies the contents of any
      mChannelFromMain.Read<FromMainSlot::Reader>(mEffect, mSettings);
   }
   void WorkerWrite() {
      // Worker thread avoids memory allocation.  It copies the contents of any
      mChannelToMain.Write(EffectAndSettings{mEffect, mSettings});
   }

private:
   struct ToMainSlot {
      // For initialization of the channel
      ToMainSlot() = default;
      explicit ToMainSlot(const EffectSettings &settings)
         // Copy std::any
         : mSettings{ settings }
      {}
      ToMainSlot &operator=(ToMainSlot &&) = default;

      // Worker thread writes the slot
      ToMainSlot& operator=(EffectAndSettings &&arg) {
         // This happens during MessageBuffer's busying of the slot
         arg.effect.CopySettingsContents(arg.settings, mSettings);
         mSettings.extra = arg.settings.extra;
         return *this;
      }

      // Main thread doesn't move out of the slot, but copies std::any
      // and extra fields
      struct Reader { Reader(ToMainSlot &&slot, EffectSettings &settings) {
         settings = slot.mSettings;
      } };

      EffectSettings mSettings;
   };
   MessageBuffer<ToMainSlot> mChannelToMain;

   struct FromMainSlot {
      // For initialization of the channel
      FromMainSlot() = default;
      explicit FromMainSlot(const EffectSettings &settings)
         // Copy std::any
         : mSettings{ settings }
      {}
      FromMainSlot &operator=(FromMainSlot &&) = default;

      // Main thread writes the slot
      FromMainSlot& operator=(EffectSettings &&settings) {
         mSettings.swap(settings);
         return *this;
      }

      // Worker thread reads the slot
      struct Reader { Reader(FromMainSlot &&slot,
         const EffectSettingsManager &effect, EffectSettings &settings) {
            // This happens during MessageBuffer's busying of the slot
            effect.CopySettingsContents(slot.mSettings, settings);
            settings.extra = slot.mSettings.extra;
      } };

      EffectSettings mSettings;
   };
   MessageBuffer<FromMainSlot> mChannelFromMain;

   const EffectSettingsManager &mEffect;
   EffectSettings &mSettings;
   EffectSettings mMainThreadCache;
};

//! Main thread's interface to inter-thread communication of changes of settings
struct RealtimeEffectState::Access final : EffectSettingsAccess {
   Access() = default;
   explicit Access(std::weak_ptr<AccessState> wState)
      : mwState{ move(wState) } {}
   ~Access() override = default;
   const EffectSettings &Get() override {
      if (auto pState = mwState.lock())
         return pState->MainRead();
      else {
         // Non-modal dialog may have outlived the RealtimeEffectState
         static EffectSettings empty;
         return empty;
      }
   }
   void Set(EffectSettings &&settings) override {
      if (auto pState = mwState.lock())
         pState->MainWrite(std::move(settings));
   }
   std::weak_ptr<AccessState> mwState;
};

RealtimeEffectState::RealtimeEffectState(const PluginID & id)
{
   SetID(id);
}

RealtimeEffectState::RealtimeEffectState(const RealtimeEffectState &other)
  : mID{ other.mID }
  , mPlugin{ other.mPlugin }
  , mSettings{ other.mSettings }
{
}

RealtimeEffectState::~RealtimeEffectState() = default;

void RealtimeEffectState::SetID(const PluginID & id)
{
   bool empty = id.empty();
   if (mID.empty() && !empty) {
      mID = id;
      GetEffect();
   }
   else
      // Set mID to non-empty at most once
      assert(empty);
}

const EffectInstanceFactory *RealtimeEffectState::GetEffect()
{
   if (!mPlugin) {
      mPlugin = EffectFactory::Call(mID);
      if (mPlugin)
         // Also make EffectSettings
         mSettings = mPlugin->MakeSettings();
   }
   return mPlugin;
}

bool RealtimeEffectState::Suspend()
{
   ++mSuspendCount;
   return mSuspendCount != 1 || (mInstance && mInstance->RealtimeSuspend());
}

bool RealtimeEffectState::Resume() noexcept
{
   assert(mSuspendCount > 0);
   --mSuspendCount;
   return mSuspendCount != 0 || (mInstance && mInstance->RealtimeResume());
}

bool RealtimeEffectState::Initialize(double rate)
{
   if (!mPlugin)
      return false;
   mInstance = mPlugin->MakeInstance();
   if (!mInstance)
      return false;

   mCurrentProcessor = 0;
   mGroups.clear();
   mInstance->SetSampleRate(rate);

   // PRL: conserving pre-3.2.0 behavior, but I don't know why this arbitrary
   // number was important
   mInstance->SetBlockSize(512);

   return mInstance->RealtimeInitialize(mSettings);
}

//! Set up processors to be visited repeatedly in Process.
/*! The iteration over channels in AddTrack and Process must be the same */
bool RealtimeEffectState::AddTrack(Track &track, unsigned chans, float rate)
{
   if (!mPlugin || !mInstance)
      return false;

   auto ichans = chans;
   auto ochans = chans;
   auto gchans = chans;

   mGroups[&track] = mCurrentProcessor;

   const auto numAudioIn = mPlugin->GetAudioInCount();
   const auto numAudioOut = mPlugin->GetAudioOutCount();

   // Call the client until we run out of input or output channels
   while (ichans > 0 && ochans > 0)
   {
      // If we don't have enough input channels to accommodate the client's
      // requirements, then we replicate the input channels until the
      // client's needs are met.
      if (ichans < numAudioIn)
      {
         // All input channels have been consumed
         ichans = 0;
      }
      // Otherwise fulfill the client's needs with as many input channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ichans >= numAudioIn)
      {
         gchans = numAudioIn;
         ichans -= gchans;
      }

      // If we don't have enough output channels to accommodate the client's
      // requirements, then we provide all of the output channels and fulfill
      // the client's needs with dummy buffers.  These will just get tossed.
      if (ochans < numAudioOut)
      {
         // All output channels have been consumed
         ochans = 0;
      }
      // Otherwise fulfill the client's needs with as many output channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ochans >= numAudioOut)
      {
         ochans -= numAudioOut;
      }

      // Add a NEW processor
      mInstance->RealtimeAddProcessor(mSettings, gchans, rate);
      mCurrentProcessor++;
   }

   return true;
}

bool RealtimeEffectState::ProcessStart()
{
   if (!mInstance)
      return false;

   // Get state changes from the main thread
   // But skip this if we never gave out Access, or it has expired
   if (!mwAccess.expired())
      mpAccessState->WorkerRead();

   return mInstance->RealtimeProcessStart(mSettings);
}

//! Visit the effect processors that were added in AddTrack
/*! The iteration over channels in AddTrack and Process must be the same */
size_t RealtimeEffectState::Process(Track &track,
   unsigned chans,
   const float *const *inbuf, float *const *outbuf, float *dummybuf,
   size_t numSamples)
{
   if (!mPlugin || !mInstance) {
      for (size_t ii = 0; ii < chans; ++ii)
         memcpy(outbuf[ii], inbuf[ii], numSamples * sizeof(float));
      return numSamples; // consider all samples to be trivially processed
   }

   // The caller passes the number of channels to process and specifies
   // the number of input and output buffers.  There will always be the
   // same number of output buffers as there are input buffers.
   //
   // Effects always require a certain number of input and output buffers,
   // so if the number of channels we're currently processing are different
   // than what the effect expects, then we use a few methods of satisfying
   // the effects requirements.
   const auto numAudioIn = mPlugin->GetAudioInCount();
   const auto numAudioOut = mPlugin->GetAudioOutCount();

   const auto clientIn =
      static_cast<const float **>(alloca(numAudioIn * sizeof(float *)));
   const auto clientOut =
      static_cast<float **>(alloca(numAudioOut * sizeof(float *)));
   decltype(numSamples) len = 0;
   auto ichans = chans;
   auto ochans = chans;
   auto gchans = chans;
   unsigned indx = 0;
   unsigned ondx = 0;

   auto processor = mGroups[&track];

   // Call the client until we run out of input or output channels
   while (ichans > 0 && ochans > 0)
   {
      // If we don't have enough input channels to accommodate the client's
      // requirements, then we replicate the input channels until the
      // client's needs are met.
      if (ichans < numAudioIn)
      {
         for (size_t i = 0; i < numAudioIn; i++)
         {
            if (indx == ichans)
            {
               indx = 0;
            }
            clientIn[i] = inbuf[indx++];
         }

         // All input channels have been consumed
         ichans = 0;
      }
      // Otherwise fulfill the client's needs with as many input channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ichans >= numAudioIn)
      {
         gchans = 0;
         for (size_t i = 0; i < numAudioIn; i++, ichans--, gchans++)
         {
            clientIn[i] = inbuf[indx++];
         }
      }

      // If we don't have enough output channels to accommodate the client's
      // requirements, then we provide all of the output channels and fulfill
      // the client's needs with dummy buffers.  These will just get tossed.
      if (ochans < numAudioOut)
      {
         for (size_t i = 0; i < numAudioOut; i++)
         {
            if (i < ochans)
            {
               clientOut[i] = outbuf[i];
            }
            else
            {
               clientOut[i] = dummybuf;
            }
         }

         // All output channels have been consumed
         ochans = 0;
      }
      // Otherwise fulfill the client's needs with as many output channels as possible.
      // After calling the client with this set, we will loop back up to process more
      // of the input/output channels.
      else if (ochans >= numAudioOut)
      {
         for (size_t i = 0; i < numAudioOut; i++, ochans--)
         {
            clientOut[i] = outbuf[ondx++];
         }
      }

      // Finally call the plugin to process the block
      len = 0;
      const auto blockSize = mInstance->GetBlockSize();
      for (decltype(numSamples) block = 0; block < numSamples; block += blockSize)
      {
         auto cnt = std::min(numSamples - block, blockSize);
         len += mInstance->RealtimeProcess(processor,
            mSettings, clientIn, clientOut, cnt);

         for (size_t i = 0 ; i < numAudioIn; i++)
         {
            clientIn[i] += cnt;
         }

         for (size_t i = 0 ; i < numAudioOut; i++)
         {
            clientOut[i] += cnt;
         }
      }
      processor++;
   }

   return len;
}

bool RealtimeEffectState::ProcessEnd()
{
   if (!mInstance)
      return false;

   auto result = mInstance->RealtimeProcessEnd(mSettings);
   if (result && !mwAccess.expired())
      // Some dialogs require communication back from the processor so that
      // they can update their appearance in idle time, and some plug-in
      // libraries (like lv2) require the host program to mediate the
      // communication
      // But skip this if we never gave out Access, or it has expired
      mpAccessState->WorkerWrite();

   return result;
}

bool RealtimeEffectState::IsActive() const noexcept
{
   return mSuspendCount == 0;
}

bool RealtimeEffectState::Finalize() noexcept
{
   mGroups.clear();

   if (!mInstance)
      return false;

   auto result = mInstance->RealtimeFinalize(mSettings);
   mInstance.reset();
   return result;
}

const std::string &RealtimeEffectState::XMLTag()
{
   static const std::string result{"effect"};
   return result;
}

static const auto idAttribute = "id";
static const auto versionAttribute = "version";
static const auto parametersAttribute = "parameters";
static const auto parameterAttribute = "parameter";
static const auto nameAttribute = "name";
static const auto valueAttribute = "value";

bool RealtimeEffectState::HandleXMLTag(
   const std::string_view &tag, const AttributesList &attrs)
{
   if (tag == XMLTag()) {
      mParameters.clear();
      mPlugin = nullptr;
      mID.clear();

      for (auto pair : attrs) {
         auto attr = pair.first;
         auto value = pair.second;

         if (attr == idAttribute) {
            SetID(value.ToWString());
            if (!mPlugin) {
               // TODO - complain!!!!
            }
         }
         else if (attr == versionAttribute) {
         }
      }

      return true;
   }
   else if (tag == parametersAttribute)
      return true;
   else if (tag == parameterAttribute) {
      wxString n;
      wxString v;

      for (auto pair : attrs) {
         auto attr = pair.first;
         auto value = pair.second;

         if (attr == nameAttribute)
            n = value.ToWString();
         else if (attr == valueAttribute)
            v = value.ToWString();
      }

      mParameters += wxString::Format(wxT("\"%s=%s\" "), n, v);

      return true;
   }
   else
      return false;
}

void RealtimeEffectState::HandleXMLEndTag(const std::string_view &tag)
{
   if (tag == XMLTag()) {
      if (mPlugin && !mParameters.empty()) {
         CommandParameters parms(mParameters);
         mPlugin->LoadSettings(parms, mSettings);
      }
      mParameters.clear();
   }
}

XMLTagHandler *RealtimeEffectState::HandleXMLChild(const std::string_view &tag)
{
   // Tag may be for the state, or the list of parameters, or for one parameter.
   // See the writing method below.  All are handled by this
   return this;
}

void RealtimeEffectState::WriteXML(XMLWriter &xmlFile)
{
   if (!mPlugin)
      return;

   xmlFile.StartTag(XMLTag());
   xmlFile.WriteAttr(
      idAttribute, XMLWriter::XMLEsc(PluginManager::GetID(mPlugin)));
   xmlFile.WriteAttr(versionAttribute, XMLWriter::XMLEsc(mPlugin->GetVersion()));

   CommandParameters cmdParms;
   if (mPlugin->SaveSettings(mSettings, cmdParms)) {
      xmlFile.StartTag(parametersAttribute);

      wxString entryName;
      long entryIndex;
      bool entryKeepGoing;

      entryKeepGoing = cmdParms.GetFirstEntry(entryName, entryIndex);
      while (entryKeepGoing) {
         wxString entryValue = cmdParms.Read(entryName, "");

         xmlFile.StartTag(parameterAttribute);
         xmlFile.WriteAttr(nameAttribute, XMLWriter::XMLEsc(entryName));
         xmlFile.WriteAttr(valueAttribute, XMLWriter::XMLEsc(entryValue));
         xmlFile.EndTag(parameterAttribute);

         entryKeepGoing = cmdParms.GetNextEntry(entryName, entryIndex);
      }

      xmlFile.EndTag(parametersAttribute);
   }

   xmlFile.EndTag(XMLTag());
}

std::shared_ptr<EffectSettingsAccess> RealtimeEffectState::GetAccess()
{
   if (!GetEffect())
      // Effect not found!  Return a dummy
      return std::make_shared<Access>();

   // Initialize once in the lifetime of `this`
   if (!mpAccessState)
      mpAccessState = std::make_unique<AccessState>(*mPlugin, mSettings);

   // Reinitialize
   mpAccessState->Initialize(mSettings);

   auto result = std::make_shared<Access>(mpAccessState);
   mwAccess = result;
   return result;
}
