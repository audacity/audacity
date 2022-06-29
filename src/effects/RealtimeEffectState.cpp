/**********************************************************************

  Audacity: A Digital Audio Editor

  @file RealtimeEffectState.cpp

  Paul Licameli split from RealtimeEffectManager.cpp

 *********************************************************************/

#include "RealtimeEffectState.h"

#include "AudioIOBase.h"
#include "EffectInterface.h"
#include "MessageBuffer.h"
#include "PluginManager.h"

#include <chrono>
#include <thread>

//! Mediator of two-way inter-thread communication of changes of settings
class RealtimeEffectState::AccessState : public NonInterferingBase {
public:
   AccessState(const EffectSettingsManager &effect,
      EffectSettings &mainSettings, EffectSettings &workerSettings)
      : mEffect{ effect }
      , mWorkerSettings{ workerSettings }
   {
      // Clean initial state of the counter
      mainSettings.extra.SetCounter(0);
      // Initialize each message buffer with two copies of settings
      mChannelToMain.Write(ToMainSlot{ mainSettings });
      mChannelToMain.Write(ToMainSlot{ mainSettings });
      mChannelFromMain.Write(FromMainSlot{ mainSettings });
      mChannelFromMain.Write(FromMainSlot{ mainSettings });
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
   const EffectSettings &MainWriteThrough(const EffectSettings &settings) {
      // Send a copy of settings for worker to update from later
      MainWrite(EffectSettings{ settings });
      // Main thread assumes worker isn't processing and bypasses the echo
      return (mMainThreadCache = settings);
   }

   struct EffectAndSettings{
      const EffectSettingsManager &effect; const EffectSettings &settings; };
   void WorkerRead() {
      // Worker thread avoids memory allocation.  It copies the contents of any
      mChannelFromMain.Read<FromMainSlot::Reader>(mEffect, mWorkerSettings);
   }
   void WorkerWrite() {
      // Worker thread avoids memory allocation.  It copies the contents of any
      mChannelToMain.Write(EffectAndSettings{ mEffect, mWorkerSettings });
   }

   EffectSettings mLastSettings;
   const EffectSettings &MainThreadCache() const { return mMainThreadCache; }

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
   EffectSettings &mWorkerSettings;
   EffectSettings mMainThreadCache;
};

//! Main thread's interface to inter-thread communication of changes of settings
struct RealtimeEffectState::Access final : EffectSettingsAccess {
   Access() = default;
   explicit Access(RealtimeEffectState &state)
      : mwState{ state.weak_from_this() }
   {
   }
   ~Access() override = default;
   // Try once to detect that the worker thread has echoed the last write
   static const EffectSettings *FlushAttempt(AccessState &state) {
      auto &lastSettings = state.mLastSettings;
      auto pResult = &state.MainRead();
      if (!lastSettings.has_value() ||
         pResult->extra.GetCounter() ==
         lastSettings.extra.GetCounter()
      ){
         // First time test, or echo is completed
         return pResult;
      }
      else if (auto pAudioIO = AudioIOBase::Get()
         ; !(pAudioIO && pAudioIO->IsStreamActive())
      ){
         // not relying on the other thread to make progress
         // and no fear of data races
         return &state.MainWriteThrough(lastSettings);
      }
      return nullptr;
   }
   const EffectSettings &Get() override {
      if (auto pState = mwState.lock()) {
         if (auto pAccessState = pState->GetAccessState()) {
            FlushAttempt(*pAccessState); // try once, ignore success
            auto &lastSettings = pAccessState->mLastSettings;
            return lastSettings.has_value()
               ? lastSettings
               // If no value there yet, then FlushAttempt did MainRead which
               // has copied the initial value given to the constructor
               : pAccessState->MainThreadCache();
         }
      }
      // Non-modal dialog may have outlived the RealtimeEffectState
      static EffectSettings empty;
      return empty;
   }
   void Set(EffectSettings &&settings) override {
      if (auto pState = mwState.lock())
         if (auto pAccessState = pState->GetAccessState()) {
            auto &lastSettings = pAccessState->mLastSettings;
            auto lastCounter = lastSettings.extra.GetCounter();
            settings.extra.SetCounter(++lastCounter);
            // move to remember values here
            lastSettings = std::move(settings);
            // move a copy to there
            pAccessState->MainWrite(EffectSettings{ lastSettings });
         }
   }
   void Flush() override {
      if (auto pState = mwState.lock()) {
         if (auto pAccessState = pState->GetAccessState()) {
            auto &lastSettings = pAccessState->mLastSettings;
            const EffectSettings *pResult{};
            while (!(pResult = FlushAttempt(*pAccessState))) {
               // Wait for progress of audio thread
               using namespace std::chrono;
               std::this_thread::sleep_for(50ms);
            }
            pState->mMainSettings.Set(*pResult); // Update the state
         }
      }
   }
   bool IsSameAs(const EffectSettingsAccess &other) const override {
      if (auto pOther = dynamic_cast<const Access*>(&other)) {
         auto &mine = mwState;
         auto &theirs = pOther->mwState;
         auto less = std::owner_less{};
         return !(less(mine, theirs) || less(theirs, mine));
      }
      return false;
   }
   //! Store no state here but this weak pointer, so `IsSameAs` isn't lying
   std::weak_ptr<RealtimeEffectState> mwState;
};

RealtimeEffectState::RealtimeEffectState(const PluginID & id)
{
   SetID(id);
}

RealtimeEffectState::RealtimeEffectState(const RealtimeEffectState &other)
  : mID{ other.mID }
  , mPlugin{ other.mPlugin }
  , mMainSettings{ other.mMainSettings }
{
   // Do not copy mWorkerSettings
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

const PluginID& RealtimeEffectState::GetID() const noexcept
{
   return mID;
}


const EffectInstanceFactory *RealtimeEffectState::GetEffect()
{
   if (!mPlugin) {
      mPlugin = EffectFactory::Call(mID);
      if (mPlugin) {
         // Also make EffectSettings, but preserve activation
         auto wasActive = mMainSettings.extra.GetActive();
         mMainSettings.Set(mPlugin->MakeSettings());
         mMainSettings.extra.SetActive(wasActive);
      }
   }
   return mPlugin;
}

std::shared_ptr<EffectInstance>
RealtimeEffectState::EnsureInstance(double sampleRate)
{
   if (!mPlugin)
      return {};

   auto pInstance = mwInstance.lock();
   if (!mInitialized) {
      //! copying settings in the main thread while worker isn't yet running
      mWorkerSettings = mMainSettings;
      mLastActive = IsActive();
      
      //! If there was already an instance, recycle it; else make one here
      if (!pInstance)
         mwInstance = pInstance = mPlugin->MakeInstance();
      if (!pInstance)
         return {};

      mInitialized = true;
      
      // PRL: conserving pre-3.2.0 behavior, but I don't know why this arbitrary
      // number was important
      pInstance->SetBlockSize(512);
      
      if (!pInstance->RealtimeInitialize(mMainSettings, sampleRate))
         return {};
      return pInstance;
   }
   return pInstance;
}

std::shared_ptr<EffectInstance> RealtimeEffectState::GetInstance()
{
   //! If there was already an instance, recycle it; else make one here
   auto pInstance = mwInstance.lock();
   if (!pInstance)
      mwInstance = pInstance = mPlugin->MakeInstance();
   return pInstance;
}

std::shared_ptr<EffectInstance>
RealtimeEffectState::Initialize(double sampleRate)
{
   if (!mPlugin)
      return {};

   mCurrentProcessor = 0;
   mGroups.clear();
   return EnsureInstance(sampleRate);
}

//! Set up processors to be visited repeatedly in Process.
/*! The iteration over channels in AddTrack and Process must be the same */
std::shared_ptr<EffectInstance>
RealtimeEffectState::AddTrack(Track &track, unsigned chans, float sampleRate)
{
   auto pInstance = EnsureInstance(sampleRate);
   if (!pInstance)
      return {};

   if (!mPlugin)
      return {};

   auto ichans = chans;
   auto ochans = chans;
   auto gchans = chans;

   auto first = mCurrentProcessor;

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
      // Pass reference to worker settings, not main -- such as, for connecting
      // Ladspa ports to the proper addresses.
      if (pInstance->RealtimeAddProcessor(mWorkerSettings, gchans, sampleRate))
         mCurrentProcessor++;
      else
         break;
   }

   if (mCurrentProcessor > first) {
      mGroups[&track] = first;
      return pInstance;
   }
   return {};
}

bool RealtimeEffectState::ProcessStart(bool running)
{
   // Get state changes from the main thread
   // Note that it is only here that the answer of IsActive() may be changed,
   // and it is important that for each state the answer is unchanging in one
   // processing scope.
   if (auto pAccessState = TestAccessState())
      pAccessState->WorkerRead();
   
   // Detect transitions of activity state
   auto pInstance = mwInstance.lock();
   bool active = IsActive() && running;
   if (active != mLastActive) {
      if (pInstance) {
         bool success = active
            ? pInstance->RealtimeResume()
            : pInstance->RealtimeSuspend();
         if (!success)
            return false;
      }
      mLastActive = active;
   }

   if (!pInstance || !active)
      return false;

   // Assuming we are in a processing scope, use the worker settings
   return pInstance->RealtimeProcessStart(mWorkerSettings);
}

//! Visit the effect processors that were added in AddTrack
/*! The iteration over channels in AddTrack and Process must be the same */
size_t RealtimeEffectState::Process(Track &track,
   unsigned chans,
   const float *const *inbuf, float *const *outbuf, float *dummybuf,
   size_t numSamples)
{
   auto pInstance = mwInstance.lock();
   if (!mPlugin || !pInstance || !mLastActive) {
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
      const auto blockSize = pInstance->GetBlockSize();
      for (decltype(numSamples) block = 0; block < numSamples; block += blockSize)
      {
         auto cnt = std::min(numSamples - block, blockSize);
         // Assuming we are in a processing scope, use the worker settings
         len += pInstance->RealtimeProcess(processor,
            mWorkerSettings, clientIn, clientOut, cnt);

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
   auto pInstance = mwInstance.lock();
   bool result = pInstance && IsActive() && mLastActive &&
      // Assuming we are in a processing scope, use the worker settings
      pInstance->RealtimeProcessEnd(mWorkerSettings);

   if (auto pAccessState = TestAccessState())
      // Always done, regardless of activity
      // Some dialogs require communication back from the processor so that
      // they can update their appearance in idle time, and some plug-in
      // libraries (like lv2) require the host program to mediate the
      // communication
      pAccessState->WorkerWrite();

   return result;
}

bool RealtimeEffectState::IsActive() const noexcept
{
   return mWorkerSettings.extra.GetActive();
}

bool RealtimeEffectState::Finalize() noexcept
{
   // This is the main thread cleaning up a state not now used in processing
   mMainSettings = mWorkerSettings;

   mGroups.clear();
   mCurrentProcessor = 0;

   auto pInstance = mwInstance.lock();
   if (!pInstance)
      return false;

   auto result = pInstance->RealtimeFinalize(mMainSettings);
   mInitialized = false;
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
static constexpr auto activeAttribute = "active";

bool RealtimeEffectState::HandleXMLTag(
   const std::string_view &tag, const AttributesList &attrs)
{
   if (tag == XMLTag()) {
      mParameters.clear();
      mPlugin = nullptr;
      mID.clear();
      for (auto &[attr, value] : attrs) {
         if (attr == idAttribute) {
            SetID(value.ToWString());
            if (!mPlugin) {
               // TODO - complain!!!!
            }
         }
         else if (attr == versionAttribute) {
         }
         else if (attr == activeAttribute)
            // Updating the EffectSettingsExtra although we haven't yet built
            // the settings
            mMainSettings.extra.SetActive(value.Get<bool>());
      }
      return true;
   }
   else if (tag == parametersAttribute)
      return true;
   else if (tag == parameterAttribute) {
      wxString n;
      wxString v;
      for (auto &[attr, value] : attrs) {
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
         mPlugin->LoadSettings(parms, mMainSettings);
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
   const auto active = mMainSettings.extra.GetActive();
   xmlFile.WriteAttr(activeAttribute, active);
   xmlFile.WriteAttr(
      idAttribute, XMLWriter::XMLEsc(PluginManager::GetID(mPlugin)));
   xmlFile.WriteAttr(versionAttribute, XMLWriter::XMLEsc(mPlugin->GetVersion()));

   CommandParameters cmdParms;
   if (mPlugin->SaveSettings(mMainSettings, cmdParms)) {
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

   // Only the main thread assigns to the atomic pointer, here and
   // once only in the lifetime of the state
   if (!GetAccessState())
      mpAccessState.emplace(*mPlugin, mMainSettings, mWorkerSettings);
   return std::make_shared<Access>(*this);
}
