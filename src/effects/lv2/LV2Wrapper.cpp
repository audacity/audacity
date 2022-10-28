/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2Wrapper.cpp

  Paul Licameli split from LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/

#if defined(USE_LV2)

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__clang__)
#pragma clang diagnostic ignored "-Wparentheses"
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include "LV2Wrapper.h"
#include "LV2FeaturesList.h"
#include "LV2Ports.h"

#if defined(__WXMSW__)
#include <wx/msw/wrapwin.h>
#endif

std::unique_ptr<LV2Wrapper> LV2Wrapper::Create(
   LV2InstanceFeaturesList &baseFeatures,
   const LV2Ports &ports, LV2PortStates &portStates,
   const LV2EffectSettings &settings, float sampleRate,
   EffectOutputs *pOutputs)
{
   auto &plug = baseFeatures.mPlug;

   std::unique_ptr<LV2Wrapper> wrapper;
   try {
      wrapper = std::make_unique<LV2Wrapper>(CreateToken{},
         baseFeatures, plug, sampleRate);
   } catch(const std::exception&) {
      return nullptr;
   }

   const auto instance = &wrapper->GetInstance();
   wrapper->SendBlockSize();
   wrapper->ConnectPorts(ports, portStates, settings, pOutputs);

   // Give plugin a chance to initialize.  The SWH plugins (like AllPass) need
   // this before it can be safely deleted.
   lilv_instance_activate(instance);
   lilv_instance_deactivate(instance);

   // Send to the dialog whatever was the result of that "pulse"
   for (auto & state : portStates.mAtomPortStates)
      state->ReceiveFromInstance();

   return wrapper;
}

void LV2Wrapper::ConnectControlPorts(
   const LV2Ports &ports, const LV2EffectSettings &settings,
   EffectOutputs *pOutputs)
{
   const auto instance = &GetInstance();
   static float blackHole;
   auto pValues = static_cast<LV2EffectOutputs *>(pOutputs);

   // Connect all control ports
   const auto latencyPort = ports.mLatencyPort;
   if (latencyPort >= 0)
      lilv_instance_connect_port(instance, latencyPort, &mLatency);

   auto &values = settings.values;
   size_t index = 0;
   for (auto & port : ports.mControlPorts) {
      void *const location = port->mIsInput
         // Settings slots for input ports must pass to the library
         // as nominal pointers to non-const
         ? &const_cast<float&>(values[index])
         : pValues ? &pValues->values[index]
         : &blackHole
      ;
      lilv_instance_connect_port(instance, port->mIndex, location);
      ++index;
   }
}

void LV2Wrapper::ConnectPorts(const LV2Ports &ports, LV2PortStates &portStates,
   const LV2EffectSettings &settings, EffectOutputs *pOutputs)
{
   ConnectControlPorts(ports, settings, pOutputs);

   const auto instance = &GetInstance();

   // Connect all atom ports
   for (auto & state : portStates.mAtomPortStates)
      lilv_instance_connect_port(instance,
         state->mpPort->mIndex, state->mBuffer.get());

   // We don't fully support CV ports, so connect them to dummy buffers for now.
   for (auto & state : portStates.mCVPortStates)
      lilv_instance_connect_port(instance, state.mpPort->mIndex,
         state.mBuffer.get());
}

LV2Wrapper::~LV2Wrapper()
{
   if (mInstance) {
      if (mThread.joinable()) {
         // Even if we have been freewheeling, this unblocks the unused thread
         // so it can be joined
         mStopWorker = true;
         mRequests.Post({ 0, NULL });  // Must do after writing mStopWorker
         mThread.join();
      }
      Deactivate();
   }
}

LV2Wrapper::LV2Wrapper(CreateToken&&, LV2InstanceFeaturesList &baseFeatures,
   const LilvPlugin &plugin, float sampleRate
)  : mFeaturesList{ baseFeatures, sampleRate, &mWorkerSchedule }
, mInstance{ [&instanceFeaturesList = mFeaturesList, &plugin, sampleRate](){
   auto features = instanceFeaturesList.GetFeaturePointers();

#if defined(__WXMSW__)
   // Plugins may have dependencies that need to be loaded from the same path
   // as the main DLL, so add this plugin's path to the DLL search order.
   const auto libNode = lilv_plugin_get_library_uri(&plugin);
   const auto libUri = lilv_node_as_uri(libNode);
   LilvCharsPtr libPath{ lilv_file_uri_parse(libUri, nullptr) };
   const auto path = wxPathOnly(libPath.get());
   SetDllDirectory(path.c_str());
   auto cleanup = finally([]{ SetDllDirectory(nullptr); });
#endif

   auto result = lilv_plugin_instantiate(&plugin, sampleRate, features.data());
   return result ? result : throw std::exception{};
}()}
, mHandle{ lilv_instance_get_handle(mInstance.get()) }
, mOptionsInterface{ static_cast<const LV2_Options_Interface *>(
   lilv_instance_get_extension_data(mInstance.get(), LV2_OPTIONS__interface))
}
, mStateInterface{ static_cast<const LV2_State_Interface *>(
   lilv_instance_get_extension_data(mInstance.get(), LV2_STATE__interface))
}
, mWorkerInterface{ static_cast<const LV2_Worker_Interface *>(
   lilv_instance_get_extension_data(mInstance.get(), LV2_WORKER__interface))
}
{
   if (mWorkerInterface)
      mThread = std::thread{
         std::mem_fn( &LV2Wrapper::ThreadFunction ), std::ref(*this)
      };
}

void LV2Wrapper::Activate()
{
   if (!mActivated) {
      lilv_instance_activate(&GetInstance());
      mActivated = true;
   }
}

void LV2Wrapper::Deactivate()
{
   if (mActivated) {
      lilv_instance_deactivate(&GetInstance());
      mActivated = false;
   }
}

LilvInstance &LV2Wrapper::GetInstance() const
{
   return *mInstance;
}

LV2_Handle LV2Wrapper::GetHandle() const
{
   return mHandle;
}

float LV2Wrapper::GetLatency() const
{
   return mLatency;
}

// Where is this called?
void LV2Wrapper::SetFreeWheeling(bool enable)
{
   mFreeWheeling = enable;
}

void LV2Wrapper::SendBlockSize()
{
   if (auto pOption = mFeaturesList.Base().NominalBlockLengthOption()
      ; pOption && mOptionsInterface && mOptionsInterface->set
   ){
      LV2_Options_Option options[2]{ *pOption, {} };
      // I assume the pointer to temporary options is not retained by
      // well written plug-ins, but they may watch the location at the pointer
      // that is in the option structure.
      mOptionsInterface->set(mHandle, options);
   }
}

// Thread body
void LV2Wrapper::ThreadFunction()
{
   for (LV2Work work{};
      // Must test mStopWorker only after reading mRequests
      mRequests.Receive(work) == wxMSGQUEUE_NO_ERROR && !mStopWorker;
   )
      // Call foreign instance code in this thread, which is neither the
      // main nor the audio thread
      mWorkerInterface->work(mHandle, respond, this, work.size, work.data);
}

void LV2Wrapper::ConsumeResponses()
{
   if (mWorkerInterface) {
      LV2Work work{};
      while (mResponses.ReceiveTimeout(0, work) == wxMSGQUEUE_NO_ERROR)
         // Invoke foreign instance code in main (destructive) or
         // audio thread (real-time) processing
         mWorkerInterface->work_response(mHandle, work.size, work.data);
      if (mWorkerInterface->end_run)
         // More foreign code
         mWorkerInterface->end_run(mHandle);
   }
}

// static callback
LV2_Worker_Status LV2Wrapper::schedule_work(LV2_Worker_Schedule_Handle handle,
   uint32_t size, const void *data)
{
   return static_cast<LV2Wrapper *>(handle)->ScheduleWork(size, data);
}

LV2_Worker_Status LV2Wrapper::ScheduleWork(uint32_t size, const void *data)
{
   if (mFreeWheeling)
      // Not using another thread
      return mWorkerInterface->work(mHandle, respond, this, size, data);
   else {
      // Put in the queue for the worker thread
      // which will then do mWorkerInterface->work
      const auto err = mRequests.Post({ size, data });
      return (err == wxMSGQUEUE_NO_ERROR)
         ? LV2_WORKER_SUCCESS : LV2_WORKER_ERR_UNKNOWN;
   }
}

// static callback given to mWorkerInterface->work and
// called back by the foreign instance code
LV2_Worker_Status LV2Wrapper::respond(
   LV2_Worker_Respond_Handle handle, uint32_t size, const void *data)
{
   return static_cast<LV2Wrapper*>(handle)->Respond(size, data);
}

LV2_Worker_Status LV2Wrapper::Respond(uint32_t size, const void *data)
{
   // Put in the queue, for another thread -- when not "freewheeling."
   // Otherwise it is just roundabout communication within a thread
   const auto err = mResponses.Post({ size, data });
   return (err == wxMSGQUEUE_NO_ERROR)
      ? LV2_WORKER_SUCCESS : LV2_WORKER_ERR_UNKNOWN;
}

#endif
