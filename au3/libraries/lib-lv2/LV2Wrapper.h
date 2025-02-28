/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2Wrapper.h
  @brief manager for a handle to an lv2 plug-in instance and request and
  response queues for inter-thread work scheduling

  Paul Licameli split from LV2Effect.h

  Audacity(R) is copyright (c) 1999-2013 Audacity Team.
  License: GPL v2 or later.  See License.txt.

*********************************************************************/

#ifndef __AUDACITY_LV2_WRAPPER__
#define __AUDACITY_LV2_WRAPPER__

#if USE_LV2

#include "LV2Utils.h"
#include "LV2InstanceFeaturesList.h"

#include "lilv/lilv.h"
#include "lv2/core/attributes.h"
#include "lv2/options/options.h"
#include "lv2/state/state.h"
#include "lv2/worker/worker.h"

#include <thread>
#include <wx/msgqueue.h>

struct EffectOutputs;
struct LV2EffectSettings;
class LV2Ports;
class LV2PortStates;
using LilvInstancePtr = Lilv_ptr<LilvInstance, lilv_instance_free>;

// We use deprecated LV2 interfaces to remain compatible with older
// plug-ins, so disable warnings
LV2_DISABLE_DEPRECATION_WARNINGS

//! Manager of a handle to an LV2 plug-in instantiation
class LV2_API LV2Wrapper final
{
    //! To compel use of the factory
    struct CreateToken {};
public:
    struct LV2Work {
        uint32_t size{};
        const void* data{};
    };

public:
    //! Factory
    static std::unique_ptr<LV2Wrapper> Create(
        LV2InstanceFeaturesList& baseFeatures, const LV2Ports& ports, LV2PortStates& portStates, const LV2EffectSettings& settings,
        float sampleRate, EffectOutputs* pOutputs);

    //! Constructor may spawn a thread
    LV2Wrapper(CreateToken&&, LV2InstanceFeaturesList& baseFeatures, const LilvPlugin& plugin, float sampleRate);

    //! If a thread was started, joins it
    ~LV2Wrapper();

    void ConnectControlPorts(const LV2Ports& ports, const LV2EffectSettings& settings, EffectOutputs* pOutputs);
    void ConnectPorts(const LV2Ports& ports, LV2PortStates& portStates, const LV2EffectSettings& settings, EffectOutputs* pOutputs);
    void Activate();
    void Deactivate();
    LilvInstance& GetInstance() const;
    LV2_Handle GetHandle() const;
    float GetLatency() const;
    void SetFreeWheeling(bool enable);
    void SendBlockSize();
    void ConsumeResponses();
    static LV2_Worker_Status schedule_work(LV2_Worker_Schedule_Handle handle, uint32_t size, const void* data);
    LV2_Worker_Status ScheduleWork(uint32_t size, const void* data);
    static LV2_Worker_Status respond(LV2_Worker_Respond_Handle handle, uint32_t size, const void* data);
    LV2_Worker_Status Respond(uint32_t size, const void* data);

    LV2WrapperFeaturesList& GetFeatures() { return mFeaturesList; }
    const LV2WrapperFeaturesList& GetFeatures() const { return mFeaturesList; }

private:
    void ThreadFunction();

    // Another object with an explicit virtual function table
    LV2_Worker_Schedule mWorkerSchedule{ this, LV2Wrapper::schedule_work };

    LV2WrapperFeaturesList mFeaturesList;

    //! @invariant not null
    const LilvInstancePtr mInstance;
    const LV2_Handle mHandle;

    // Pointers to extension interfaces that the foreign plug-in instance
    // may expose:
    // Options extension
    const LV2_Options_Interface* const mOptionsInterface;
    // State extension
    const LV2_State_Interface* const mStateInterface;
    // Worker extension
    const LV2_Worker_Interface* const mWorkerInterface;

    std::thread mThread;
    wxMessageQueue<LV2Work> mRequests;
    wxMessageQueue<LV2Work> mResponses;
    float mLatency{ 0.0 };

    //! If true, do not spawn extra worker threads
    bool mFreeWheeling{ false };

    //! Written by main thread, read by worker, but atomic isn't needed because
    //! mRequests provides synchronization
    bool mStopWorker{ false };
    bool mActivated{ false };
};

#endif
#endif
