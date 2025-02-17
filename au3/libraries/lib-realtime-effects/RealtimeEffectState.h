/**********************************************************************

 Audacity: A Digital Audio Editor

 @file RealtimeEffectState.h

 Paul Licameli split from RealtimeEffectManager.cpp

 *********************************************************************/

#ifndef __AUDACITY_REALTIMEEFFECTSTATE_H__
#define __AUDACITY_REALTIMEEFFECTSTATE_H__

#include <atomic>
#include <cstddef>
#include <optional>
#include <unordered_map>
#include <vector>
#include "ClientData.h"
#include "EffectInterface.h"
#include "GlobalVariable.h"
#include "MemoryX.h"
#include "Observer.h"
#include "PluginProvider.h" // for PluginID
#include "XMLTagHandler.h"

class ChannelGroup;
class EffectSettingsAccess;

enum class RealtimeEffectStateChange {
    EffectOff, EffectOn
};

class REALTIME_EFFECTS_API RealtimeEffectState : public XMLTagHandler, public std::enable_shared_from_this<RealtimeEffectState>,
    public SharedNonInterfering<RealtimeEffectState>, public ClientData::Site<RealtimeEffectState>,
    public Observer::Publisher<RealtimeEffectStateChange>
{
public:
    struct REALTIME_EFFECTS_API EffectFactory : GlobalHook<EffectFactory,
                                                           const EffectInstanceFactory* (const PluginID&)
                                                           > {};

    explicit RealtimeEffectState(const PluginID& id);
    RealtimeEffectState(const RealtimeEffectState& other) = delete;
    RealtimeEffectState& operator =(const RealtimeEffectState& other) = delete;
    ~RealtimeEffectState();

    //! May be called with nonempty id at most once in the lifetime of a state
    /*!
     Call with empty id is ignored.
     Called by the constructor that takes an id */
    void SetID(const PluginID& id);
    const PluginID& GetID() const noexcept;
    //! Initializes the effect on demand
    const EffectInstanceFactory* GetEffect();
    //! const accessor will not initialize the effect on demand
    const EffectInstanceFactory* GetEffect() const { return mPlugin; }

    //! Expose a pointer to the state's instance (making one as needed).
    /*!
     @post `true` (no promise result is not null)
     */
    std::shared_ptr<EffectInstance> GetInstance();

    //! Get locations that a GUI can connect meters to
    const EffectOutputs* GetOutputs() const { return mMovedOutputs.get(); }

    //! Main thread sets up for playback
    std::shared_ptr<EffectInstance> Initialize(double rate);
    //! Main thread sets up this state before adding it to lists
    std::shared_ptr<EffectInstance>
    AddGroup(
        const ChannelGroup* group, unsigned chans, float sampleRate);
    //! Worker thread begins a batch of samples
    /*! @param running means no pause or deactivation of containing list */
    bool ProcessStart(bool running);
    //! Worker thread processes part of a batch of samples
    /*!
     @return how many leading samples are discardable for latency
     */
    size_t Process(const ChannelGroup* group, unsigned chans, // How many channels the playback device needs
                   const float* const* inbuf, //!< chans input buffers
                   float* const* outbuf, //!< chans output buffers
                   float* dummybuf, //!<  one dummy output buffer
                   size_t numSamples);
    //! Worker thread finishes a batch of samples
    bool ProcessEnd();

    const EffectSettings& GetSettings() const { return mMainSettings.settings; }

    //! Test only in the main thread
    bool IsEnabled() const noexcept;

    //! Test only in the worker thread, or else when there is no processing
    bool IsActive() const noexcept;

    //! Set only in the main thread
    void SetActive(bool active);

    //! Main thread cleans up playback
    bool Finalize() noexcept;

    static const std::string& XMLTag();
    bool HandleXMLTag(
        const std::string_view& tag, const AttributesList& attrs) override;
    void HandleXMLEndTag(const std::string_view& tag) override;
    XMLTagHandler* HandleXMLChild(const std::string_view& tag) override;
    void WriteXML(XMLWriter& xmlFile);

    //! Expose access so a dialog can be connected to this state
    //! To be called by the main thread only
    /*!
     @post result: `result != nullptr`
     */
    std::shared_ptr<EffectSettingsAccess> GetAccess();

private:

    std::shared_ptr<EffectInstance> MakeInstance();
    std::shared_ptr<EffectInstance> EnsureInstance(double rate);

    struct Access;
    struct AccessState;

    AccessState* GetAccessState() const
    {
        return mpAccessState.load(std::memory_order_relaxed);
    }

    AccessState* TestAccessState() const
    {
        return mpAccessState.load(std::memory_order_acquire);
    }

    PluginID mID;

    //! Stateful instance made by the plug-in
    std::weak_ptr<EffectInstance> mwInstance;
    //! Stateless effect object
    const EffectInstanceFactory* mPlugin{};

    struct SettingsAndCounter {
        using Counter = unsigned char;

        EffectSettings settings;
        Counter counter{ 0 };

        void swap(SettingsAndCounter& other)
        {
            settings.swap(other.settings);
            std::swap(counter, other.counter);
        }
    };

    struct Response {
        using Counter = unsigned char;

        Counter counter{ 0 };
        std::unique_ptr<EffectOutputs> pOutputs;
    };

    //! Updated immediately by Access::Set in the main thread
    NonInterfering<SettingsAndCounter> mMainSettings;
    std::unique_ptr<EffectInstance::Message> mMessage;
    std::unique_ptr<EffectOutputs> mMovedOutputs;

    /*! @name Members that are changed also in the worker thread
     @{
     */

    //! Updated with delay, but atomically, in the worker thread; skipped by the
    //! copy constructor so that there isn't a race when pushing an Undo state
    NonInterfering<SettingsAndCounter> mWorkerSettings;
    std::unique_ptr<EffectInstance::Message> mMovedMessage;
    std::unique_ptr<EffectOutputs> mOutputs;

    //! How many samples must be discarded
    std::optional<EffectInstance::SampleCount> mLatency;
    //! Assigned in the worker thread at the start of each processing scope
    bool mLastActive{};

    //! @}

    /*! @name Members that do not change during processing
     @{
     */

    std::unordered_map<const ChannelGroup*, std::pair<size_t, double> >
    mGroups;

    // This must not be reset to nullptr while a worker thread is running.
    // In fact it is never yet reset to nullptr, before destruction.
    // Destroy before mWorkerSettings:
    AtomicUniquePointer<AccessState> mpAccessState{ nullptr };

    wxString mParameters; // Used only during deserialization
    size_t mCurrentProcessor{ 0 };
    bool mInitialized{ false };

    //! @}
};

#endif // __AUDACITY_REALTIMEEFFECTSTATE_H__
