/**********************************************************************

 Audacity: A Digital Audio Editor

 @file RealtimeEffectState.h

 Paul Licameli split from RealtimeEffectManager.cpp

 *********************************************************************/

#ifndef __AUDACITY_REALTIMEEFFECTSTATE_H__
#define __AUDACITY_REALTIMEEFFECTSTATE_H__

#include <atomic>
#include <unordered_map>
#include <vector>
#include <cstddef>
#include "EffectInterface.h"
#include "GlobalVariable.h"
#include "MemoryX.h"
#include "PluginProvider.h" // for PluginID
#include "XMLTagHandler.h"

class EffectSettingsAccess;
class Track;

class RealtimeEffectState
   : public XMLTagHandler
   , public std::enable_shared_from_this<RealtimeEffectState>
   , public SharedNonInterfering<RealtimeEffectState>
{
public:
   struct AUDACITY_DLL_API EffectFactory : GlobalHook<EffectFactory,
      const EffectInstanceFactory *(const PluginID &)
   >{};

   explicit RealtimeEffectState(const PluginID & id);
   RealtimeEffectState(const RealtimeEffectState &other);
   RealtimeEffectState &operator =(const RealtimeEffectState &other) = delete;
   ~RealtimeEffectState();

   //! May be called with nonempty id at most once in the lifetime of a state
   /*!
    Call with empty id is ignored.
    Called by the constructor that takes an id */
   void SetID(const PluginID & id);
   const PluginID& GetID() const noexcept;
   const EffectInstanceFactory *GetEffect();

   //! Expose a pointer to the state's instance (making one as needed).
   /*!
    @post `true` (no promise result is not null)
    */
   std::shared_ptr<EffectInstance> GetInstance();
   
   //! Main thread sets up for playback
   std::shared_ptr<EffectInstance> Initialize(double rate);
   //! Main thread sets up this state before adding it to lists
   std::shared_ptr<EffectInstance>
   AddTrack(Track &track, unsigned chans, float sampleRate);
   //! Worker thread begins a batch of samples
   /*! @param running means no pause or deactivation of containing list */
   bool ProcessStart(bool running);
   //! Worker thread processes part of a batch of samples
   size_t Process(Track &track,
      unsigned chans,
      const float *const *inbuf, //!< chans input buffers
      float *const *outbuf, //!< chans output buffers
      float *dummybuf, //!< one scratch buffer
      size_t numSamples);
   //! Worker thread finishes a batch of samples
   bool ProcessEnd();

   //! Test only in the worker thread, or else when there is no processing
   bool IsActive() const noexcept;

   //! Main thread cleans up playback
   bool Finalize() noexcept;

   static const std::string &XMLTag();
   bool HandleXMLTag(
      const std::string_view &tag, const AttributesList &attrs) override;
   void HandleXMLEndTag(const std::string_view &tag) override;
   XMLTagHandler *HandleXMLChild(const std::string_view &tag) override;
   void WriteXML(XMLWriter &xmlFile);

   //! Expose access so a dialog can be connected to this state
   //! To be called by the main thread only
   /*!
    @post result: `result != nullptr`
    */
   std::shared_ptr<EffectSettingsAccess> GetAccess();

private:
   std::shared_ptr<EffectInstance> EnsureInstance(double rate);

   struct Access;
   struct AccessState;

   AccessState *GetAccessState() const
   {
      return mpAccessState.load(std::memory_order_relaxed);
   }
   AccessState *TestAccessState() const
   {
      return mpAccessState.load(std::memory_order_acquire);
   }

   /*! @name Members that are copied
    @{
    */

   PluginID mID;

   //! Stateless effect object
   const EffectInstanceFactory *mPlugin{};
   
   //! Updated immediately by Access::Set
   NonInterfering<EffectSettings> mMainSettings;

   //! @}

   // Following are not copied

   //! Updated with delay, but atomically, in the worker thread; skipped by the
   //! copy constructor so that there isn't a race when pushing an Undo state
   NonInterfering<EffectSettings> mWorkerSettings;

   wxString mParameters;  // Used only during deserialization

   //! Stateful instance made by the plug-in
   std::weak_ptr<EffectInstance> mwInstance;
   bool mInitialized{ false };

   // This must not be reset to nullptr while a worker thread is running.
   // In fact it is never yet reset to nullptr, before destruction.
   // Destroy before mWorkerSettings:
   AtomicUniquePointer<AccessState> mpAccessState{ nullptr };
   
   size_t mCurrentProcessor{ 0 };
   std::unordered_map<Track *, size_t> mGroups;

   bool mLastActive{};
};

#endif // __AUDACITY_REALTIMEEFFECTSTATE_H__
