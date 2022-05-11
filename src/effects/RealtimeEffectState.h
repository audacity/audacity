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
   ~RealtimeEffectState();

   //! May be called with nonempty id at most once in the lifetime of a state
   /*!
    Call with empty id is ignored.
    Called by the constructor that takes an id */
   void SetID(const PluginID & id);
   const PluginID& GetID() const noexcept;
   const EffectInstanceFactory *GetEffect();

   bool Suspend();
   bool Resume() noexcept;

   //! Main thread sets up for playback
   bool Initialize(double rate);
   bool AddTrack(Track &track, unsigned chans, float rate);
   //! Worker thread begins a batch of samples
   bool ProcessStart();
   //! Worker thread processes part of a batch of samples
   size_t Process(Track &track,
      unsigned chans,
      const float *const *inbuf, //!< chans input buffers
      float *const *outbuf, //!< chans output buffers
      float *dummybuf, //!< one scratch buffer
      size_t numSamples);
   //! Worker thread finishes a batch of samples
   bool ProcessEnd();
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
   
   NonInterfering<EffectSettings> mSettings;

   //! @}

   // Following are not copied
   wxString mParameters;  // Used only during deserialization

   //! Stateful instance made by the plug-in
   std::shared_ptr<EffectInstance> mInstance;

   // This must not be reset to nullptr while a worker thread is running.
   // In fact it is never yet reset to nullptr, before destruction.
   // Destroy before mSettings:
   AtomicUniquePointer<AccessState> mpAccessState{ nullptr };
   
   size_t mCurrentProcessor{ 0 };
   std::unordered_map<Track *, size_t> mGroups;

   std::atomic<int> mSuspendCount{ 1 };    // Effects are initially suspended
};

#endif // __AUDACITY_REALTIMEEFFECTSTATE_H__

