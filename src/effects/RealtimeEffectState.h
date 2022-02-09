/**********************************************************************

 Audacity: A Digital Audio Editor

 @file RealtimeEffectState.h

 Paul Licameli split from RealtimeEffectManager.cpp

 *********************************************************************/

#ifndef __AUDACITY_REALTIMEEFFECTSTATE_H__
#define __AUDACITY_REALTIMEEFFECTSTATE_H__

#include <atomic>
#include <memory>
#include <unordered_map>
#include <vector>
#include <cstddef>
#include "EffectInterface.h"
#include "GlobalVariable.h"
#include "ModuleInterface.h" // for PluginID
#include "XMLTagHandler.h"

class EffectProcessor;
class Track;

class RealtimeEffectState : public XMLTagHandler
{
public:
   struct AUDACITY_DLL_API EffectFactory : GlobalHook<EffectFactory,
      std::unique_ptr<EffectProcessor>(const PluginID &)
   >{};

   explicit RealtimeEffectState(const PluginID & id);
   ~RealtimeEffectState();

   //! May be called with nonempty id at most once in the lifetime of a state
   /*!
    Call with empty id is ignored.
    Called by the constructor that takes an id */
   void SetID(const PluginID & id);
   EffectProcessor *GetEffect();

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

private:
   PluginID mID;
   wxString mParameters;  // Used only during deserialization
   std::unique_ptr<EffectProcessor> mEffect;
   EffectSettings mSettings;

   size_t mCurrentProcessor{ 0 };
   std::unordered_map<Track *, size_t> mGroups;

   std::atomic<int> mSuspendCount{ 1 };    // Effects are initially suspended
};

#endif // __AUDACITY_REALTIMEEFFECTSTATE_H__

