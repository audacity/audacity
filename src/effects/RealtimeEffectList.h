/**********************************************************************

 Audacity: A Digital Audio Editor

 RealtimeEffectList.h

 *********************************************************************/

#ifndef __AUDACITY_REALTIMEEFFECTLIST_H__
#define __AUDACITY_REALTIMEEFFECTLIST_H__

#include <vector>

#include "TrackAttachment.h"
#include "ModuleInterface.h" // for PluginID
#include "XMLTagHandler.h"

class AudacityProject;

class RealtimeEffectState;

class Track;

class RealtimeEffectList final : public TrackAttachment, public XMLTagHandler
{
   RealtimeEffectList(const RealtimeEffectList &) = delete;
   RealtimeEffectList &operator=(const RealtimeEffectList &) = delete;

public:
   RealtimeEffectList();
   virtual ~RealtimeEffectList();

   static RealtimeEffectList &Get(AudacityProject &project);
   static const RealtimeEffectList &Get(const AudacityProject &project);

   static RealtimeEffectList &Get(Track &track);
   static const RealtimeEffectList &Get(const Track &track);

   using StateVisitor =
      std::function<void(RealtimeEffectState &state, bool bypassed)>;

   //! Apply the function to all states sequentially.
   void Visit(StateVisitor func);

   //! Returns null if the id is nonempty but no such effect was found
   RealtimeEffectState *AddState(const PluginID &id);
   void RemoveState(RealtimeEffectState &state);
   void Swap(size_t index1, size_t index2);

   using States = std::vector<std::unique_ptr<RealtimeEffectState>>;

   static const std::string &XMLTag();
   bool HandleXMLTag(
      const std::string_view &tag, const AttributesList &attrs) override;
   void HandleXMLEndTag(const std::string_view &tag) override;
   XMLTagHandler *HandleXMLChild(const std::string_view &tag) override;
   void WriteXML(XMLWriter &xmlFile) const;

private:
   States mStates;
};

#endif // __AUDACITY_REALTIMEEFFECTLIST_H__
