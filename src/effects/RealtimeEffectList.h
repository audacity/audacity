/**********************************************************************

 Audacity: A Digital Audio Editor

 RealtimeEffectList.h

 *********************************************************************/

#ifndef __AUDACITY_REALTIMEEFFECTLIST_H__
#define __AUDACITY_REALTIMEEFFECTLIST_H__

#include <vector>

#include "PluginProvider.h" // for PluginID
#include "UndoManager.h"
#include "XMLTagHandler.h"

class AudacityProject;

class RealtimeEffectState;

class Track;

class RealtimeEffectList final
   // Inheritance from std::enable_shared_from_this must be public
   // but the per-track lists are managed by unique not shared pointers
   : public std::enable_shared_from_this<RealtimeEffectList>
   , public ClientData::Base
   , public ClientData::Cloneable<>
   , public UndoStateExtension
   , public XMLTagHandler
{
   RealtimeEffectList(const RealtimeEffectList &) = delete;
   RealtimeEffectList &operator=(const RealtimeEffectList &) = delete;

public:
   
   using States = std::vector<std::unique_ptr<RealtimeEffectState>>;

   RealtimeEffectList();
   virtual ~RealtimeEffectList();

   std::unique_ptr<ClientData::Cloneable<>> Clone() const override;

   static RealtimeEffectList &Get(AudacityProject &project);
   static const RealtimeEffectList &Get(const AudacityProject &project);
   static RealtimeEffectList &Set(
      AudacityProject &project,
      const std::shared_ptr<RealtimeEffectList> &list);

   static RealtimeEffectList &Get(Track &track);
   static const RealtimeEffectList &Get(const Track &track);

   using StateVisitor =
      std::function<void(RealtimeEffectState &state, bool bypassed)>;

   //! Apply the function to all states sequentially.
   void Visit(StateVisitor func);

   //! Returns null if the id is nonempty but no such effect was found
   RealtimeEffectState *AddState(const PluginID &id);
   void RemoveState(RealtimeEffectState &state);
   size_t GetStatesCount() const noexcept;
   RealtimeEffectState* GetStateAt(size_t index) noexcept;
   /**
    * \brief Changes effect order in the stack
    * \param fromIndex Index of the moved effect
    * \param toIndex Desired position of the moved effect
    */
   void Reorder(size_t fromIndex, size_t toIndex);

   static const std::string &XMLTag();
   bool HandleXMLTag(
      const std::string_view &tag, const AttributesList &attrs) override;
   void HandleXMLEndTag(const std::string_view &tag) override;
   XMLTagHandler *HandleXMLChild(const std::string_view &tag) override;
   void WriteXML(XMLWriter &xmlFile) const;

   void RestoreUndoRedoState(AudacityProject &project) noexcept override;

private:
   States mStates;
};

#endif // __AUDACITY_REALTIMEEFFECTLIST_H__
