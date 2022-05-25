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
#include "Observer.h"

class AudacityProject;

class RealtimeEffectState;

class Track;

struct RealtimeEffectListMessage
{
   enum class Type
   {
      Insert,///<New effect item was added to the list at srcIndex position
      Remove,///<Effect item was removed from the list at srcIndex position
      Move ///<Item position has changed, from srcIndex to dstIndex
   };
   Type type;
   size_t srcIndex;
   size_t dstIndex;
};

class RealtimeEffectList final
   // Inheritance from std::enable_shared_from_this must be public
   // but the per-track lists are managed by unique not shared pointers
   : public std::enable_shared_from_this<RealtimeEffectList>
   , public ClientData::Base
   , public ClientData::Cloneable<>
   , public UndoStateExtension
   , public XMLTagHandler
   , public Observer::Publisher<RealtimeEffectListMessage>
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

   //! Returns null if no such effect was found.
   //! Sends Insert message on success.
   RealtimeEffectState *AddState(const PluginID &id);
   //! On success sends Remove message.
   void RemoveState(RealtimeEffectState &state);

   //! Returns total number of effects in this list
   size_t GetStatesCount() const noexcept;
   //! Returns effect state at given position, does not perform bounds check
   RealtimeEffectState& GetStateAt(size_t index) noexcept;

   /**
    * \brief Changes effect position in the stack. Does nothing if fromIndex equal
    * toIndex. Otherwise effects between fromIndex(excluding) and toIndex are shifted
    * towards fromIndex. Sends Move event.
    * \param fromIndex Index of the moved effect
    * \param toIndex Final position of the moved effect
    */
   void MoveEffect(size_t fromIndex, size_t toIndex);

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
