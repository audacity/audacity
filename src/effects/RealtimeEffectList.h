/**********************************************************************

 Audacity: A Digital Audio Editor

 RealtimeEffectList.h

 *********************************************************************/

#ifndef __AUDACITY_REALTIMEEFFECTLIST_H__
#define __AUDACITY_REALTIMEEFFECTLIST_H__

#include <atomic>
#include <vector>

#include "PluginProvider.h" // for PluginID
#include "spinlock.h"
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
   using Lock = spinlock;
   using States = std::vector<std::shared_ptr<RealtimeEffectState>>;

   RealtimeEffectList();
   virtual ~RealtimeEffectList();

   Lock &GetLock() const { return mLock; }

   //! Should be called (for pushing undo states) only from main thread, to
   //! avoid races
   std::unique_ptr<ClientData::Cloneable<>> Clone() const override;

   static RealtimeEffectList &Get(AudacityProject &project);
   static const RealtimeEffectList &Get(const AudacityProject &project);
   static RealtimeEffectList &Set(
      AudacityProject &project,
      const std::shared_ptr<RealtimeEffectList> &list);

   static RealtimeEffectList &Get(Track &track);
   static const RealtimeEffectList &Get(const Track &track);

   // Type that state visitor functions would have for out-of-line definition
   // of Visit
   // using StateVisitor =
      // std::function<void(RealtimeEffectState &state, bool listIsActive)> ;

   //! Apply the function to all states sequentially.
   template<typename StateVisitor>
   void Visit(const StateVisitor &func)
   {
      for (auto &state : mStates)
         func(*state, IsActive());
   }

   //! Use only in the main thread
   //! Returns true for success.
   //! Sends Insert message on success.
   /*!
    @post result: `!result || pState->GetEffect() != nullptr`
    */
   bool AddState(std::shared_ptr<RealtimeEffectState> pState);

   //! Use only in the main thread
   //! On success sends Remove message.
   void RemoveState(const std::shared_ptr<RealtimeEffectState> &pState);

   //! Use only in the main thread, to avoid races
   //! Returns total number of effects in this list
   size_t GetStatesCount() const noexcept;
   //! Returns effect state at given position
   //! Use only in the main thread, to avoid races
   std::shared_ptr<RealtimeEffectState> GetStateAt(size_t index) noexcept;

   /**
    * \brief Use only in the main thread. Changes effect order in the stack.
    * Does nothing if fromIndex equals toIndex. Otherwise effects between
    * fromIndex (exclusive) and toIndex are shifted towards fromIndex.
    * Sends Move event.
    * \param fromIndex Index of the moved effect
    * \param toIndex Final position of the moved effect
    */
   void MoveEffect(size_t fromIndex, size_t toIndex);

   static const std::string &XMLTag();
   bool HandleXMLTag(
      const std::string_view &tag, const AttributesList &attrs) override;

   //! Use only in the main thread.  May add a state while deserializing
   XMLTagHandler *HandleXMLChild(const std::string_view &tag) override;

   //! Use only in the main thread, to avoid races
   void WriteXML(XMLWriter &xmlFile) const;

   void RestoreUndoRedoState(AudacityProject &project) noexcept override;

   //! Non-blocking atomic boolean load
   bool IsActive() const;

   //! Done by main thread only, under a lock guard
   void SetActive(bool value);

private:
   States mStates;

   using LockGuard = std::lock_guard<Lock>;
   mutable Lock mLock;

   std::atomic<bool> mActive{ true };
};

#endif // __AUDACITY_REALTIMEEFFECTLIST_H__
