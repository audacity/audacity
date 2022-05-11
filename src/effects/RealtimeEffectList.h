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

   using StateVisitor = std::function<void(RealtimeEffectState &state)>;

   //! Apply the function to all states sequentially.
   void Visit(StateVisitor func);

   //! Use only in the main thread
   //! Returns null if the id is nonempty but no such effect was found
   bool AddState(const std::shared_ptr<RealtimeEffectState> &pState);

   //! Use only in the main thread
   void RemoveState(const std::shared_ptr<RealtimeEffectState> &pState);

   //! Use only in the main thread, to avoid races
   size_t GetStatesCount() const noexcept;

   //! Use only in the main thread, to avoid races
   std::shared_ptr<RealtimeEffectState> GetStateAt(size_t index) noexcept;
   /**
    * \brief Use only in the main thread. Changes effect order in the stack
    * \param fromIndex Index of the moved effect
    * \param toIndex Desired position of the moved effect
    */
   void Reorder(size_t fromIndex, size_t toIndex);

   static const std::string &XMLTag();
   bool HandleXMLTag(
      const std::string_view &tag, const AttributesList &attrs) override;

   //! Use only in the main thread.  May remove a failed state
   void HandleXMLEndTag(const std::string_view &tag) override;

   //! Use only in the main thread.  May add a state while deserializing
   XMLTagHandler *HandleXMLChild(const std::string_view &tag) override;

   //! Use only in the main thread, to avoid races
   void WriteXML(XMLWriter &xmlFile) const;

   void RestoreUndoRedoState(AudacityProject &project) noexcept override;

   //! Non-blocking atomic boolean load
   bool IsActive() const;

   //! Done under a lock guard
   void SetActive(bool value);

private:
   States mStates;

   using LockGuard = std::lock_guard<Lock>;
   mutable Lock mLock;

   std::atomic<bool> mActive{ true };
};

#endif // __AUDACITY_REALTIMEEFFECTLIST_H__
