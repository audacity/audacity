/**********************************************************************
 
  Audacity: A Digital Audio Editor
 
  RealtimeEffectList.cpp
 
 *********************************************************************/

#include "RealtimeEffectList.h"
#include "RealtimeEffectState.h"

#include "Project.h"
#include "Track.h"

RealtimeEffectList::RealtimeEffectList()
{
}

RealtimeEffectList::~RealtimeEffectList()
{
}

// Deep copy of states
std::unique_ptr<ClientData::Cloneable<>> RealtimeEffectList::Clone() const
{
   auto result = std::make_unique<RealtimeEffectList>();
   for (auto &pState : mStates)
      result->mStates.push_back(RealtimeEffectState::make_shared(*pState));
   result->SetActive(this->IsActive());
   return result;
}

// Access for per-project effect list
static const AttachedProjectObjects::RegisteredFactory masterEffects
{
   [](AudacityProject &project)
   {
      return std::make_shared<RealtimeEffectList>();
   }
};

RealtimeEffectList &RealtimeEffectList::Get(AudacityProject &project)
{
   return project.AttachedObjects::Get<RealtimeEffectList>(masterEffects);
}

RealtimeEffectList &RealtimeEffectList::Set(
   AudacityProject &project, const std::shared_ptr<RealtimeEffectList> &list)
{
   auto &result = *list;
   project.AttachedObjects::Assign(masterEffects, list);
   return result;
}

const RealtimeEffectList &RealtimeEffectList::Get(const AudacityProject &project)
{
   return Get(const_cast<AudacityProject &>(project));
}

static const Track::ChannelGroupAttachments::RegisteredFactory trackEffects
{
   [](Track::ChannelGroupData &)
   {
      return std::make_unique<RealtimeEffectList>();
   }
};

// Access for per-track effect list
RealtimeEffectList &RealtimeEffectList::Get(Track &track)
{
   return track.GetGroupData()
      .Track::ChannelGroupAttachments::Get<RealtimeEffectList>(trackEffects);
}

const RealtimeEffectList &RealtimeEffectList::Get(const Track &track)
{
   return Get(const_cast<Track &>(track));
}

bool
RealtimeEffectList::AddState(std::shared_ptr<RealtimeEffectState> pState)
{
   const auto &id = pState->GetID();
   if (pState->GetEffect() != nullptr) {
      auto shallowCopy = mStates;
      shallowCopy.emplace_back(move(pState));
      // Lock for only a short time
      (LockGuard{ mLock }, swap(shallowCopy, mStates));

      Publisher<RealtimeEffectListMessage>::Publish({
         RealtimeEffectListMessage::Type::Insert,
         mStates.size() - 1,
         { }
      });

      return true;
   }
   else
      // Effect initialization failed for the id
      return false;
}

void RealtimeEffectList::RemoveState(
   const std::shared_ptr<RealtimeEffectState> &pState)
{
   auto shallowCopy = mStates;
   auto end = shallowCopy.end(),
      found = std::find(shallowCopy.begin(), end, pState);
   if (found != end)
   {
      const auto index = std::distance(shallowCopy.begin(), found);
      shallowCopy.erase(found);

      // Lock for only a short time
      (LockGuard{ mLock }, swap(shallowCopy, mStates));

      Publisher<RealtimeEffectListMessage>::Publish({
         RealtimeEffectListMessage::Type::Remove,
         static_cast<size_t>(index),
         { }
      });
   }
}

size_t RealtimeEffectList::GetStatesCount() const noexcept
{
   return mStates.size();
}

std::shared_ptr<RealtimeEffectState>
RealtimeEffectList::GetStateAt(size_t index) noexcept
{
   if (index < mStates.size())
      return mStates[index];
   return nullptr;
}

void RealtimeEffectList::MoveEffect(size_t fromIndex, size_t toIndex)
{
   assert(fromIndex < mStates.size());
   assert(toIndex < mStates.size());

   auto shallowCopy = mStates;
   if(fromIndex == toIndex)
      return;
   if(fromIndex < toIndex)
   {
      const auto first = shallowCopy.begin() + fromIndex;
      const auto last = shallowCopy.begin() + toIndex + 1;
      std::rotate(first, first + 1, last);
   }
   else
   {
      const auto first = shallowCopy.rbegin() + (shallowCopy.size() - (fromIndex + 1));
      const auto last = shallowCopy.rbegin() + (shallowCopy.size() - toIndex);
      std::rotate(first, first + 1, last);
   }
   // Lock for only a short time
   (LockGuard{ mLock }, swap(shallowCopy, mStates));

   Publisher<RealtimeEffectListMessage>::Publish({
      RealtimeEffectListMessage::Type::Move,
      fromIndex,
      toIndex
   });
}

const std::string &RealtimeEffectList::XMLTag()
{
   static const std::string result{"effects"};
   return result;
}

static constexpr auto activeAttribute = "active";

bool RealtimeEffectList::HandleXMLTag(
   const std::string_view &tag, const AttributesList &attrs)
{
   if (tag == XMLTag()) {
      for (auto &[attr, value] : attrs) {
         if (attr == activeAttribute)
            SetActive(value.Get<bool>());
      }
      return true;
   }
   return false;
}

XMLTagHandler *RealtimeEffectList::HandleXMLChild(const std::string_view &tag)
{
   if (tag == RealtimeEffectState::XMLTag()) {
      mStates.push_back(RealtimeEffectState::make_shared(PluginID{}));
      return mStates.back().get();
   }
   return nullptr;
}

void RealtimeEffectList::WriteXML(XMLWriter &xmlFile) const
{
   xmlFile.StartTag(XMLTag());
   xmlFile.WriteAttr(activeAttribute, IsActive());
   for (const auto & state : mStates)
      state->WriteXML(xmlFile);
   xmlFile.EndTag(XMLTag());
}

void RealtimeEffectList::RestoreUndoRedoState(AudacityProject &project) noexcept
{
   // Restore per-project states
   Set(project, shared_from_this());
}

bool RealtimeEffectList::IsActive() const
{
   return mActive.load(std::memory_order_relaxed);
}

void RealtimeEffectList::SetActive(bool value)
{
   (LockGuard{ mLock }, mActive.store(value, std::memory_order_relaxed));
}

static UndoRedoExtensionRegistry::Entry sEntry {
   [](AudacityProject &project) -> std::shared_ptr<UndoStateExtension> {
      return RealtimeEffectList::Get(project).shared_from_this();
   }
};
