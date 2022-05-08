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

std::unique_ptr<ClientData::Cloneable<>> RealtimeEffectList::Clone() const
{
   auto result = std::make_unique<RealtimeEffectList>();
   for (auto &pState : mStates)
      result->mStates.push_back(
         std::make_shared<RealtimeEffectState>(*pState));
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

void RealtimeEffectList::Visit(StateVisitor func)
{
   for (auto &state : mStates)
      func(*state, !state->IsActive());
}

std::shared_ptr<RealtimeEffectState>
RealtimeEffectList::AddState(const PluginID &id)
{
   auto pState = std::make_shared<RealtimeEffectState>(id);
   if (id.empty() || pState->GetEffect() != nullptr) {
      mStates.emplace_back(pState);
      return pState;
   }
   else
      // Effect initialization failed for the id
      return nullptr;
}

void RealtimeEffectList::RemoveState(
   const std::shared_ptr<RealtimeEffectState> &pState)
{
   auto end = mStates.end(),
      found = std::find(mStates.begin(), end, pState);
   if (found != end)
      mStates.erase(found);
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

void RealtimeEffectList::Reorder(size_t fromIndex, size_t toIndex)
{
   assert(fromIndex < mStates.size());
   assert(toIndex < mStates.size());
   if(fromIndex != toIndex)
   {
      size_t iFirst, iMid, iLast;
      if (toIndex < fromIndex)
         iFirst = toIndex, iMid = fromIndex, iLast = fromIndex + 1;
      else
         iFirst = fromIndex, iMid = fromIndex + 1, iLast = toIndex + 1;
      auto begin = mStates.begin();
      std::rotate(begin + iFirst, begin + iMid, begin + iLast);
   }
}

const std::string &RealtimeEffectList::XMLTag()
{
   static const std::string result{"effects"};
   return result;
}

bool RealtimeEffectList::HandleXMLTag(
   const std::string_view &tag, const AttributesList &)
{
   return (tag == XMLTag());
}

void RealtimeEffectList::HandleXMLEndTag(const std::string_view &tag)
{
   if (tag == XMLTag()) {
      // Remove states that fail to load their effects
      auto end = mStates.end();
      auto newEnd = std::remove_if( mStates.begin(), end,
         [](const auto &pState){ return pState->GetEffect() == nullptr; });
      mStates.erase(newEnd, end);
   }
}

XMLTagHandler *RealtimeEffectList::HandleXMLChild(const std::string_view &tag)
{
   if (tag == RealtimeEffectState::XMLTag()) {
      auto pState = AddState({});
      assert(pState); // Should succeed always for empty id
      return pState.get();
   }
   return nullptr;
}

void RealtimeEffectList::WriteXML(XMLWriter &xmlFile) const
{
   if (mStates.size() == 0)
      return;

   xmlFile.StartTag(XMLTag());

   for (const auto & state : mStates)
      state->WriteXML(xmlFile);
   
   xmlFile.EndTag(XMLTag());
}

void RealtimeEffectList::RestoreUndoRedoState(AudacityProject &project) noexcept
{
   // Restore per-project states
   Set(project, shared_from_this());
}

static UndoRedoExtensionRegistry::Entry sEntry {
   [](AudacityProject &project) -> std::shared_ptr<UndoStateExtension> {
      return RealtimeEffectList::Get(project).shared_from_this();
   }
};
