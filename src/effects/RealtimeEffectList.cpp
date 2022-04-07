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

static const AttachedTrackObjects::RegisteredFactory trackEffects
{
   [](Track &track)
   {
      return std::make_shared<RealtimeEffectList>();
   }
};

// Access for per-track effect list
RealtimeEffectList &RealtimeEffectList::Get(Track &track)
{
   return track.AttachedObjects::Get<RealtimeEffectList>(trackEffects);
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

RealtimeEffectState *RealtimeEffectList::AddState(const PluginID &id)
{
   auto pState = std::make_unique<RealtimeEffectState>(id);
   if (id.empty() || pState->GetEffect() != nullptr) {
      auto result = pState.get();
      mStates.emplace_back(move(pState));
      return result;
   }
   else
      // Effect initialization failed for the id
      return nullptr;
}

void RealtimeEffectList::RemoveState(RealtimeEffectState &state)
{
   auto end = mStates.end(),
      found = std::find_if(mStates.begin(), end,
         [&](const auto &item) { return item.get() == &state; } );
   if (found != end)
      mStates.erase(found);
}

void RealtimeEffectList::Swap(size_t index1, size_t index2)
{
   std::swap(mStates[index1], mStates[index2]);
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
      return pState;
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
