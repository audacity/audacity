/*
 * Audacity: A Digital Audio Editor
 */
#include "SavedMasterEffectList.h"
#include "Project.h"

SavedMasterEffectList::SavedMasterEffectList(AudacityProject& project)
    : m_project(project)
{
    UpdateCopy();
}

void SavedMasterEffectList::UpdateCopy()
{
    mLastSavedList = std::make_unique<RealtimeEffectList>(RealtimeEffectList::Get(m_project));
}

static const AudacityProject::AttachedObjects::RegisteredFactory key2{
    [](AudacityProject& project)
    {
        return std::make_shared<SavedMasterEffectList>(project);
    } };

SavedMasterEffectList& SavedMasterEffectList::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<SavedMasterEffectList>(key2);
}

const SavedMasterEffectList& SavedMasterEffectList::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

RealtimeEffectList& SavedMasterEffectList::List()
{
    return *mLastSavedList;
}

const RealtimeEffectList& SavedMasterEffectList::List() const
{
    return *mLastSavedList;
}
