/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ClientData.h"
#include "RealtimeEffectList.h"
#include <memory>

class AudacityProject;
class RealtimeEffectList;

class SavedMasterEffectList final : public ClientData::Base
{
public:
    SavedMasterEffectList(AudacityProject& project);

    static SavedMasterEffectList& Get(AudacityProject& project);
    static const SavedMasterEffectList& Get(const AudacityProject& project);

    void UpdateCopy();
    RealtimeEffectList& List();
    const RealtimeEffectList& List() const;

private:

    AudacityProject& m_project;
    std::unique_ptr<RealtimeEffectList> mLastSavedList;
};
