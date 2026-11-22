/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "CloudSyncDTO.h"

namespace audacity::cloud::audiocom::sync {
enum class ReconcileDecision
{
    Resend,
    Adopt,
    Diverged,
};

[[nodiscard]] CLOUD_AUDIOCOM_API ReconcileDecision DecideReconcile(
    const std::string& localHeadId, const SnapshotInfo& serverHead, const SampleBlockHashes& requestBlockHashes);
} // namespace audacity::cloud::audiocom::sync
