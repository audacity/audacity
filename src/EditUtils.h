// TODO header
#pragma once

#include "MemoryX.h"

class AudacityProject;

namespace EditUtils
{
//! Doesn't do anything if playback is already stopped.
void StopAndResumePlayback(AudacityProject& project);

// Assumes that `project` outlives the returned lambda.
[[nodiscard]] Finally<std::function<void()>>
StopPlaybackWhileEditing(AudacityProject& project);
} // namespace EditUtils
