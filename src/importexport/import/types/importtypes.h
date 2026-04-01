/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "io/path.h"

namespace au::importexport {
struct FileInfo
{
    muse::io::path_t path;
    double duration = 0.0;
    int trackCount = 0;

    bool isEmpty() const noexcept
    {
        return path.empty();
    }
};

/// Persisted "remember my choice" action for tempo detection dialogs on import.
enum class LoopAction
{
    Ask,
    MatchProjectToLoop,
    MatchLoopToProject,
    DoNothing,
};
}
