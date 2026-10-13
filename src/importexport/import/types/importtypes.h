/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <vector>

#include <QObject>

#include "io/path.h"

namespace au::importexport {
class TempoDetectionPref
{
    Q_GADGET
public:
    enum class TempoDetection {
        ALWAYS = 0,
        WORKSPACE_DEPENDENT,
        NEVER
    };
    Q_ENUM(TempoDetection)
};
//! Audio decoded from a file: one buffer of float samples per channel
struct DecodedAudio
{
    double sampleRate = 0.0;
    std::vector<std::vector<float> > channels;
};

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
