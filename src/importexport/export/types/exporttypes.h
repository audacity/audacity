/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <qobjectdefs.h>

namespace au::importexport {
enum class ExportProcessType {
    FULL_PROJECT_AUDIO = 0,
    SELECTED_AUDIO,
    AUDIO_IN_LOOP_REGION,
    TRACKS_AS_SEPARATE_AUDIO_FILES,
    EACH_LABEL_AS_SEPARATE_AUDIO_FILE,
    ALL_LABELS_AS_SUBTITLE_FILE
};

class ExportChannelsPref
{
    Q_GADGET
public:
    enum class ExportChannels {
        MONO = 1,
        STEREO,
        CUSTOM
    };
    Q_ENUM(ExportChannels)
};
}
