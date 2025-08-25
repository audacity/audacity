/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <qobjectdefs.h>
#include <string>
#include <vector>

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

class ExportOptionType
{
    Q_GADGET
public:
    enum Type {
        TypeEnum,
        TypeBool,
        TypeRange,
        TypeString
    };
    Q_ENUM(Type)
};

using OptionValue = std::variant<
    bool,
    int,
    double,
    std::string>;

struct ExportOption
{
    enum Flags : int
    {
        TypeMask         = 0xff,
        TypeRange        = 1,
        TypeEnum         = 2,

        ReadOnly         = 0x100,
        Hidden           = 0x200,

        Default          = 0
    };

    int id = -1;
    std::string title;
    int flags { Default };
    std::vector<OptionValue> values;
    std::vector<std::string> names;
};
}
