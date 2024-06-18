/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROJECTSCENE_TRACKTYPES_H
#define AU_PROJECTSCENE_TRACKTYPES_H

#include "qobjectdefs.h"

#include "processing/processingtypes.h"

namespace au::projectscene {
class TrackTypes
{
    Q_GADGET

public:
    //! NOTE: must be in sync with au::processing::TrackType
    enum class Type {
        UNDEFINED = 0,
        MONO,
        STEREO,
        LABEL
    };
    Q_ENUM(Type)
};

class ClipKey
{
    Q_GADGET

public:

    processing::ClipKey key;
};

class VerticalRulerTypes
{
    Q_GADGET
public:
    enum class Type {
        LINEAR_AMP = 0,
        LOGARITHMIC_DB,
        LINEAR_DB
    };
    Q_ENUM(Type)
};
}

enum class TimelineRulerMode {
    MINUTES_AND_SECONDS = 0,
    BEATS_AND_MEASURES
};

#endif // AU_PROJECTSCENE_TRACKTYPES_H
