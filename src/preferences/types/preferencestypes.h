/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <qobjectdefs.h>

namespace au::preferences {
class SaveBehaviorPref
{
    Q_GADGET

public:
    enum class SaveBehavior {
        AlwaysAsk = 0,
        AlwaysSaveToCloud,
        AlwaysSaveToComputer
    };
    Q_ENUM(SaveBehavior)
};
}
