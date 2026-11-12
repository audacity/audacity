/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QString>

#include "framework/rcommand/commandtypes.h"
#include "effects/effects_base/ieffectsprovider.h"

namespace au::macromanager {
struct CommandDisplay {
    QString title;
    QString subtitle;
};

CommandDisplay commandDisplay(const muse::rcommand::CommandInfo& info, const effects::IEffectsProvider& effectsProvider);
}
