/*
* Audacity: A Digital Audio Editor
*/
#include "commanddisplay.h"

#include <QStringList>

#include "effects/effects_base/effectscommands.h"

using namespace au::macromanager;
using namespace muse::rcommand;

namespace {
std::optional<au::effects::EffectDisplayInfo> appliedEffect(const Command& command, const au::effects::IEffectsProvider& effectsProvider)
{
    const au::effects::EffectId effectId = au::effects::effectIdFromCommand(command);
    if (effectId.empty() || command != au::effects::makeEffectApplyCommand(effectId)) {
        return std::nullopt;
    }

    return effectsProvider.effectDisplayInfo(effectId);
}
}

CommandDisplay au::macromanager::commandDisplay(const CommandInfo& info, const effects::IEffectsProvider& effectsProvider)
{
    const std::optional<effects::EffectDisplayInfo> effect = appliedEffect(info.command, effectsProvider);
    if (!effect) {
        return { info.title.qTranslatedWithoutMnemonic(), QString() };
    }

    QStringList origin { effect->family.toQString(), effect->vendor.toQString() };
    origin.removeAll(QString());
    origin.removeDuplicates();

    return { effect->title.toQString(), origin.join(u" · ") };
}
