/*
* Audacity: A Digital Audio Editor
*/
#include "commanddisplay.h"

#include <algorithm>
#include <utility>
#include <vector>

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

QString valueText(const muse::Val& value)
{
    if (value.type() != muse::Val::Type::List) {
        return QString::fromStdString(value.toString());
    }

    QStringList items;
    for (const muse::Val& item : value.toList()) {
        items << QString::fromStdString(item.toString());
    }
    return items.join(u", ");
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

QString au::macromanager::paramsSummary(const InputSchema& schema, const Params& params)
{
    std::vector<std::pair<std::string, Arg> > args(schema.args.begin(), schema.args.end());
    std::ranges::stable_sort(args, {}, [](const std::pair<std::string, Arg>& entry) {
        return entry.second.uiHints.order;
    });

    QStringList parts;
    for (const auto& [key, arg] : args) {
        const auto value = params.find(key);
        if (value == params.end()) {
            continue;
        }

        const QString label = arg.label.isEmpty() ? QString::fromStdString(key) : arg.label.qTranslated();
        parts << label + ": " + valueText(value->second);
    }

    return parts.join(u" · ");
}
