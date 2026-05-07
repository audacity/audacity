/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

namespace au::effects {
class ViewerComponentTypes
{
    Q_GADGET
public:
    enum class Type {
        AudioUnit,
        Lv2,
        Vst,
        Builtin,
        Generated,
        Unknown
    };
    Q_ENUM(Type)
};

using ViewerComponentType = ViewerComponentTypes::Type;

struct DropdownOption {
    QString id;
    QString title;
};
}
