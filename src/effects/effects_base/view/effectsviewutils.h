/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effectsviewtypes.h"

#include "framework/uicomponents/qml/Muse/UiComponents/menuitem.h"

#define REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(Factory) \
    qmlRegisterSingletonType<Factory>("Audacity.Effects", 1, 0, #Factory, \
                                      [] (QQmlEngine*, QJSEngine*) -> QObject* { \
        return new Factory(); \
    });

namespace au::effects::utils {
muse::uicomponents::MenuItemList toMenuItemList(const std::vector<DropdownOption>&, int selectedIndex, QObject* parent = nullptr);
}
