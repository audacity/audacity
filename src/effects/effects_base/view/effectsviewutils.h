/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#define REGISTER_AUDACITY_EFFECTS_SINGLETON_TYPE(Factory) \
    qmlRegisterSingletonType<Factory>("Audacity.Effects", 1, 0, #Factory, \
                                      [] (QQmlEngine*, QJSEngine*) -> QObject* { \
        return new Factory(); \
    });
