/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

namespace au::effects {
class PluginManagerTableViewModel : public QObject
{
    Q_OBJECT

public:
    explicit PluginManagerTableViewModel(QObject* parent = nullptr)
        : QObject(parent) {}
};
}
