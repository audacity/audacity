/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QQuickItem>

namespace au::projectscene {
class MouseHelper : public QObject
{
    Q_OBJECT
public:
    Q_INVOKABLE void callUngrabMouseOnItem(QQuickItem* item);
};
}
