/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QObject>
#include <QtQml/qqmlregistration.h>

namespace au::macromanager {
class MacroManagerModel : public QObject
{
    Q_OBJECT
    QML_ELEMENT

public:
    explicit MacroManagerModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();
};
}
