/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

#include "uicomponents/qml/Muse/UiComponents/abstracttableviewmodel.h"

namespace au::appshell {
class TableTestsModel : public muse::uicomponents::AbstractTableViewModel
{
    Q_OBJECT
    QML_ELEMENT

public:
    explicit TableTestsModel(QObject* parent = nullptr);

    Q_INVOKABLE void load();
};
}
