/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/uicomponents/qml/Muse/UiComponents/abstracttableviewmodel.h"

#include <QQmlParserStatus>
#include <QObject>

namespace au::effects {
namespace PluginManagerTableViewCellType {
Q_NAMESPACE;
QML_ELEMENT;

enum class Type {
    Enabled = static_cast<int>(muse::uicomponents::TableViewCellType::Type::UserType) + 1,
};

Q_ENUM_NS(Type)
}

class PluginManagerTableViewModel : public muse::uicomponents::AbstractTableViewModel, public QQmlParserStatus
{
    Q_OBJECT

public:
    explicit PluginManagerTableViewModel(QObject* parent = nullptr);

private:
    void classBegin() override {}
    void componentComplete() override;

    QVector<muse::uicomponents::TableViewHeader*> makeHorizontalHeaders();
    QVector<muse::uicomponents::TableViewHeader*> makeVerticalHeaders();
    QVector<QVector<muse::uicomponents::TableViewCell*> > makeTable();
};
}
