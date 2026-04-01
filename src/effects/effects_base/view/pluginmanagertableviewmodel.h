/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ieffectsprovider.h"

#include "framework/global/modularity/ioc.h"
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
    Q_INTERFACES(QQmlParserStatus)

    muse::GlobalInject<IEffectsProvider> effectsProvider;

public:
    explicit PluginManagerTableViewModel(QObject* parent = nullptr);
    ~PluginManagerTableViewModel() override;

    Q_INVOKABLE void handleEdit(int row, int column);
    Q_INVOKABLE void setSearchText(const QString& searchText);

private:
    void classBegin() override {}
    void componentComplete() override;

    QVector<muse::uicomponents::TableViewHeader*> makeHorizontalHeaders();
    QVector<muse::uicomponents::TableViewHeader*> makeVerticalHeaders(const EffectMetaList& effects);
    QVector<QVector<muse::uicomponents::TableViewCell*> > makeTable(const EffectMetaList& effects);
    void setTableRows(const EffectMetaList& effects);
};
}
