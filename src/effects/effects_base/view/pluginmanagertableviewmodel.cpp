/*
 * Audacity: A Digital Audio Editor
 */

 #include "pluginmanagertableviewmodel.h"

#include "framework/global/translation.h"
#include "framework/uicomponents/qml/Muse/UiComponents/internal/tableviewcell.h"

namespace au::effects {
PluginManagerTableViewModel::PluginManagerTableViewModel(QObject* parent)
    : AbstractTableViewModel(parent) {}

void PluginManagerTableViewModel::componentComplete()
{
    setHorizontalHeaders(makeHorizontalHeaders());
    setVerticalHeaders(makeVerticalHeaders());
    setTable(makeTable());
}

QVector<muse::uicomponents::TableViewHeader*> PluginManagerTableViewModel::makeHorizontalHeaders()
{
    using namespace muse::uicomponents;

    QVector<TableViewHeader*> hHeaders;

    hHeaders << makeHorizontalHeader(muse::qtrc("effects",
                                                "Name"), TableViewCellType::Type::String, TableViewCellEditMode::Mode::DoubleClick, 152);
    hHeaders << makeHorizontalHeader(muse::qtrc("effects",
                                                "Type"), TableViewCellType::Type::String, TableViewCellEditMode::Mode::DoubleClick, 296);
    hHeaders << makeHorizontalHeader(muse::qtrc("effects", "Enabled"),
                                     static_cast<TableViewCellType::Type>(PluginManagerTableViewCellType::Type::Enabled),
                                     TableViewCellEditMode::Mode::StartInEdit);

    return hHeaders;
}

QVector<muse::uicomponents::TableViewHeader*> PluginManagerTableViewModel::makeVerticalHeaders()
{
    using namespace muse::uicomponents;

    QVector<TableViewHeader*> vHeaders;

    for (auto i = 0; i < 4; ++i) {
        auto* vHeader = new TableViewHeader(this);
        vHeader->setTitle(QString("Plugin %1").arg(i));
        vHeaders << vHeader;
    }

    return vHeaders;
}

QVector<QVector<muse::uicomponents::TableViewCell*> > PluginManagerTableViewModel::makeTable()
{
    QVector<QVector<muse::uicomponents::TableViewCell*> > table;
    for (auto i = 0; i < 4; ++i) {
        QVector<muse::uicomponents::TableViewCell*> row;
        for (auto j = 0; j < 2; ++j) {
            row.append(makeCell(muse::Val(QString("Cell %1,%2").arg(i).arg(j))));
        }
        row.append(makeCell(muse::Val(false)));
        table.append(row);
    }
    return table;
}
}
