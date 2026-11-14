/*
 * Audacity: A Digital Audio Editor
 */
#include "tabletestsmodel.h"

using namespace au::appshell;
using namespace muse;
using namespace muse::uicomponents;

TableTestsModel::TableTestsModel(QObject* parent)
    : AbstractTableViewModel(parent)
{
}

void TableTestsModel::load()
{
    QVector<TableViewHeader*> hHeaders = {
        makeHorizontalHeader("Name", TableViewCellType::Type::String, TableViewCellEditMode::Mode::DoubleClick, 160),
        makeHorizontalHeader("Size", TableViewCellType::Type::Int, TableViewCellEditMode::Mode::DoubleClick, 120),
        makeHorizontalHeader("Active", TableViewCellType::Type::Bool, TableViewCellEditMode::Mode::DoubleClick, 120),
    };
    setHorizontalHeaders(hHeaders);

    struct Row {
        QString name;
        int size;
        bool active;
    };
    const std::vector<Row> rows = {
        { "Banana", 3, true },
        { "apple", 10, false },
        { "Cherry", 1, true },
        { "date", 7, false },
        { "Elderberry", 5, true },
        { "fig", 2, false },
    };

    QVector<TableViewHeader*> vHeaders;
    QVector<QVector<TableViewCell*> > table;
    for (int i = 0; i < static_cast<int>(rows.size()); ++i) {
        const Row& r = rows[i];
        QVector<TableViewCell*> row = {
            makeCell(Val(r.name)),
            makeCell(Val(r.size)),
            makeCell(Val(r.active)),
        };
        table.append(row);

        auto* header = new TableViewHeader(this);
        header->setTitle(QString::number(i + 1));
        vHeaders << header;
    }
    setVerticalHeaders(vHeaders);
    setTable(table);
}
