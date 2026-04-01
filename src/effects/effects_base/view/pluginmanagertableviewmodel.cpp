/*
 * Audacity: A Digital Audio Editor
 */

 #include "pluginmanagertableviewmodel.h"

 #include "internal/effectsutils.h"

#include "framework/global/translation.h"
#include "framework/uicomponents/qml/Muse/UiComponents/internal/tableviewcell.h"

namespace au::effects {
PluginManagerTableViewModel::PluginManagerTableViewModel(QObject* parent)
    : AbstractTableViewModel(parent) {}

PluginManagerTableViewModel::~PluginManagerTableViewModel()
{
    effectsProvider()->save();
}

void PluginManagerTableViewModel::componentComplete()
{
    setHorizontalHeaders(makeHorizontalHeaders());
    const EffectMetaList effects = effectsProvider()->effectMetaList();
    setTableRows(effects);
}

void PluginManagerTableViewModel::setTableRows(const EffectMetaList& effects)
{
    setVerticalHeaders(makeVerticalHeaders(effects));
    setTable(makeTable(effects));
}

void PluginManagerTableViewModel::setSearchText(const QString& searchText)
{
    auto effects = effectsProvider()->effectMetaList();
    const QString searchTextLower = searchText.toLower();
    effects.erase(std::remove_if(effects.begin(), effects.end(), [&searchTextLower](const EffectMeta& meta) {
        return !meta.title.toQString().toLower().contains(searchTextLower);
    }), effects.end());

    setTableRows(effects);
}

QVector<muse::uicomponents::TableViewHeader*> PluginManagerTableViewModel::makeHorizontalHeaders()
{
    using namespace muse::uicomponents;

    QVector<TableViewHeader*> hHeaders;

    hHeaders << makeHorizontalHeader(muse::qtrc("effects", "Enabled"),
                                     static_cast<TableViewCellType::Type>(PluginManagerTableViewCellType::Type::Enabled),
                                     TableViewCellEditMode::Mode::StartInEdit, 100);
    hHeaders << makeHorizontalHeader(muse::qtrc("effects", "Name"),
                                     TableViewCellType::Type::String, TableViewCellEditMode::Mode::DoubleClick, 152);
    hHeaders << makeHorizontalHeader(muse::qtrc("effects", "Path"),
                                     TableViewCellType::Type::String, TableViewCellEditMode::Mode::DoubleClick, 296);
    hHeaders << makeHorizontalHeader(muse::qtrc("effects", "Type"),
                                     TableViewCellType::Type::String, TableViewCellEditMode::Mode::DoubleClick, 152);

    return hHeaders;
}

QVector<muse::uicomponents::TableViewHeader*> PluginManagerTableViewModel::makeVerticalHeaders(const EffectMetaList& effects)
{
    using namespace muse::uicomponents;

    QVector<TableViewHeader*> vHeaders;

    for (const auto& _ : effects) {
        vHeaders << new TableViewHeader(this);
    }

    return vHeaders;
}

QVector<QVector<muse::uicomponents::TableViewCell*> > PluginManagerTableViewModel::makeTable(const EffectMetaList& effects)
{
    QVector<QVector<muse::uicomponents::TableViewCell*> > table;

    for (const auto& meta : effects) {
        QVector<muse::uicomponents::TableViewCell*> row;
        row.append(makeCell(muse::Val(meta.isActivated)));
        row.append(makeCell(muse::Val(meta.title.toQString())));
        row.append(makeCell(muse::Val(meta.path.toQString())));
        row.append(makeCell(muse::Val(utils::effectFamilyToString(meta.family).toQString())));
        table.append(row);
    }

    return table;
}

void PluginManagerTableViewModel::handleEdit(int row, int column)
{
    if (!isRowValid(row) || column != 0) {
        return;
    }

    const EffectMetaList effects = effectsProvider()->effectMetaList();

    if (row >= static_cast<int>(effects.size())) {
        return;
    }

    muse::uicomponents::TableViewCell* const cell = findCell(row, column);
    if (!cell) {
        return;
    }

    const bool current = cell->value().toBool();
    const bool next = !current;

    effectsProvider()->setEffectActivated(effects[row].id, next);

    cell->setValue(muse::Val(next));
}
}
