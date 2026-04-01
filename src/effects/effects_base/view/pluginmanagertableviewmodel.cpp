/*
 * Audacity: A Digital Audio Editor
 */

 #include "pluginmanagertableviewmodel.h"

 #include "internal/effectsutils.h"
 #include "effectsviewutils.h"

#include "framework/global/translation.h"
#include "framework/uicomponents/qml/Muse/UiComponents/internal/tableviewcell.h"
#include "uicomponents/qml/Muse/UiComponents/menuitem.h"

namespace au::effects {
const PluginManagerTableViewModel::EffectFilter PluginManagerTableViewModel::allPassFilter = [](const EffectMeta&) { return true; };

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

void PluginManagerTableViewModel::setTableRows(EffectMetaList effects)
{
    const auto combinedFilter = [&] (const EffectMeta& meta) {
        return m_enabledDisabledEffectFilter(meta) && m_effectFamilyFilter(meta) && m_effectTypeFilter(meta);
    };
    effects.erase(std::remove_if(effects.begin(), effects.end(), [&combinedFilter](const EffectMeta& meta) {
        return !combinedFilter(meta);
    }), effects.end());

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

muse::uicomponents::MenuItemList PluginManagerTableViewModel::enabledDisabledOptions() const
{
    return utils::toMenuItemList({
            { "all", muse::qtrc("effects", "All") },
            { "disabled", muse::qtrc("effects", "Disabled") },
            { "enabled", muse::qtrc("effects", "Enabled") },
        }, m_enabledDisabledSelectedIndex);
}

void PluginManagerTableViewModel::setEnabledDisabledSelectedIndex(int index)
{
    if (index == m_enabledDisabledSelectedIndex) {
        return;
    }

    m_enabledDisabledSelectedIndex = index;
    emit enabledDisabledSelectedIndexChanged();

    switch (index) {
    case 0:
        m_enabledDisabledEffectFilter = allPassFilter;
        break;
    case 1:
        m_enabledDisabledEffectFilter = [](const EffectMeta& meta) { return !meta.isActivated; };
        break;
    case 2:
        m_enabledDisabledEffectFilter = [](const EffectMeta& meta) { return meta.isActivated; };
        break;
    default:
        assert(false);
        break;
    }

    setTableRows(effectsProvider()->effectMetaList());
}

muse::uicomponents::MenuItemList PluginManagerTableViewModel::effectFamilyOptions() const
{
    std::vector<DropdownOption> options = { { "all", muse::qtrc("effects", "All") } };
    for (auto i = 0; i < static_cast<int>(EffectFamily::_count); ++i) {
        options.push_back({ QString::number(i), utils::effectFamilyToString(static_cast<EffectFamily>(i)) });
    }
    return utils::toMenuItemList(options, m_effectFamilySelectedIndex);
}

void PluginManagerTableViewModel::setEffectFamilySelectedIndex(int index)
{
    if (index == m_effectFamilySelectedIndex) {
        return;
    }

    m_effectFamilySelectedIndex = index;
    emit effectFamilySelectedIndexChanged();

    if (index == 0) {
        m_effectFamilyFilter = allPassFilter;
    } else {
        m_effectFamilyFilter = [family = static_cast<EffectFamily>(index - 1)](const EffectMeta& meta) {
            return meta.family == family;
        };
    }

    setTableRows(effectsProvider()->effectMetaList());
}

muse::uicomponents::MenuItemList PluginManagerTableViewModel::effectTypeOptions() const
{
    std::vector<DropdownOption> options = { { "all", muse::qtrc("effects", "All") } };
    for (auto i = 0; i < static_cast<int>(EffectType::_count); ++i) {
        options.push_back({ QString::number(i), utils::effectTypeToString(static_cast<EffectType>(i)) });
    }
    return utils::toMenuItemList(options, m_effectTypeSelectedIndex);
}

void PluginManagerTableViewModel::setEffectTypeSelectedIndex(int index)
{
    if (index == m_effectTypeSelectedIndex) {
        return;
    }

    m_effectTypeSelectedIndex = index;
    emit effectTypeSelectedIndexChanged();

    if (index == 0) {
        m_effectTypeFilter = allPassFilter;
    } else {
        m_effectTypeFilter = [type = static_cast<EffectType>(index - 1)](const EffectMeta& meta) {
            return meta.type == type;
        };
    }

    setTableRows(effectsProvider()->effectMetaList());
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
