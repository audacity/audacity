/*
 * Audacity: A Digital Audio Editor
 */

 #include "pluginmanagertableviewmodel.h"

 #include "internal/effectsutils.h"
 #include "pluginmanagertableviewverticalheader.h"
 #include "pluginmanagersortfilterproxy.h"
 #include "effectsviewutils.h"

#include "framework/global/translation.h"
#include "framework/global/modularity/ioc.h"
#include "framework/uicomponents/qml/Muse/UiComponents/internal/tableviewcell.h"
#include "framework/uicomponents/qml/Muse/UiComponents/menuitem.h"

namespace au::effects {
namespace {
constexpr auto enabledColumnWidth = 100;
constexpr auto nameColumnWidth = 152;
constexpr auto vendorColumnWidth = 152;
constexpr auto pathColumnWidth = 296;
constexpr auto typeColumnWidth = 152;
}

const PluginManagerTableViewModel::EffectFilter PluginManagerTableViewModel::allPassFilter = [](const EffectMeta&) { return true; };

PluginManagerTableViewModel::PluginManagerTableViewModel(QObject* parent)
    : AbstractTableViewModel(parent), Contextable(muse::iocCtxForQmlObject(this)),
    m_sortFilterProxy(new PluginManagerSortFilterProxy(this))
{
    m_sortFilterProxy->setSourceModel(this);
}

void PluginManagerTableViewModel::componentComplete()
{
    setHorizontalHeaders(makeHorizontalHeaders());
    m_initialState = effectsProvider()->effectMetaList();
    m_initialState.erase(std::remove_if(m_initialState.begin(), m_initialState.end(), [](const EffectMeta& meta) {
        // 3rd-party plugins typically also have instruments (e.g. VSTi) but we don't support them yet.
        return meta.type == EffectType::Unknown;
    }), m_initialState.end());
    rebuildSourceTable(m_initialState);

    // Initial sort: start from least important.
    m_sortFilterProxy->toggleColumnSort(s_enabledDisabledColumnIndex);
    m_sortFilterProxy->toggleColumnSort(s_pathColumnIndex);
    m_sortFilterProxy->toggleColumnSort(s_typeColumnIndex);
    m_sortFilterProxy->toggleColumnSort(s_vendorColumnIndex);
    m_sortFilterProxy->toggleColumnSort(s_nameColumnIndex);
}

void PluginManagerTableViewModel::aboutToDestroy()
{
    if (!m_changesConfirmed) {
        for (const auto& effectId : m_editedEffects) {
            const auto it = std::find_if(m_initialState.begin(), m_initialState.end(), [&](const EffectMeta& meta) {
                return meta.id == effectId;
            });
            IF_ASSERT_FAILED(it != m_initialState.end()) {
                continue;
            }
            effectsProvider()->setEffectActivated(effectId, it->isActivated);
        }
    }

    effectsProvider()->save();
}

void PluginManagerTableViewModel::rebuildSourceTable(EffectMetaList effects)
{
    m_allEffects = std::move(effects);
    setVerticalHeaders(makeVerticalHeaders(m_allEffects));
    setTable(makeTable(m_allEffects));
    m_sortFilterProxy->invalidateFilters();
}

void PluginManagerTableViewModel::setSearchText(const QString& searchText)
{
    if (m_searchText == searchText) {
        return;
    }
    m_searchText = searchText;
    m_sortFilterProxy->invalidateFilters();
}

void PluginManagerTableViewModel::toggleColumnSort(int column)
{
    m_sortFilterProxy->toggleColumnSort(column);
}

muse::uicomponents::MenuItemList PluginManagerTableViewModel::enabledDisabledOptions()
{
    return utils::toMenuItemList({
            { "all", muse::qtrc("effects", "All") },
            { "disabled", muse::qtrc("effects", "Disabled") },
            { "enabled", muse::qtrc("effects", "Enabled") },
        }, m_enabledDisabledSelectedIndex, this);
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
        m_acceptEnabledDisabledState = allPassFilter;
        break;
    case 1:
        m_acceptEnabledDisabledState = [](const EffectMeta& meta) { return !meta.isActivated; };
        break;
    case 2:
        m_acceptEnabledDisabledState = [](const EffectMeta& meta) { return meta.isActivated; };
        break;
    default:
        assert(false);
        break;
    }

    m_sortFilterProxy->invalidateFilters();
}

muse::uicomponents::MenuItemList PluginManagerTableViewModel::effectFamilyOptions()
{
    std::vector<DropdownOption> options = { { "all", muse::qtrc("effects", "All") } };
    for (auto i = 0; i < static_cast<int>(EffectFamily::_count); ++i) {
        options.push_back({ QString::number(i), utils::effectFamilyToString(static_cast<EffectFamily>(i)) });
    }
    return utils::toMenuItemList(options, m_effectFamilySelectedIndex, this);
}

void PluginManagerTableViewModel::setEffectFamilySelectedIndex(int index)
{
    if (index == m_effectFamilySelectedIndex) {
        return;
    }

    m_effectFamilySelectedIndex = index;
    emit effectFamilySelectedIndexChanged();

    if (index == 0) {
        m_acceptFamily = allPassFilter;
    } else {
        m_acceptFamily = [family = static_cast<EffectFamily>(index - 1)](const EffectMeta& meta) {
            return meta.family == family;
        };
    }

    m_sortFilterProxy->invalidateFilters();
}

muse::uicomponents::MenuItemList PluginManagerTableViewModel::effectTypeOptions()
{
    std::vector<DropdownOption> options = { { "all", muse::qtrc("effects", "All") } };
    for (auto i = 0; i < static_cast<int>(EffectType::_count); ++i) {
        options.push_back({ QString::number(i), utils::effectTypeToString(static_cast<EffectType>(i)) });
    }
    return utils::toMenuItemList(options, m_effectTypeSelectedIndex, this);
}

void PluginManagerTableViewModel::setEffectTypeSelectedIndex(int index)
{
    if (index == m_effectTypeSelectedIndex) {
        return;
    }

    m_effectTypeSelectedIndex = index;
    emit effectTypeSelectedIndexChanged();

    if (index == 0) {
        m_acceptType = allPassFilter;
    } else {
        m_acceptType = [type = static_cast<EffectType>(index - 1)](const EffectMeta& meta) {
            return meta.type == type;
        };
    }

    m_sortFilterProxy->invalidateFilters();
}

int PluginManagerTableViewModel::totalWidth() const
{
    return enabledColumnWidth + nameColumnWidth + vendorColumnWidth + pathColumnWidth + typeColumnWidth;
}

void PluginManagerTableViewModel::setAlsoRescanBrokenPlugins(bool alsoRescanBrokenPlugins)
{
    if (alsoRescanBrokenPlugins == m_alsoRescanBrokenPlugins) {
        return;
    }

    m_alsoRescanBrokenPlugins = alsoRescanBrokenPlugins;
    emit alsoRescanBrokenPluginsChanged();
}

au::uicomponents::TableSortFilterProxyModel* PluginManagerTableViewModel::sortFilterProxy() const
{
    return m_sortFilterProxy;
}

QVector<muse::uicomponents::TableViewHeader*> PluginManagerTableViewModel::makeHorizontalHeaders()
{
    using namespace muse::uicomponents;

    QVector<TableViewHeader*> hHeaders;

    hHeaders << makeHorizontalHeader(muse::qtrc("effects", "Enabled"),
                                     static_cast<TableViewCellType::Type>(PluginManagerTableViewCellType::Type::Enabled),
                                     TableViewCellEditMode::Mode::StartInEdit, enabledColumnWidth);
    hHeaders << makeHorizontalHeader(muse::qtrc("effects", "Name"),
                                     TableViewCellType::Type::String, TableViewCellEditMode::Mode::DoubleClick, nameColumnWidth);
    hHeaders << makeHorizontalHeader(muse::qtrc("effects", "Vendor"),
                                     TableViewCellType::Type::String, TableViewCellEditMode::Mode::DoubleClick, vendorColumnWidth);
    hHeaders << makeHorizontalHeader(muse::qtrc("effects", "Path"),
                                     TableViewCellType::Type::String, TableViewCellEditMode::Mode::DoubleClick, pathColumnWidth);
    hHeaders << makeHorizontalHeader(muse::qtrc("effects", "Type"),
                                     TableViewCellType::Type::String, TableViewCellEditMode::Mode::DoubleClick, typeColumnWidth);

    return hHeaders;
}

QVector<muse::uicomponents::TableViewHeader*> PluginManagerTableViewModel::makeVerticalHeaders(const EffectMetaList& effects)
{
    using namespace muse::uicomponents;

    QVector<TableViewHeader*> vHeaders;

    for (auto i = 0; i < static_cast<int>(effects.size()); ++i) {
        auto* header = new PluginManagerTableViewVerticalHeader(this);
        header->setEffectId(effects[i].id);
        vHeaders << header;
    }

    return vHeaders;
}

QVector<QVector<muse::uicomponents::TableViewCell*> > PluginManagerTableViewModel::makeTable(const EffectMetaList& effects)
{
    QVector<QVector<muse::uicomponents::TableViewCell*> > table;

    for (const auto& meta : effects) {
        auto title = meta.title.toQString();
        if (!meta.isLoadable) {
            title = muse::qtrc("effects", "%1 (broken)").arg(title);
        }

        QVector<muse::uicomponents::TableViewCell*> row;
        row.append(makeCell(muse::Val(meta.isActivated && meta.isLoadable)));
        row.append(makeCell(muse::Val(std::move(title))));
        row.append(makeCell(muse::Val(meta.vendor.toQString())));
        row.append(makeCell(muse::Val(meta.path.toQString())));
        row.append(makeCell(muse::Val(utils::effectFamilyToString(meta.family).toQString())));
        table.append(row);
    }

    return table;
}

void PluginManagerTableViewModel::handleEdit(int proxyRow, int column)
{
    const int sourceRow = m_sortFilterProxy->mapRowToSource(proxyRow);
    if (!isRowValid(sourceRow) || column != 0) {
        return;
    }

    PluginManagerTableViewVerticalHeader* const verticalHeader
        = dynamic_cast<PluginManagerTableViewVerticalHeader*>(findVerticalHeader(sourceRow));
    IF_ASSERT_FAILED(verticalHeader) {
        return;
    }

    if (!effectsProvider()->meta(verticalHeader->effectId()).isLoadable) {
        return;
    }

    muse::uicomponents::TableViewCell* const cell = findCell(sourceRow, column);
    IF_ASSERT_FAILED(cell) {
        return;
    }

    const bool current = cell->value().toBool();
    const bool next = !current;

    effectsProvider()->setEffectActivated(verticalHeader->effectId(), next);
    m_editedEffects.insert(verticalHeader->effectId());

    cell->setValue(muse::Val(next));

    // Keep m_allEffects in sync so the proxy sees the new value when re-sorting.
    if (sourceRow < static_cast<int>(m_allEffects.size())) {
        m_allEffects[sourceRow].isActivated = next;
    }

    const QModelIndex idx = index(sourceRow, column);
    emit dataChanged(idx, idx);
}

void PluginManagerTableViewModel::rescanPlugins()
{
    // Save changes so far
    effectsProvider()->save();

    if (m_alsoRescanBrokenPlugins) {
        effectsProvider()->forgetPlugins([](const EffectMeta& meta) {
            return !meta.isLoadable;
        });
    }

    const auto excludeFromScan = [this](const EffectMeta& meta) {
        return !m_acceptFamily(meta) || !m_acceptType(meta);
    };
    effectsProvider()->rescanPlugins(*interactive(), *registerAudioPluginsScenario(), excludeFromScan);

    rebuildSourceTable(effectsProvider()->effectMetaList());
}

void PluginManagerTableViewModel::accept()
{
    m_changesConfirmed = true;
}
}
