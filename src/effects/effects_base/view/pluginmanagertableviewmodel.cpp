/*
 * Audacity: A Digital Audio Editor
 */

 #include "pluginmanagertableviewmodel.h"

 #include "internal/effectsutils.h"
 #include "pluginmanagertableviewverticalheader.h"
 #include "effectsviewutils.h"

 #include "framework/global/stringutils.h"
#include "framework/global/translation.h"
#include "framework/global/modularity/ioc.h"
#include "framework/uicomponents/qml/Muse/UiComponents/internal/tableviewcell.h"
#include "framework/uicomponents/qml/Muse/UiComponents/menuitem.h"

namespace au::effects {
namespace {
constexpr auto enabledColumnWidth = 100;
constexpr auto nameColumnWidth = 152;
constexpr auto pathColumnWidth = 296;
constexpr auto typeColumnWidth = 152;

constexpr auto enabledDisabledColumnIndex = 0;
constexpr auto nameColumnIndex = 1;
constexpr auto pathColumnIndex = 2;
constexpr auto typeColumnIndex = 3;
}

const PluginManagerTableViewModel::EffectFilter PluginManagerTableViewModel::allPassFilter = [](const EffectMeta&) { return true; };

PluginManagerTableViewModel::PluginManagerTableViewModel(QObject* parent)
    : AbstractTableViewModel(parent), Contextable(muse::iocCtxForQmlObject(this)) {}

void PluginManagerTableViewModel::componentComplete()
{
    setHorizontalHeaders(makeHorizontalHeaders());
    m_initialState = effectsProvider()->effectMetaList();
    setTableRows(m_initialState);

    effectsProvider()->effectMetaListChanged().onNotify(this, [this]() {
        setTableRows(effectsProvider()->effectMetaList());
    });

    // Initial sort: start from least important.
    toggleColumnSort(enabledDisabledColumnIndex);
    toggleColumnSort(pathColumnIndex);
    toggleColumnSort(typeColumnIndex);
    toggleColumnSort(nameColumnIndex);
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

void PluginManagerTableViewModel::setTableRows(EffectMetaList effects)
{
    const QString searchTextLower = m_searchText.toLower();

    effects.erase(std::remove_if(effects.begin(), effects.end(), [this, &searchTextLower](const EffectMeta& meta) {
        if (!meta.title.toQString().toLower().contains(searchTextLower)) {
            return true;
        }
        return !m_acceptEnabledDisabledState(meta) || !m_acceptFamily(meta) || !m_acceptType(meta);
    }), effects.end());

    applySorting(effects);
    setVerticalHeaders(makeVerticalHeaders(effects));
    setTable(makeTable(effects));
}

void PluginManagerTableViewModel::setSearchText(const QString& searchText)
{
    m_searchText = searchText;
    setTableRows(effectsProvider()->effectMetaList());
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

    setTableRows(effectsProvider()->effectMetaList());
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

    setTableRows(effectsProvider()->effectMetaList());
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

    setTableRows(effectsProvider()->effectMetaList());
}

int PluginManagerTableViewModel::totalWidth() const
{
    return enabledColumnWidth + nameColumnWidth + pathColumnWidth + typeColumnWidth;
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

    PluginManagerTableViewVerticalHeader* const verticalHeader
        = dynamic_cast<PluginManagerTableViewVerticalHeader*>(findVerticalHeader(row));
    IF_ASSERT_FAILED(verticalHeader) {
        return;
    }

    if (!effectsProvider()->meta(verticalHeader->effectId()).isLoadable) {
        return;
    }

    muse::uicomponents::TableViewCell* const cell = findCell(row, column);
    IF_ASSERT_FAILED(cell) {
        return;
    }

    const bool current = cell->value().toBool();
    const bool next = !current;

    effectsProvider()->setEffectActivated(verticalHeader->effectId(), next);
    m_editedEffects.insert(verticalHeader->effectId());

    cell->setValue(muse::Val(next));
}

void PluginManagerTableViewModel::rescanPlugins()
{
    const auto excludeFromScan = [this](const EffectMeta& meta) {
        return !m_acceptFamily(meta) || !m_acceptType(meta);
    };
    effectsProvider()->rescanPlugins(*interactive(), *registerAudioPluginsScenario(), excludeFromScan);
}

void PluginManagerTableViewModel::toggleColumnSort(int column)
{
    auto it = std::find_if(m_sortPipeline.begin(), m_sortPipeline.end(), [column](const SortEntry& entry) {
        return entry.column == column;
    });

    if (it == m_sortPipeline.end()) {
        m_sortPipeline.push_back({ column, true });
        if (m_sortPipeline.size() > 4) {
            m_sortPipeline.erase(m_sortPipeline.begin());
        }
    } else {
        const auto ascending = !it->ascending;
        m_sortPipeline.erase(it);
        m_sortPipeline.push_back({ column, ascending });
    }

    setTableRows(effectsProvider()->effectMetaList());
}

void PluginManagerTableViewModel::applySorting(EffectMetaList& effects) const
{
    for (const auto& entry : m_sortPipeline) {
        const auto cmp = [&entry](const EffectMeta& a, const EffectMeta& b) {
            switch (entry.column) {
            case enabledDisabledColumnIndex:
                return static_cast<int>(a.isActivated) < static_cast<int>(b.isActivated);
            case nameColumnIndex:
                return muse::strings::lessThanCaseInsensitive(a.title, b.title);
            case pathColumnIndex:
                return muse::strings::lessThanCaseInsensitive(a.path.toString(), b.path.toString());
            case typeColumnIndex:
                return muse::strings::lessThanCaseInsensitive(utils::effectFamilyToString(a.family),
                                                              utils::effectFamilyToString(b.family));
            default:
                assert(false);
            }
            return false;
        };

        if (entry.ascending) {
            std::stable_sort(effects.begin(), effects.end(), cmp);
        } else {
            std::stable_sort(effects.begin(), effects.end(), [&cmp](const EffectMeta& a, const EffectMeta& b) {
                return cmp(b, a);
            });
        }
    }
}

void PluginManagerTableViewModel::accept()
{
    m_changesConfirmed = true;
}
}
