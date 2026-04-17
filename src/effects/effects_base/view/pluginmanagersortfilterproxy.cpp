/*
 * Audacity: A Digital Audio Editor
 */

 #include "pluginmanagersortfilterproxy.h"
 #include "pluginmanagertableviewmodel.h"

 #include "internal/effectsutils.h"
 #include "framework/global/stringutils.h"

namespace au::effects {
PluginManagerSortFilterProxy::PluginManagerSortFilterProxy(PluginManagerTableViewModel* view)
    : au::uicomponents::TableSortFilterProxyModel(view), m_view(view)
{
    setMaxSortKeys(PluginManagerTableViewModel::columnCount);
}

bool PluginManagerSortFilterProxy::acceptsRow(int sourceRow) const
{
    if (!m_view) {
        return true;
    }

    const auto& effects = m_view->m_allEffects;
    if (sourceRow < 0 || sourceRow >= static_cast<int>(effects.size())) {
        return false;
    }

    const EffectMeta& meta = effects[sourceRow];

    if (!m_view->m_searchText.isEmpty()) {
        const QString searchTextLower = m_view->m_searchText.toLower();
        if (!meta.title.toQString().toLower().contains(searchTextLower)) {
            return false;
        }
    }

    return m_view->m_acceptEnabledDisabledState(meta)
           && m_view->m_acceptFamily(meta)
           && m_view->m_acceptType(meta);
}

int PluginManagerSortFilterProxy::compareCells(int column, int leftSourceRow, int rightSourceRow) const
{
    if (!m_view) {
        return 0;
    }

    const auto& effects = m_view->m_allEffects;
    const int n = static_cast<int>(effects.size());
    if (leftSourceRow < 0 || leftSourceRow >= n || rightSourceRow < 0 || rightSourceRow >= n) {
        return 0;
    }

    const EffectMeta& a = effects[leftSourceRow];
    const EffectMeta& b = effects[rightSourceRow];

    auto cmpBool = [](bool l, bool r) { return static_cast<int>(l) - static_cast<int>(r); };
    auto cmpStr = [](const muse::String& l, const muse::String& r) {
        if (muse::strings::lessThanCaseInsensitive(l, r)) {
            return -1;
        }
        if (muse::strings::lessThanCaseInsensitive(r, l)) {
            return 1;
        }
        return 0;
    };

    switch (column) {
    case PluginManagerTableViewModel::enabledDisabledColumnIndex:
        return cmpBool(a.isActivated, b.isActivated);
    case PluginManagerTableViewModel::nameColumnIndex:
        return cmpStr(a.title, b.title);
    case PluginManagerTableViewModel::pathColumnIndex:
        return cmpStr(a.path.toString(), b.path.toString());
    case PluginManagerTableViewModel::typeColumnIndex:
        return cmpStr(utils::effectFamilyToString(a.family), utils::effectFamilyToString(b.family));
    default:
        break;
    }
    return 0;
}
}
