/*
 * Audacity: A Digital Audio Editor
 */
#include "pluginmanagertoppanelmodel.h"

#include "internal/effectsutils.h"

#include "dropdownoptionsmodel.h"
#include "framework/global/translation.h"

#include <set>

namespace au::effects {
PluginManagerTopPanelModel::PluginManagerTopPanelModel(QObject* parent)
    : QObject(parent),
    m_showModel(new DropdownOptionsModel(muse::qtrc("effects", "Show:"), this)),
    m_typeModel(new DropdownOptionsModel(muse::qtrc("effects", "Type:"), this)),
    m_categoryModel(new DropdownOptionsModel(muse::qtrc("effects", "Category:"), this))
{
}

void PluginManagerTopPanelModel::componentComplete()
{
    const EffectMetaList effects = effectsProvider()->effectMetaList();
    std::set<EffectFamily> families;
    std::set<EffectType> types;
    for (const EffectMeta& meta : effects) {
        families.insert(meta.family);
        types.insert(meta.type);
    }

    m_showModel->setOptions({
            { "all", muse::qtrc("effects", "All") },
            { "disabled", muse::qtrc("effects", "Disabled") },
            { "enabled", muse::qtrc("effects", "Enabled") },
        });

    std::vector<DropdownOption> typeOptions = { { "all", muse::qtrc("effects", "All") } };
    std::transform(families.begin(), families.end(), std::back_inserter(typeOptions), [](EffectFamily family) {
        const QString title = utils::effectFamilyToString(family);
        return DropdownOption { QString::number(static_cast<int>(family)), title };
    });
    m_typeModel->setOptions(typeOptions);

    std::vector<DropdownOption> categoryOptions = { { "all", muse::qtrc("effects", "All") } };
    std::transform(types.begin(), types.end(), std::back_inserter(categoryOptions), [](EffectType type) {
        const QString title = utils::effectTypeToString(type);
        return DropdownOption { QString::number(static_cast<int>(type)), title };
    });
    m_categoryModel->setOptions(categoryOptions);
}

DropdownOptionsModel* PluginManagerTopPanelModel::showModel() const
{
    return m_showModel;
}

DropdownOptionsModel* PluginManagerTopPanelModel::typeModel() const
{
    return m_typeModel;
}

DropdownOptionsModel* PluginManagerTopPanelModel::categoryModel() const
{
    return m_categoryModel;
}
}
