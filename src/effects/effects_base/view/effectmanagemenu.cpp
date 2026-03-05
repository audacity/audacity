#include "effectmanagemenu.h"

#include "effects/effects_base/effectstypes.h"
#include "au3-components/EffectInterface.h"
#include "au3-effects/Effect.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include <algorithm>

using namespace muse;
using namespace muse::uicomponents;
using namespace muse::actions;
using namespace au::effects;

void EffectManageMenu::load()
{
    AbstractMenuModel::load();

    const EffectId effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);

    // Subscribe on user presets change (replace subscription each time with new effectId)
    presetsController()->userPresetsChanged().onReceive(this, [this, effectId](const EffectId& eid) {
        if (effectId != eid) {
            return;
        }

        reload(effectId, m_instanceId);
    }, muse::async::Asyncable::Mode::SetReplace);

    presetsController()->presetSaved().onReceive(this, [this](const PresetSavedInfo& info) {
        if (info.instanceId != m_instanceId) {
            return;
        }

        const QString presetId = au3::wxToString(info.presetId).toQString();
        if (presetId.isEmpty() || m_currentPreset == presetId) {
            return;
        }

        const bool oldCanReset = canResetPreset();
        m_currentPreset = presetId;
        m_isPresetUnsaved = false;
        updatePresetDisplayNames();
        emit presetChanged();
        emit canDeletePresetChanged();
        if (oldCanReset != canResetPreset()) {
            emit canResetPresetChanged();
        }
    }, muse::async::Asyncable::Mode::SetReplace);

    parametersProvider()->parameterChanged().onReceive(this, [this](const ParameterChangedData& data) {
        if (data.instanceId != m_instanceId || m_currentPreset.isEmpty()) {
            return;
        }

        setPresetUnsaved(isCurrentPresetUnsaved());
    }, muse::async::Asyncable::Mode::SetReplace);

    instancesRegister()->settingsChanged(m_instanceId).onNotify(this, [this] {
        if (m_currentPreset.isEmpty()) {
            return;
        }

        setPresetUnsaved(isCurrentPresetUnsaved());
    }, muse::async::Asyncable::Mode::SetReplace);

    reload(effectId, m_instanceId);

    if (m_persistLastUsedPreset && !m_hasLoadedInitialPreset) {
        m_hasLoadedInitialPreset = true;
        const QString storedPresetId = QString::fromStdString(configuration()->lastUsedPreset(effectId));
        if (!storedPresetId.isEmpty() && hasPreset(storedPresetId)) {
            const bool oldCanReset = canResetPreset();
            m_currentPreset = storedPresetId;
            resetPreset();
            m_isPresetUnsaved = false;
            updatePresetDisplayNames();
            emit presetChanged();
            emit canDeletePresetChanged();
            if (oldCanReset != canResetPreset()) {
                emit canResetPresetChanged();
            }
        }
    }
}

void EffectManageMenu::reload(const EffectId& effectId, const EffectInstanceId& instanceId)
{
    assert(!effectId.empty());
    assert(instanceId != 0);

    MenuItemList items;
    QVariantList presets;
    m_basePresetNames.clear();
    m_factoryPresets.clear();

    presets << QVariantMap {
        { "id", "default" },
        { "name", muse::qtrc("effects", "Default preset") } };
    m_basePresetNames.insert("default", muse::qtrc("effects", "Default preset"));

    auto makeApplyAction = [](const EffectInstanceId& iid, const PresetId& p) {
        ActionQuery q("action://effects/presets/apply");
        q.addParam("instanceId", Val(iid));
        q.addParam("presetId", Val(au3::wxToStdString(p)));
        return q;
    };

    // user
    PresetIdList userPresets = presetsController()->userPresets(effectId);
    m_userPresets.clear();
    {
        MenuItem* menuItem = makeMenu(muse::TranslatableString("effects", "User Presets"), {});
        if (userPresets.empty()) {
            menuItem->setState(ui::UiActionState::make_disabled());
        } else {
            MenuItemList subitems;
            for (const PresetId& p : userPresets) {
                String name = au3::wxToString(p);
                m_userPresets << name.toQString();
                MenuItem* item = makeMenuItem(makeApplyAction(instanceId, p).toString(), muse::TranslatableString::untranslatable(name));
                item->setId("user_apply_" + name);
                subitems << item;

                presets << QVariantMap {
                    { "id", name.toQString() },
                    { "name", name.toQString() } };
                m_basePresetNames.insert(name.toQString(), name.toQString());
            }
            menuItem->setSubitems(subitems);
        }
        items << menuItem;
    }

    {
        ActionQuery q("action://effects/presets/save_as");
        q.addParam("instanceId", Val(instanceId));
        MenuItem* item = makeMenuItem(q.toString());
        items << item;
    }

    items << makeSeparator();

    // factory
    {
        PresetIdList factoryPresets = presetsController()->factoryPresets(effectId);
        MenuItem* menuItem = makeMenu(muse::TranslatableString("effects", "Factory Presets"), {});

        MenuItemList subitems;
        MenuItem* defItem = makeMenuItem(makeApplyAction(instanceId, "default").toString(), muse::TranslatableString("effects", "Defaults"));
        defItem->setId("factory_apply_default");
        subitems << defItem;

        for (const PresetId& p : factoryPresets) {
            String name = au3::wxToString(p);
            m_factoryPresets << name.toQString();
            MenuItem* item = makeMenuItem(makeApplyAction(instanceId, p).toString(), muse::TranslatableString::untranslatable(name));
            QString id = "factory_apply_" + name;
            item->setId(id);
            subitems << item;

            presets << QVariantMap {
                { "id", name.toQString() },
                { "name", name.toQString() } };
            m_basePresetNames.insert(name.toQString(), name.toQString());
        }
        menuItem->setSubitems(subitems);

        items << menuItem;
    }

    items << makeSeparator();

    // import / export
    {
        ActionQuery q("action://effects/presets/import");
        q.addParam("instanceId", Val(instanceId));
        MenuItem* item = makeMenuItem(q.toString());
        items << item;
    }

    {
        ActionQuery q("action://effects/presets/export");
        q.addParam("instanceId", Val(instanceId));
        MenuItem* item = makeMenuItem(q.toString());
        items << item;
    }

    // UI Mode toggle - only for external plugins with vendor UI (VST3, LV2, Audio Units)
    // Built-in and Nyquist effects don't have a "Vendor UI" concept
    {
        const EffectMeta effectMeta = effectsProvider()->meta(effectId);
        const bool hasVendorUI = effectMeta.family != EffectFamily::Builtin
                                 && effectMeta.family != EffectFamily::Nyquist
                                 && effectMeta.family != EffectFamily::Unknown;

        if (hasVendorUI) {
            items << makeSeparator();
            ActionQuery q("action://effects/toggle_vendor_ui");
            q.addParam("effectId", Val(effectId.toStdString()));
            MenuItem* item = makeMenuItem(q.toString());

            // Manually set the checked state based on current UI mode
            if (item) {
                const bool isVendorUI = configuration()->effectUIMode(effectId) == EffectUIMode::VendorUI;
                ui::UiActionState state = item->state();
                state.checked = isVendorUI;
                item->setState(state);
            }

            items << item;
        }
    }

    setItems(items);
    m_presets = presets;

    const bool oldCanReset = canResetPreset();
    if (!m_currentPreset.isEmpty() && !hasPreset(m_currentPreset)) {
        m_currentPreset.clear();
        m_isPresetUnsaved = false;
        emit presetChanged();
    } else if (!m_currentPreset.isEmpty()) {
        m_isPresetUnsaved = isCurrentPresetUnsaved();
    } else {
        m_isPresetUnsaved = false;
    }

    updatePresetDisplayNames();
    emit canDeletePresetChanged();
    if (oldCanReset != canResetPreset()) {
        emit canResetPresetChanged();
    }
}

int EffectManageMenu::instanceId_prop() const
{
    return m_instanceId;
}

void EffectManageMenu::setInstanceId_prop(int newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }
    const bool oldCanReset = canResetPreset();
    m_instanceId = newInstanceId;
    m_currentPreset.clear();
    m_isPresetUnsaved = false;
    m_hasLoadedInitialPreset = false;
    emit instanceIdChanged();
    if (oldCanReset != canResetPreset()) {
        emit canResetPresetChanged();
    }
}

QVariantList EffectManageMenu::presets()
{
    return m_presets;
}

QString EffectManageMenu::preset() const
{
    return m_currentPreset;
}

void EffectManageMenu::setPreset(QString presetId)
{
    const bool oldCanReset = canResetPreset();
    m_currentPreset = presetId;
    resetPreset();
    m_isPresetUnsaved = false;
    updatePresetDisplayNames();
    emit presetChanged();
    emit canDeletePresetChanged();
    if (oldCanReset != canResetPreset()) {
        emit canResetPresetChanged();
    }
}

bool EffectManageMenu::enabled() const
{
    return m_presets.size() > 1; // Do not take default preset into account
}

bool EffectManageMenu::canDeletePreset() const
{
    return m_userPresets.contains(m_currentPreset);
}

bool EffectManageMenu::canResetPreset() const
{
    return !m_currentPreset.isEmpty() && m_isPresetUnsaved;
}

void EffectManageMenu::resetPreset()
{
    if (m_currentPreset.isEmpty()) {
        return;
    }

    ActionQuery q("action://effects/presets/apply");
    q.addParam("instanceId", Val(m_instanceId));
    q.addParam("presetId", Val(m_currentPreset.toStdString()));
    dispatcher()->dispatch(q);
}

void EffectManageMenu::savePresetAs()
{
    ActionQuery q("action://effects/presets/save_as");
    q.addParam("instanceId", Val(m_instanceId));
    dispatcher()->dispatch(q);
}

void EffectManageMenu::deletePreset()
{
    if (!canDeletePreset()) {
        return;
    }

    const EffectId effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);
    if (effectId.empty()) {
        return;
    }

    ActionQuery q("action://effects/presets/delete");
    q.addParam("effectId", Val(effectId.toStdString()));
    q.addParam("presetId", Val(m_currentPreset.toStdString()));
    dispatcher()->dispatch(q);
}

void EffectManageMenu::commitSelectedPreset()
{
    if (!m_persistLastUsedPreset) {
        return;
    }

    const EffectId effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);
    if (effectId.empty() || m_currentPreset.isEmpty() || !hasPreset(m_currentPreset)) {
        return;
    }

    configuration()->setLastUsedPreset(effectId, m_currentPreset.toStdString());
}

bool EffectManageMenu::useVendorUI() const
{
    const EffectInstanceId instanceId = m_instanceId;
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(instanceId);
    if (effectId.empty()) {
        return true; // Default to vendor UI
    }
    return configuration()->effectUIMode(effectId) == EffectUIMode::VendorUI;
}

void EffectManageMenu::setUseVendorUI(const bool value)
{
    const EffectInstanceId instanceId = m_instanceId;
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(instanceId);
    if (effectId.empty()) {
        return;
    }

    const EffectUIMode mode = value ? EffectUIMode::VendorUI : EffectUIMode::FallbackUI;
    configuration()->setEffectUIMode(effectId, mode);
    emit useVendorUIChanged();
}

bool EffectManageMenu::persistLastUsedPreset() const
{
    return m_persistLastUsedPreset;
}

void EffectManageMenu::setPersistLastUsedPreset(bool value)
{
    if (m_persistLastUsedPreset == value) {
        return;
    }

    m_persistLastUsedPreset = value;
    m_hasLoadedInitialPreset = false;
    emit persistLastUsedPresetChanged();
}

bool EffectManageMenu::hasPreset(const QString& presetId) const
{
    if (presetId.isEmpty()) {
        return false;
    }

    const auto it = std::find_if(m_presets.cbegin(), m_presets.cend(), [&](const QVariant& v) {
        return v.toMap().value("id").toString() == presetId;
    });

    return it != m_presets.cend();
}

bool EffectManageMenu::isUserPreset(const QString& presetId) const
{
    return m_userPresets.contains(presetId);
}

bool EffectManageMenu::isFactoryPreset(const QString& presetId) const
{
    return m_factoryPresets.contains(presetId);
}

int EffectManageMenu::factoryPresetIndex(const QString& presetId) const
{
    return m_factoryPresets.indexOf(presetId);
}

bool EffectManageMenu::isCurrentPresetUnsaved() const
{
    if (m_currentPreset.isEmpty()) {
        return false;
    }

    const EffectId effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);
    Effect* effect = effectsProvider()->effect(effectId);
    const EffectSettings* settings = instancesRegister()->settingsById(m_instanceId);
    if (!effect || !settings) {
        return false;
    }

    wxString currentSettingsString;
    if (!effect->SaveSettingsAsString(*settings, currentSettingsString)) {
        return false;
    }

    auto& definition = effect->GetDefinition();
    EffectSettings presetSettings = definition.MakeSettings();
    if (!definition.CopySettingsContents(*settings, presetSettings)) {
        return false;
    }

    OptionalMessage loadResult;
    if (m_currentPreset == "default") {
        loadResult = definition.LoadFactoryDefaults(presetSettings);
    } else if (isUserPreset(m_currentPreset)) {
        const auto name = au3::wxFromString(String::fromQString(m_currentPreset));
        loadResult = definition.LoadUserPreset(UserPresetsGroup(name), presetSettings);
    } else if (isFactoryPreset(m_currentPreset)) {
        const int index = factoryPresetIndex(m_currentPreset);
        if (index < 0) {
            return false;
        }
        loadResult = definition.LoadFactoryPreset(index, presetSettings);
    } else {
        return false;
    }

    if (!loadResult) {
        return false;
    }

    wxString selectedPresetSettingsString;
    if (!effect->SaveSettingsAsString(presetSettings, selectedPresetSettingsString)) {
        return false;
    }

    return currentSettingsString != selectedPresetSettingsString;
}

void EffectManageMenu::setPresetUnsaved(bool unsaved)
{
    if (m_isPresetUnsaved == unsaved) {
        return;
    }

    const bool oldCanReset = canResetPreset();
    m_isPresetUnsaved = unsaved;
    updatePresetDisplayNames();
    if (oldCanReset != canResetPreset()) {
        emit canResetPresetChanged();
    }
}

void EffectManageMenu::updatePresetDisplayNames()
{
    for (int i = 0; i < m_presets.size(); ++i) {
        QVariantMap map = m_presets[i].toMap();
        const QString id = map.value("id").toString();
        const QString baseName = m_basePresetNames.value(id, map.value("name").toString());
        QString displayName = baseName;
        if (m_isPresetUnsaved && id == m_currentPreset) {
            displayName += "*";
        }
        map.insert("name", displayName);
        m_presets[i] = map;
    }

    emit presetsChanged();
}
