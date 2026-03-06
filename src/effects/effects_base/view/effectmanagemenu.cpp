#include "effectmanagemenu.h"

#include "effects/effects_base/effectstypes.h"
#include "au3-components/EffectInterface.h"
#include "au3-effects/Effect.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include <algorithm>

using namespace muse;
using namespace muse::actions;
using namespace au::effects;

namespace {
constexpr int USER_PRESET_ICON_CODE = 0xEF99;
}

EffectManageMenu::EffectManageMenu(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
    m_saveContextMenu = new EffectSaveContextMenu(this);
    m_presetsContextMenu = new PresetsContextMenuModel(this);

    connect(m_presetsContextMenu, &PresetsContextMenuModel::useVendorUIChanged,
            this, &EffectManageMenu::useVendorUIChanged);
}

void EffectManageMenu::load()
{
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

    QVariantList presets;
    m_basePresetNames.clear();
    m_factoryPresets.clear();

    presets << QVariantMap {
        { "id", "default" },
        { "name", muse::qtrc("effects", "Default preset") },
        { "iconCode", 0 } };
    m_basePresetNames.insert("default", muse::qtrc("effects", "Default preset"));

    // Build presets model for the dropdown, but do not expose duplicated
    // preset apply/save actions in the three-dots manage menu.
    PresetIdList userPresets = presetsController()->userPresets(effectId);
    m_userPresets.clear();
    for (const PresetId& p : userPresets) {
        String name = au3::wxToString(p);
        m_userPresets << name.toQString();
        presets << QVariantMap {
            { "id", name.toQString() },
            { "name", name.toQString() },
            { "iconCode", USER_PRESET_ICON_CODE } };
        m_basePresetNames.insert(name.toQString(), name.toQString());
    }

    PresetIdList factoryPresets = presetsController()->factoryPresets(effectId);
    for (const PresetId& p : factoryPresets) {
        String name = au3::wxToString(p);
        m_factoryPresets << name.toQString();
        presets << QVariantMap {
            { "id", name.toQString() },
            { "name", name.toQString() },
            { "iconCode", 0 } };
        m_basePresetNames.insert(name.toQString(), name.toQString());
    }

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

QObject* EffectManageMenu::saveContextMenu()
{
    IF_ASSERT_FAILED(m_saveContextMenu) {
        return nullptr;
    }

    m_saveContextMenu->setInstanceId_prop(m_instanceId);
    m_saveContextMenu->setPreset(m_currentPreset);
    m_saveContextMenu->load();
    return m_saveContextMenu;
}

QObject* EffectManageMenu::presetContextMenu()
{
    IF_ASSERT_FAILED(m_presetsContextMenu) {
        return nullptr;
    }

    m_presetsContextMenu->setInstanceId_prop(m_instanceId);
    m_presetsContextMenu->load();
    return m_presetsContextMenu;
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
    if (!m_presetsContextMenu) {
        return true;
    }

    return m_presetsContextMenu->useVendorUI();
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
