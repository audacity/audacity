/*
* Audacity: A Digital Audio Editor
*/
#include "effectpresetsbarmodel.h"

#include "effects/effects_base/effectstypes.h"
#include "au3-components/EffectInterface.h"
#include "au3-effects/Effect.h"
#include "au3wrap/internal/wxtypes_convert.h"

using namespace muse;
using namespace muse::actions;
using namespace au::effects;

namespace {
constexpr int USER_PRESET_ICON_CODE = 0xEF99;
}

EffectPresetsBarModel::EffectPresetsBarModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
    m_saveContextMenu = new EffectSaveContextMenu(this);
    m_presetsContextMenu = new PresetsContextMenuModel(this);

    connect(m_presetsContextMenu, &PresetsContextMenuModel::useVendorUIChanged,
            this, &EffectPresetsBarModel::useVendorUIChanged);
}

void EffectPresetsBarModel::load()
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

        m_currentPreset = presetId;
        m_isPresetUnsaved = false;
        persistPresetState();
        updatePresetDisplayNames();
        updatePresetBar();
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

    if (restorePresetState()) {
        captureInitialPresetState();
        return;
    }

    // NOTE: No restorable preset state was found for this dialog instance,
    // so fall back to destructive last-used preset restore or realtime preset matching.
    if (m_usedDestructively) {
        restoreLastUsedPreset(effectId);
    } else if (m_currentPreset.isEmpty()) {
        restoreMatchedPresetForCurrentSettings();
    }

    captureInitialPresetState();
}

void EffectPresetsBarModel::reload(const EffectId& effectId, const EffectInstanceId& instanceId)
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

    m_allPresets = presets;

    if (!m_currentPreset.isEmpty() && !hasPreset(m_currentPreset)) {
        m_currentPreset.clear();
        m_isPresetUnsaved = false;
        persistPresetState();
        emit presetChanged();
    } else if (!m_currentPreset.isEmpty()) {
        m_isPresetUnsaved = isCurrentPresetUnsaved();
        persistPresetState();
    } else {
        m_isPresetUnsaved = false;
    }

    updatePresetDisplayNames();
    emit canDeletePresetChanged();
    emit canResetPresetChanged();
}

bool EffectPresetsBarModel::restorePresetState()
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);
    const PresetKey key { effectId, m_realtimeEffectState.toStdString() };
    const std::optional<IPresetStatesRegister::PresetState> presetState = presetStatesRegister()->presetState(key);
    if (!presetState.has_value()) {
        return false;
    }

    const QString presetId = QString::fromStdString(presetState->presetId);
    if (!hasPreset(presetId)) {
        return false;
    }

    m_currentPreset = presetId;
    m_isPresetUnsaved = presetState->unsaved;
    updatePresetDisplayNames();
    updatePresetBar();
    return true;
}

void EffectPresetsBarModel::captureInitialPresetState()
{
    if (!m_currentPreset.isEmpty() && hasPreset(m_currentPreset)) {
        m_initialPreset = m_currentPreset;
        m_initialPresetUnsaved = m_isPresetUnsaved;
        return;
    }

    m_initialPreset.reset();
    m_initialPresetUnsaved = false;
}

void EffectPresetsBarModel::restoreLastUsedPreset(const EffectId& effectId)
{
    const QString storedPresetId = QString::fromStdString(configuration()->lastUsedPreset(effectId));
    if (storedPresetId.isEmpty() || !hasPreset(storedPresetId)) {
        return;
    }

    m_currentPreset = storedPresetId;
    resetPreset();
    m_isPresetUnsaved = false;
    persistPresetState();
    updatePresetDisplayNames();
    updatePresetBar();
}

void EffectPresetsBarModel::restoreInitialPresetState()
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);
    const PresetKey key { effectId, m_realtimeEffectState.toStdString() };
    if (effectId.empty()) {
        return;
    }

    if (!m_initialPreset.has_value() || !hasPreset(*m_initialPreset)) {
        presetStatesRegister()->removePresetState(key);
        return;
    }

    presetStatesRegister()->setPresetState(key, { m_initialPreset->toStdString(), m_initialPresetUnsaved });
}

void EffectPresetsBarModel::restoreMatchedPresetForCurrentSettings()
{
    m_currentPreset = matchPresetForCurrentSettings();
    m_isPresetUnsaved = false;
    persistPresetState();
    updatePresetDisplayNames();
    updatePresetBar();
}

int EffectPresetsBarModel::instanceId_prop() const
{
    return m_instanceId;
}

void EffectPresetsBarModel::setInstanceId_prop(int newInstanceId)
{
    if (m_instanceId == newInstanceId) {
        return;
    }

    m_instanceId = newInstanceId;
    m_currentPreset.clear();
    m_isPresetUnsaved = false;
    emit instanceIdChanged();
    emit canResetPresetChanged();
}

QString EffectPresetsBarModel::realtimeEffectState() const
{
    return m_realtimeEffectState;
}

void EffectPresetsBarModel::setRealtimeEffectState(const QString& newRealtimeEffectState)
{
    if (m_realtimeEffectState == newRealtimeEffectState) {
        return;
    }

    m_realtimeEffectState = newRealtimeEffectState;
    emit realtimeEffectStateChanged();
}

QVariantList EffectPresetsBarModel::presets()
{
    return m_allPresets;
}

QString EffectPresetsBarModel::preset() const
{
    return m_currentPreset;
}

void EffectPresetsBarModel::setPreset(QString presetId)
{
    m_currentPreset = presetId;
    resetPreset();
    m_isPresetUnsaved = false;
    persistPresetState();
    updatePresetDisplayNames();
    updatePresetBar();
}

bool EffectPresetsBarModel::presetsDropdownEnabled() const
{
    return m_allPresets.size() > 1; // Do not take default preset into account
}

bool EffectPresetsBarModel::canDeletePreset() const
{
    return m_userPresets.contains(m_currentPreset);
}

bool EffectPresetsBarModel::canResetPreset() const
{
    return !m_currentPreset.isEmpty() && m_isPresetUnsaved;
}

void EffectPresetsBarModel::resetPreset()
{
    if (m_currentPreset.isEmpty()) {
        return;
    }

    ActionQuery q("action://effects/presets/apply");
    q.addParam("instanceId", Val(m_instanceId));
    q.addParam("presetId", Val(m_currentPreset.toStdString()));
    dispatcher()->dispatch(q);
}

void EffectPresetsBarModel::savePresetAs()
{
    ActionQuery q("action://effects/presets/save_as");
    q.addParam("instanceId", Val(m_instanceId));
    dispatcher()->dispatch(q);
}

muse::uicomponents::AbstractMenuModel* EffectPresetsBarModel::saveContextMenu()
{
    IF_ASSERT_FAILED(m_saveContextMenu) {
        return nullptr;
    }

    m_saveContextMenu->setInstanceId_prop(m_instanceId);
    m_saveContextMenu->setPreset(m_currentPreset);
    m_saveContextMenu->load();
    return m_saveContextMenu;
}

muse::uicomponents::AbstractMenuModel* EffectPresetsBarModel::presetContextMenu()
{
    IF_ASSERT_FAILED(m_presetsContextMenu) {
        return nullptr;
    }

    m_presetsContextMenu->setInstanceId_prop(m_instanceId);
    m_presetsContextMenu->load();
    return m_presetsContextMenu;
}

void EffectPresetsBarModel::deletePreset()
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

void EffectPresetsBarModel::commitSelectedPreset()
{
    if (!m_usedDestructively) {
        return;
    }

    const EffectId effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);
    if (effectId.empty() || m_currentPreset.isEmpty() || !hasPreset(m_currentPreset)) {
        return;
    }

    configuration()->setLastUsedPreset(effectId, m_currentPreset.toStdString());
    persistPresetState();
}

bool EffectPresetsBarModel::useVendorUI() const
{
    if (!m_presetsContextMenu) {
        return true;
    }

    return m_presetsContextMenu->useVendorUI();
}

bool EffectPresetsBarModel::persistLastUsedPreset() const
{
    return m_usedDestructively;
}

void EffectPresetsBarModel::setPersistLastUsedPreset(bool value)
{
    if (m_usedDestructively == value) {
        return;
    }

    m_usedDestructively = value;
    emit persistLastUsedPresetChanged();
}

bool EffectPresetsBarModel::hasPreset(const QString& presetId) const
{
    if (presetId.isEmpty()) {
        return false;
    }

    const auto it = std::find_if(m_allPresets.cbegin(), m_allPresets.cend(), [&](const QVariant& v) {
        return v.toMap().value("id").toString() == presetId;
    });

    return it != m_allPresets.cend();
}

bool EffectPresetsBarModel::isUserPreset(const QString& presetId) const
{
    return m_userPresets.contains(presetId);
}

bool EffectPresetsBarModel::isFactoryPreset(const QString& presetId) const
{
    return m_factoryPresets.contains(presetId);
}

int EffectPresetsBarModel::factoryPresetIndex(const QString& presetId) const
{
    return m_factoryPresets.indexOf(presetId);
}

QString EffectPresetsBarModel::matchPresetForCurrentSettings() const
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);
    Effect* effect = effectsProvider()->effect(effectId);
    const EffectSettings* settings = instancesRegister()->settingsById(m_instanceId);
    if (!effect || !settings) {
        return {};
    }

    wxString currentSettingsString;
    if (!effect->SaveSettingsAsString(*settings, currentSettingsString)) {
        return {};
    }

    auto& definition = effect->GetDefinition();
    auto matchesPreset = [&](const QString& presetId) {
        EffectSettings presetSettings = definition.MakeSettings();
        if (!definition.CopySettingsContents(*settings, presetSettings)) {
            return false;
        }

        OptionalMessage loadResult;
        if (presetId == "default") {
            loadResult = definition.LoadFactoryDefaults(presetSettings);
        } else if (isUserPreset(presetId)) {
            const auto name = au3::wxFromString(String::fromQString(presetId));
            loadResult = definition.LoadUserPreset(UserPresetsGroup(name), presetSettings);
        } else if (isFactoryPreset(presetId)) {
            const int index = factoryPresetIndex(presetId);
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

        return currentSettingsString == selectedPresetSettingsString;
    };

    if (matchesPreset("default")) {
        return "default";
    }

    for (const QString& presetId : m_userPresets) {
        if (matchesPreset(presetId)) {
            return presetId;
        }
    }

    for (const QString& presetId : m_factoryPresets) {
        if (matchesPreset(presetId)) {
            return presetId;
        }
    }

    return {};
}

void EffectPresetsBarModel::persistPresetState()
{
    const EffectId effectId = instancesRegister()->effectIdByInstanceId(m_instanceId);
    if (effectId.empty()) {
        return;
    }

    const PresetKey key { effectId, m_realtimeEffectState.toStdString() };

    if (m_currentPreset.isEmpty() || !hasPreset(m_currentPreset)) {
        presetStatesRegister()->removePresetState(key);
        return;
    }

    presetStatesRegister()->setPresetState(key, { m_currentPreset.toStdString(), m_isPresetUnsaved });
}

bool EffectPresetsBarModel::isCurrentPresetUnsaved() const
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

    const auto& definition = effect->GetDefinition();
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

void EffectPresetsBarModel::setPresetUnsaved(bool unsaved)
{
    if (m_isPresetUnsaved == unsaved) {
        return;
    }

    m_isPresetUnsaved = unsaved;
    persistPresetState();
    updatePresetDisplayNames();
    emit canResetPresetChanged();
}

void EffectPresetsBarModel::updatePresetDisplayNames()
{
    for (int i = 0; i < m_allPresets.size(); ++i) {
        QVariantMap map = m_allPresets[i].toMap();
        const QString id = map.value("id").toString();
        const QString baseName = m_basePresetNames.value(id, map.value("name").toString());
        QString displayName = baseName;
        if (isUserPreset(id)) {
            displayName += QString(" (%1)").arg(muse::qtrc("effects", "custom"));
        }
        if (m_isPresetUnsaved && id == m_currentPreset) {
            displayName += "*";
        }
        map.insert("name", displayName);
        m_allPresets[i] = map;
    }

    emit presetsChanged();
}

void EffectPresetsBarModel::updatePresetBar()
{
    emit presetChanged();
    emit canDeletePresetChanged();
    emit canResetPresetChanged();
}
