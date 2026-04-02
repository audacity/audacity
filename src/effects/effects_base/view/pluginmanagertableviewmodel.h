/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ieffectsprovider.h"

#include "framework/audioplugins/iregisteraudiopluginsscenario.h"
#include "framework/global/modularity/ioc.h"
#include "framework/uicomponents/qml/Muse/UiComponents/abstracttableviewmodel.h"

#include <QQmlParserStatus>
#include <QObject>

#include <functional>
#include <unordered_set>

namespace au::effects {
namespace PluginManagerTableViewCellType {
Q_NAMESPACE;
QML_ELEMENT;

enum class Type {
    Enabled = static_cast<int>(muse::uicomponents::TableViewCellType::Type::UserType) + 1,
};

Q_ENUM_NS(Type)
}

class PluginManagerTableViewModel : public muse::uicomponents::AbstractTableViewModel, public QQmlParserStatus, muse::Contextable
{
    Q_OBJECT
    Q_INTERFACES(QQmlParserStatus)

    Q_PROPERTY(
        muse::uicomponents::MenuItemList enabledDisabledOptions READ enabledDisabledOptions NOTIFY enabledDisabledSelectedIndexChanged)
    Q_PROPERTY(
        int enabledDisabledSelectedIndex READ enabledDisabledSelectedIndex WRITE setEnabledDisabledSelectedIndex NOTIFY enabledDisabledSelectedIndexChanged)

    Q_PROPERTY(muse::uicomponents::MenuItemList effectFamilyOptions READ effectFamilyOptions NOTIFY effectFamilySelectedIndexChanged)
    Q_PROPERTY(
        int effectFamilySelectedIndex READ effectFamilySelectedIndex WRITE setEffectFamilySelectedIndex NOTIFY effectFamilySelectedIndexChanged)

    Q_PROPERTY(muse::uicomponents::MenuItemList effectTypeOptions READ effectTypeOptions NOTIFY effectTypeSelectedIndexChanged)
    Q_PROPERTY(
        int effectTypeSelectedIndex READ effectTypeSelectedIndex WRITE setEffectTypeSelectedIndex NOTIFY effectTypeSelectedIndexChanged)

    muse::GlobalInject<IEffectsProvider> effectsProvider;
    muse::ContextInject<muse::audioplugins::IRegisterAudioPluginsScenario> registerAudioPluginsScenario { this };

public:
    explicit PluginManagerTableViewModel(QObject* parent = nullptr);
    ~PluginManagerTableViewModel() override;

    muse::uicomponents::MenuItemList enabledDisabledOptions() const;
    int enabledDisabledSelectedIndex() const { return m_enabledDisabledSelectedIndex; }
    void setEnabledDisabledSelectedIndex(int index);

    muse::uicomponents::MenuItemList effectFamilyOptions() const;
    int effectFamilySelectedIndex() const { return m_effectFamilySelectedIndex; }
    void setEffectFamilySelectedIndex(int index);

    muse::uicomponents::MenuItemList effectTypeOptions() const;
    int effectTypeSelectedIndex() const { return m_effectTypeSelectedIndex; }
    void setEffectTypeSelectedIndex(int index);

    Q_INVOKABLE void handleEdit(int row, int column);
    Q_INVOKABLE void setSearchText(const QString& searchText);
    Q_INVOKABLE void rescanPlugins();
    Q_INVOKABLE void cancel();

signals:
    void enabledDisabledSelectedIndexChanged();
    void effectFamilySelectedIndexChanged();
    void effectTypeSelectedIndexChanged();

private:
    void classBegin() override {}
    void componentComplete() override;

    QVector<muse::uicomponents::TableViewHeader*> makeHorizontalHeaders();
    QVector<muse::uicomponents::TableViewHeader*> makeVerticalHeaders(const EffectMetaList& effects);
    QVector<QVector<muse::uicomponents::TableViewCell*> > makeTable(const EffectMetaList& effects);
    void setTableRows(EffectMetaList effects);

    EffectMetaList m_initialState;
    std::unordered_set<EffectId> m_editedEffects;
    bool m_resetOnClose = false;

    using EffectFilter = std::function<bool (const EffectMeta&)>;
    static const EffectFilter allPassFilter;

    int m_enabledDisabledSelectedIndex = 0;
    EffectFilter m_enabledDisabledEffectFilter = allPassFilter;

    int m_effectFamilySelectedIndex = 0;
    EffectFilter m_effectFamilyFilter = allPassFilter;

    int m_effectTypeSelectedIndex = 0;
    EffectFilter m_effectTypeFilter = allPassFilter;
};
}
