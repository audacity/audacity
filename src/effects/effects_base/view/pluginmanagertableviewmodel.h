/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ieffectsprovider.h"

#include "framework/audioplugins/iregisteraudiopluginsscenario.h"
#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"
#include "framework/uicomponents/qml/Muse/UiComponents/abstracttableviewmodel.h"

#include <QQmlParserStatus>
#include <QObject>

#include <functional>
#include <unordered_set>

namespace au::uicomponents {
class TableSortFilterProxyModel;
}

namespace au::effects {
class PluginManagerSortFilterProxy;

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

    Q_PROPERTY(int totalWidth READ totalWidth CONSTANT)
    Q_PROPERTY(
        bool alsoRescanBrokenPlugins READ alsoRescanBrokenPlugins WRITE setAlsoRescanBrokenPlugins NOTIFY alsoRescanBrokenPluginsChanged)

    Q_PROPERTY(au::uicomponents::TableSortFilterProxyModel * sortFilterProxy READ sortFilterProxy CONSTANT)

    muse::GlobalInject<IEffectsProvider> effectsProvider;

    muse::ContextInject<muse::audioplugins::IRegisterAudioPluginsScenario> registerAudioPluginsScenario { this };
    muse::ContextInject<muse::IInteractive> interactive { this };

public:
    explicit PluginManagerTableViewModel(QObject* parent = nullptr);
    ~PluginManagerTableViewModel() override = default;

    muse::uicomponents::MenuItemList enabledDisabledOptions();
    int enabledDisabledSelectedIndex() const { return m_enabledDisabledSelectedIndex; }
    void setEnabledDisabledSelectedIndex(int index);

    muse::uicomponents::MenuItemList effectFamilyOptions();
    int effectFamilySelectedIndex() const { return m_effectFamilySelectedIndex; }
    void setEffectFamilySelectedIndex(int index);

    muse::uicomponents::MenuItemList effectTypeOptions();
    int effectTypeSelectedIndex() const { return m_effectTypeSelectedIndex; }
    void setEffectTypeSelectedIndex(int index);

    int totalWidth() const;

    bool alsoRescanBrokenPlugins() const { return m_alsoRescanBrokenPlugins; }
    void setAlsoRescanBrokenPlugins(bool alsoRescanBrokenPlugins);

    au::uicomponents::TableSortFilterProxyModel* sortFilterProxy() const;

    Q_INVOKABLE void handleEdit(int proxyRow, int column);
    Q_INVOKABLE void setSearchText(const QString& searchText);
    Q_INVOKABLE void toggleColumnSort(int column);
    Q_INVOKABLE void rescanPlugins();
    Q_INVOKABLE void aboutToDestroy();

    Q_INVOKABLE void accept();

    // Used by the sort/filter proxy; not intended for QML.
    const EffectMetaList& allEffects() const { return m_allEffects; }

signals:
    void enabledDisabledSelectedIndexChanged();
    void effectFamilySelectedIndexChanged();
    void effectTypeSelectedIndexChanged();
    void alsoRescanBrokenPluginsChanged();

private:
    friend class PluginManagerSortFilterProxy;

    static constexpr auto s_enabledDisabledColumnIndex = 0;
    static constexpr auto s_nameColumnIndex = 1;
    static constexpr auto s_vendorColumnIndex = 2;
    static constexpr auto s_pathColumnIndex = 3;
    static constexpr auto s_typeColumnIndex = 4;
    static constexpr auto s_columnCount = 5;

    void classBegin() override {}
    void componentComplete() override;

    QVector<muse::uicomponents::TableViewHeader*> makeHorizontalHeaders();
    QVector<muse::uicomponents::TableViewHeader*> makeVerticalHeaders(const EffectMetaList& effects);
    QVector<QVector<muse::uicomponents::TableViewCell*> > makeTable(const EffectMetaList& effects);
    void rebuildSourceTable(EffectMetaList effects);

    EffectMetaList m_allEffects;
    EffectMetaList m_initialState;
    std::unordered_set<EffectId> m_editedEffects;
    bool m_changesConfirmed = false;
    QString m_searchText;

    using EffectFilter = std::function<bool (const EffectMeta&)>;
    static const EffectFilter allPassFilter;

    int m_enabledDisabledSelectedIndex = 0;
    EffectFilter m_acceptEnabledDisabledState = allPassFilter;

    bool m_alsoRescanBrokenPlugins = false;

    int m_effectFamilySelectedIndex = 0;
    EffectFilter m_acceptFamily = allPassFilter;

    int m_effectTypeSelectedIndex = 0;
    EffectFilter m_acceptType = allPassFilter;

    PluginManagerSortFilterProxy* const m_sortFilterProxy;
};
}
