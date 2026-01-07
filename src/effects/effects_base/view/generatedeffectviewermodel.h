/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ieffectinstancesregister.h"
#include "ieffectparametersprovider.h"
#include "ieffectsprovider.h"
#include "effectstypes.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include <QAbstractListModel>
#include <QHash>

namespace au::effects {
class GeneratedEffectViewerModel : public QAbstractListModel, public muse::Injectable, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(int instanceId READ instanceId WRITE setInstanceId NOTIFY instanceIdChanged FINAL)
    Q_PROPERTY(QString effectName READ effectName NOTIFY effectNameChanged FINAL)
    Q_PROPERTY(QString title READ title NOTIFY titleChanged FINAL)
    Q_PROPERTY(QString noParametersMessage READ noParametersMessage NOTIFY noParametersMessageChanged FINAL)
    Q_PROPERTY(bool hasParameters READ hasParameters NOTIFY hasParametersChanged FINAL)

    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectParametersProvider> parametersProvider;
    muse::Inject<IEffectsProvider> effectsProvider;

public:
    // Roles for QML access
    enum Roles {
        IdRole = Qt::UserRole + 1,
        NameRole,
        UnitsRole,
        TypeRole,
        MinValueRole,
        MaxValueRole,
        DefaultValueRole,
        CurrentValueRole,
        StepSizeRole,
        StepCountRole,
        CurrentValueStringRole,
        EnumValuesRole,
        EnumIndicesRole,
        IsReadOnlyRole,
        IsHiddenRole,
        IsLogarithmicRole,
        IsIntegerRole,
        CanAutomateRole,
    };
    Q_ENUM(Roles)

    explicit GeneratedEffectViewerModel(QObject* parent = nullptr);
    ~GeneratedEffectViewerModel() override = default;

    // QAbstractListModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    bool setData(const QModelIndex& index, const QVariant& value, int role = Qt::EditRole) override;
    QHash<int, QByteArray> roleNames() const override;

    Q_INVOKABLE void load();
    Q_INVOKABLE void setParameterValue(int index, double plainValue);
    Q_INVOKABLE QString getParameterValueString(int index, double normalizedValue) const;

    int instanceId() const;
    void setInstanceId(int newInstanceId);

    QString effectName() const;
    QString title() const;
    QString noParametersMessage() const;
    bool hasParameters() const;

signals:
    void instanceIdChanged();
    void effectNameChanged();
    void titleChanged();
    void noParametersMessageChanged();
    void hasParametersChanged();

private:
    void reloadParameters();
    void updateEffectName();
    int findParameterIndex(const muse::String& parameterId) const;

    int m_instanceId = -1;
    QString m_effectName;
    ParameterInfoList m_parameters;
};
}
