/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ieffectinstancesregister.h"
#include "ieffectparametersprovider.h"
#include "effectstypes.h"

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include <QAbstractListModel>
#include <QHash>

namespace au::effects {
//! Model that exposes effect parameters as a list for QML consumption.
//! This is a separate model to allow GeneratedEffectViewerModel to inherit
//! from AbstractEffectViewModel while still providing list-based parameter access.
class EffectParametersListModel : public QAbstractListModel, public muse::Injectable, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<IEffectInstancesRegister> instancesRegister;
    muse::Inject<IEffectParametersProvider> parametersProvider;

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

    explicit EffectParametersListModel(QObject* parent, EffectInstanceId instanceId);
    ~EffectParametersListModel() override = default;

    // QAbstractListModel interface
    int rowCount(const QModelIndex& parent = QModelIndex()) const override;
    QVariant data(const QModelIndex& index, int role = Qt::DisplayRole) const override;
    bool setData(const QModelIndex& index, const QVariant& value, int role = Qt::EditRole) override;
    QHash<int, QByteArray> roleNames() const override;

    Q_INVOKABLE void load();
    Q_INVOKABLE void setParameterValue(int index, double fullRangeValue);
    Q_INVOKABLE QString getParameterValueString(int index, double normalizedValue) const;

    bool hasParameters() const;

signals:
    void hasParametersChanged();

private:
    void reloadParameters();
    int findParameterIndex(const muse::String& parameterId) const;

    const EffectInstanceId m_instanceId;
    ParameterInfoList m_parameters;
};
}
