/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "abstractsectionparameterslistmodel.h"

namespace au::spectrogram {
class AbstractSpectrogramSettingsModel;

class ScaleSectionParameterListModel : public AbstractSectionParametersListModel
{
public:
    explicit ScaleSectionParameterListModel(QObject* parent = nullptr);
    ~ScaleSectionParameterListModel() override = default;

private:
    enum Control {
        MaxFreq = 0, // Max freq first to be consistent with spectrogram UI
        MinFreq,
        _count
    };

    int rowCount(const QModelIndex&) const override { return static_cast<int>(Control::_count); }
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;
    bool setData(const QModelIndex& index, const QVariant& value, int role) override;

    void onSettingsModelSet(AbstractSpectrogramSettingsModel& model) override;

    enum Roles {
        ControlLabelRole = Qt::UserRole + 1,
        ControlUnitsRole,
        ControlMinValueRole,
        ControlMaxValueRole,
        ControlCurrentValueRole,
    };

    QString controlLabel(Control) const;
    int controlMinValue(Control) const;
    int controlMaxValue(Control) const;
    int controlCurrentValue(Control) const;
};
}
