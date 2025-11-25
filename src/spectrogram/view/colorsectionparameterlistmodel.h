/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "abstractsectionparameterslistmodel.h"

namespace au::spectrogram {
class AbstractSpectrogramSettingsModel;

class ColorSectionParameterListModel : public AbstractSectionParametersListModel
{
public:
    explicit ColorSectionParameterListModel(QObject* parent = nullptr);
    ~ColorSectionParameterListModel() override = default;

private:
    enum Control {
        ColorGain = 0,
        ColorRange,
        ColorHighBoost,
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
        ControlWidthRole,
        ControlMinValueRole,
        ControlMaxValueRole,
        ControlCurrentValueRole,
    };

    QString controlLabel(Control) const;
    QString controlUnits(Control) const;
    int controlWidth(Control) const;
    int controlMinValue(Control) const;
    int controlMaxValue(Control) const;
    int controlCurrentValue(Control) const;
};
}
