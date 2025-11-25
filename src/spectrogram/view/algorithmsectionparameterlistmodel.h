/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "abstractsectionparameterslistmodel.h"
#include "abstractspectrogramsettingsmodel.h"

namespace au::spectrogram {
class AlgorithmSectionParameterListModel : public AbstractSectionParametersListModel
{
public:
    explicit AlgorithmSectionParameterListModel(QObject* parent = nullptr);
    ~AlgorithmSectionParameterListModel() override = default;

private:
    enum Control {
        Algorithm = 0,
        WindowSize,
        WindowType,
        ZeroPaddingFactor,
        _count
    };

    int rowCount(const QModelIndex&) const override { return static_cast<int>(Control::_count); }
    QVariant data(const QModelIndex& index, int role) const override;
    QHash<int, QByteArray> roleNames() const override;
    bool setData(const QModelIndex& index, const QVariant& value, int role) override;

    void onSettingsModelSet(AbstractSpectrogramSettingsModel& model) override;

    enum Roles {
        ControlLabelRole = Qt::UserRole + 1,
        ControlWidthRole,
        ControlPossibleValuesRole,
        ControlCurrentIndexRole,
        ControlCurrentValueRole,
    };

    QString controlLabel(Control) const;
    int controlWidth(Control) const;
    QVariantList controlPossibleValues(Control) const;
    int controlCurrentIndex(Control) const;
    int controlCurrentValue(Control) const;
};
}
