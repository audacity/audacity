/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QAbstractListModel>

namespace au::spectrogram {
class AbstractSpectrogramSettingsModel;

class ColorSectionParameterListModel : public QAbstractListModel
{
    Q_OBJECT

    Q_PROPERTY(AbstractSpectrogramSettingsModel * settingsModel READ settingsModel WRITE setSettingsModel NOTIFY settingsModelChanged)

public:
    explicit ColorSectionParameterListModel(QObject* parent = nullptr);
    ~ColorSectionParameterListModel() override = default;

    AbstractSpectrogramSettingsModel* settingsModel() const { return m_settingsModel; }
    void setSettingsModel(AbstractSpectrogramSettingsModel* model);

signals:
    void settingsModelChanged();

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

    enum Roles {
        ControlLabelRole = Qt::UserRole + 1,
        ControlUnitsRole,
        ControlMinValueRole,
        ControlMaxValueRole,
        ControlCurrentValueRole,
    };

    QString controlLabel(Control) const;
    QString controlUnits(Control) const;
    int controlMinValue(Control) const;
    int controlMaxValue(Control) const;
    int controlCurrentValue(Control) const;

    AbstractSpectrogramSettingsModel* m_settingsModel = nullptr;
};
}
