/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QAbstractListModel>

namespace au::spectrogram {
class AbstractSpectrogramSettingsModel;

class AlgorithmSectionParameterListModel : public QAbstractListModel
{
    Q_OBJECT

    Q_PROPERTY(AbstractSpectrogramSettingsModel * settingsModel READ settingsModel WRITE setSettingsModel NOTIFY settingsModelChanged)

public:
    explicit AlgorithmSectionParameterListModel(QObject* parent = nullptr);
    ~AlgorithmSectionParameterListModel() override = default;

    AbstractSpectrogramSettingsModel* settingsModel() const { return m_settingsModel; }
    void setSettingsModel(AbstractSpectrogramSettingsModel* model);

signals:
    void settingsModelChanged();

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

    AbstractSpectrogramSettingsModel* m_settingsModel = nullptr;
};
}
