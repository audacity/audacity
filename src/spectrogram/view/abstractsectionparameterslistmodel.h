/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "./abstractspectrogramsettingsmodel.h"

#include <QAbstractListModel>

namespace au::spectrogram {
class AbstractSectionParametersListModel : public QAbstractListModel
{
    Q_OBJECT

    Q_PROPERTY(AbstractSpectrogramSettingsModel * settingsModel READ settingsModel WRITE setSettingsModel NOTIFY settingsModelChanged)
    Q_PROPERTY(int columnWidth READ columnWidth WRITE setColumnWidth NOTIFY columnWidthChanged)

public:
    explicit AbstractSectionParametersListModel(QObject* parent = nullptr);
    ~AbstractSectionParametersListModel() override = default;

    AbstractSpectrogramSettingsModel* settingsModel() const { return m_settingsModel; }
    void setSettingsModel(AbstractSpectrogramSettingsModel* model);

    int columnWidth() const { return m_columnWidth; }
    void setColumnWidth(int width);

signals:
    void settingsModelChanged();
    void columnWidthChanged();

protected:
    AbstractSpectrogramSettingsModel* m_settingsModel = nullptr;
    virtual void onSettingsModelSet(AbstractSpectrogramSettingsModel& model) = 0;

    int controlWidthS() const;
    int controlWidthM() const;
    int controlWidthL() const;

    int m_columnWidth = 0;
};
}
