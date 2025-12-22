/*
 * Audacity: A Digital Audio Editor
 */
#include "abstractsectionparameterslistmodel.h"

namespace au::spectrogram {
namespace {
constexpr int columnSpacing = 8;
}

AbstractSectionParametersListModel::AbstractSectionParametersListModel(QObject* parent)
    : QAbstractListModel(parent) {}

void AbstractSectionParametersListModel::setSettingsModel(AbstractSpectrogramSettingsModel* model)
{
    if (m_settingsModel == model) {
        return;
    }
    m_settingsModel = model;

    if (m_settingsModel) {
        onSettingsModelSet(*m_settingsModel);
    }

    emit settingsModelChanged();
}

void AbstractSectionParametersListModel::setColumnWidth(int width)
{
    if (m_columnWidth == width) {
        return;
    }
    m_columnWidth = width;

    emit columnWidthChanged();
}

int AbstractSectionParametersListModel::controlWidthS() const
{
    return m_columnWidth;
}

int AbstractSectionParametersListModel::controlWidthM() const
{
    return 2 * m_columnWidth + columnSpacing;
}

int AbstractSectionParametersListModel::controlWidthL() const
{
    return 3 * m_columnWidth + 2 * columnSpacing;
}
}
