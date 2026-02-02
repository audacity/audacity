/*
 * Audacity: A Digital Audio Editor
 */

#include "custommappingmodel.h"

using namespace au::importexport;

CustomMappingModel::CustomMappingModel(QObject* parent)
    : QObject(parent)
{
    setExportChannels(exportConfiguration()->exportChannels());
}

void CustomMappingModel::apply()
{
    exportConfiguration()->setExportChannels(m_exportChannels);
}

int CustomMappingModel::exportChannels() const
{
    return m_exportChannels;
}

void CustomMappingModel::setExportChannels(int exportChannels)
{
    if (exportChannels == m_exportChannels) {
        return;
    }

    m_exportChannels = exportChannels;
    emit exportChannelsChanged();
}
