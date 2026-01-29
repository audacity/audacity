/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "iexportconfiguration.h"

namespace au::importexport {
class CustomMappingModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    muse::GlobalInject<IExportConfiguration> exportConfiguration;

    Q_PROPERTY(int exportChannels READ exportChannels WRITE setExportChannels NOTIFY exportChannelsChanged)

public:
    explicit CustomMappingModel(QObject* parent = nullptr);

    Q_INVOKABLE void apply();

    int exportChannels() const;
    void setExportChannels(int exportChannels);

signals:
    void exportChannelsChanged();

private:
    int m_exportChannels = 1;
};
}
