#pragma once

#include "trackedit/itrackeditconfiguration.h"
#include "modularity/ioc.h"

#include <QObject>

namespace au::appshell {
class EditPreferencesModel : public QObject
{
    Q_OBJECT
    Q_PROPERTY(
        bool askBeforeConvertingToMonoOrStereo READ askBeforeConvertingToMonoOrStereo WRITE setAskBeforeConvertingToMonoOrStereo NOTIFY askBeforeConvertingToMonoOrStereoChanged)

    muse::Inject<au::trackedit::ITrackeditConfiguration> trackeditConfiguration;

public:
    explicit EditPreferencesModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    bool askBeforeConvertingToMonoOrStereo() const;
    void setAskBeforeConvertingToMonoOrStereo(bool value);

signals:
    void askBeforeConvertingToMonoOrStereoChanged();
};
}
