/*
* Audacity: A Digital Audio Editor
*/
#include "cloudprojectsyncmodel.h"

using namespace au::project;

CloudProjectSyncModel::CloudProjectSyncModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void CloudProjectSyncModel::load()
{
    emit syncingChanged();

    audioComService()->syncingInProgressChanged().ch.onReceive(this, [this](bool) {
        emit syncingChanged();
    });
}

bool CloudProjectSyncModel::syncing() const
{
    return audioComService()->syncingInProgressChanged().val;
}
