/*
* Audacity: A Digital Audio Editor
*/

#include "leadinrecordingindicatormodel.h"

using namespace au::record;

LeadInRecordingIndicatorModel::LeadInRecordingIndicatorModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void LeadInRecordingIndicatorModel::init()
{
    recordController()->isLeadInRecordingChanged().onNotify(this, [this]() {
        emit visibleChanged();
    });
}

bool LeadInRecordingIndicatorModel::visible() const
{
    return recordController()->isLeadInRecording();
}

double LeadInRecordingIndicatorModel::startTime() const
{
    return recordController()->leadInRecordingStartTime();
}

QVariantList LeadInRecordingIndicatorModel::trackIds() const
{
    QVariantList list;
    for (const auto& id : recordController()->leadInRecordingTrackIds()) {
        list.append(QVariant::fromValue(id));
    }
    return list;
}
