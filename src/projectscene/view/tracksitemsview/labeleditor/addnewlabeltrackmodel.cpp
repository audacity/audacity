/*
* Audacity: A Digital Audio Editor
*/
#include "addnewlabeltrackmodel.h"

using namespace au::projectscene;
using namespace au::trackedit;

AddNewLabelTrackModel::AddNewLabelTrackModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

au::trackedit::TrackId AddNewLabelTrackModel::createLabelTrack(const QString& trackName)
{
    muse::RetVal<TrackId> retVal = trackeditInteraction()->newLabelTrack(trackName);
    if (!retVal.ret) {
        return trackedit::INVALID_TRACK;
    }

    return retVal.val;
}
