/*
* Audacity: A Digital Audio Editor
*/

#include "addnewtrackpopupmodel.h"

using namespace au::projectscene;

AddNewTrackPopupModel::AddNewTrackPopupModel(QObject* parent)
    : QObject(parent)
{
}

bool AddNewTrackPopupModel::isAddLabelAvailable() const
{
    return globalConfiguration()->devModeEnabled();
}
