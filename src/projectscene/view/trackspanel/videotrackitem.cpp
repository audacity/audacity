/*
* Audacity: A Digital Audio Editor
*/

#include "videotrackitem.h"

using namespace au::projectscene;

VideoTrackItem::VideoTrackItem(QObject* parent)
    : TrackItem(parent)
{
}

bool VideoTrackItem::isAudible() const
{
    return false;
}
