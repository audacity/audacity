/*
* Audacity: A Digital Audio Editor
*/

#include "labeltrackitem.h"

using namespace au::projectscene;
using namespace au::trackedit;

LabelTrackItem::LabelTrackItem(QObject* parent)
    : TrackItem(parent)
{
}

bool LabelTrackItem::isAudible() const
{
    return false;
}
