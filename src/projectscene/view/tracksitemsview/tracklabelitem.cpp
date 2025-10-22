/*
* Audacity: A Digital Audio Editor
*/
#include "tracklabelitem.h"

using namespace au::projectscene;

TrackLabelItem::TrackLabelItem(QObject* parent)
    : ViewTrackItem(parent)
{
}

void TrackLabelItem::setLabel(const trackedit::Label& label)
{
    m_key = TrackItemKey(label.key);
    m_title = label.title;
    m_color = label.color.toQColor();

    emit titleChanged();
    emit colorChanged();
    emit timeChanged();
}
