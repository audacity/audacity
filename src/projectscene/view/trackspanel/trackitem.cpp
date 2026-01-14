/*
* Audacity: A Digital Audio Editor
*/

#include "trackitem.h"

#include <QMetaType>
#include <QString>

using namespace au::projectscene;
using namespace au::trackedit;

static const std::string TRACK_ID_KEY("trackId");
static const std::string RESOURCE_ID_KEY("resourceId");
static const std::string CHAIN_ORDER_KEY("chainOrder");

static muse::ui::IconCode::Code iconFromTrackType(au::trackedit::TrackType type)
{
    switch (type) {
    case au::trackedit::TrackType::Label:
        return muse::ui::IconCode::Code::LOOP_IN;
    default:
        return muse::ui::IconCode::Code::MICROPHONE;
    }
}

TrackItem::TrackItem(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
    qRegisterMetaType<au::trackedit::TrackType>("au::trackedit::TrackType");
}

TrackItem::~TrackItem()
{
}

void TrackItem::init(const trackedit::Track& track)
{
    m_trackId = track.id;

    if (m_trackType != track.type) {
        m_trackType = track.type;
        emit trackTypeChanged();
    }

    if (m_title != track.title) {
        m_title = track.title;
        emit titleChanged(m_title);
    }

    m_icon = iconFromTrackType(track.type);

    m_isFocused = selectionController()->focusedTrack() == m_trackId;
    emit isFocusedChanged();
}

au::trackedit::TrackId TrackItem::trackId() const
{
    return m_trackId;
}

QVariant TrackItem::trackId_property() const
{
    return QVariant::fromValue(m_trackId);
}

QString TrackItem::title() const
{
    return m_title;
}

int TrackItem::icon() const
{
    return static_cast<int>(m_icon);
}

au::trackedit::TrackType TrackItem::trackType() const
{
    return m_trackType;
}

bool TrackItem::isSelected() const
{
    return m_isSelected;
}

void TrackItem::setIsSelected(bool selected)
{
    if (m_isSelected == selected) {
        return;
    }

    m_isSelected = selected;
    emit isSelectedChanged();
}

bool TrackItem::isFocused() const
{
    return m_isFocused;
}

void TrackItem::setIsFocused(bool focused)
{
    if (m_isFocused == focused) {
        return;
    }

    m_isFocused = focused;
    emit isFocusedChanged();
}

void TrackItem::setTitle(QString title)
{
    if (m_title == title) {
        return;
    }

    m_title = title;
    trackeditInteraction()->changeTrackTitle(m_trackId, title);
    emit titleChanged(m_title);
}
