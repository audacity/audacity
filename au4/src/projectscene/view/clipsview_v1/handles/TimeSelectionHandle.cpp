#include "TimeSelectionHandle.h"

#include <QCursor>

#include "../TimelineView.h"

TimeSelectionHandle::~TimeSelectionHandle() = default;

void TimeSelectionHandle::OnMouseEnter(TimelineView& view)
{
   view.setCursor(Qt::IBeamCursor);
}

void TimeSelectionHandle::OnMouseMove(TimelineView& view)
{

}

void TimeSelectionHandle::OnMousePress(TimelineView& view)
{

}

void TimeSelectionHandle::OnMouseRelease(TimelineView& view)
{

}
