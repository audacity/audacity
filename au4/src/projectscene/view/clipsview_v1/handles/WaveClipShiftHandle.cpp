#include "WaveClipShiftHandle.h"

#include <QCursor>

#include "../TimelineView.h"

WaveClipShiftHandle::~WaveClipShiftHandle() = default;

void WaveClipShiftHandle::OnMouseEnter(TimelineView& view)
{
   view.setCursor(Qt::OpenHandCursor);
   view.setKeepMouseGrab(true);
   view.setAcceptedMouseButtons(Qt::LeftButton);
}

void WaveClipShiftHandle::OnMousePress(TimelineView& view)
{
   view.setCursor(Qt::ClosedHandCursor);
}

void WaveClipShiftHandle::OnMouseMove(TimelineView& view)
{

}

void WaveClipShiftHandle::OnMouseRelease(TimelineView& view)
{

}
