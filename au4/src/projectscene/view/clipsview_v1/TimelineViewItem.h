#pragma once

#include <memory>

class TimelineViewUIHandle;
class QRect;
class QPoint;
class QPainter;
class QQmlEngine;

class TimelineContext;

class TimelineViewItem
{
public:

    virtual ~TimelineViewItem();

    virtual void Paint(QQmlEngine& engine, QPainter& painter, const QRect& viewRect, const TimelineContext& trackPanel);

    virtual std::unique_ptr<TimelineViewUIHandle> HitTest(const QPoint& at) const;
};
