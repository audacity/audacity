#pragma once

#include <memory>

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
};
