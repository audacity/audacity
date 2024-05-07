#pragma once

class TimelineView;
class TimelineContext;

class TimelineViewUIHandle
{
public:
    virtual ~TimelineViewUIHandle();

    virtual void OnMouseEnter(TimelineView&);

    virtual void OnMouseMove(TimelineView&);
    virtual void OnMousePress(TimelineView&);
    virtual void OnMouseRelease(TimelineView&);
};
