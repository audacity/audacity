#pragma once

#include "TimelineViewUIHandle.h"

class TimeSelectionHandle : public TimelineViewUIHandle
{
public:
    ~TimeSelectionHandle() override;

    void OnMouseEnter(TimelineView&) override;
    void OnMouseMove(TimelineView&) override;
    void OnMousePress(TimelineView&) override;
    void OnMouseRelease(TimelineView&) override;
};
