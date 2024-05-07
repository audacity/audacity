#pragma once

#include "../TimelineViewUIHandle.h"

class WaveClipShiftHandle : public TimelineViewUIHandle
{
public:
    ~WaveClipShiftHandle() override;

    void OnMouseEnter(TimelineView&) override;

    void OnMousePress(TimelineView&) override;
    void OnMouseMove(TimelineView&) override;
    void OnMouseRelease(TimelineView&) override;
};
