/*
* Audacity: A Digital Audio Editor
*/
#pragma once

namespace au::projectscene {
class ITimelineViewController
{
public:
    virtual ~ITimelineViewController() = default;

    virtual void zoomIn() = 0;
    virtual void zoomOut() = 0;
    virtual void zoomDefault() = 0;
    virtual void zoomToggle() = 0;

    virtual void fitSelectionToWidth() = 0;
    virtual void fitProjectToWidth() = 0;

    virtual void centerViewOnPlayhead(bool onlyIfPlayheadNotVisible) = 0;
};
}
