/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "global/modularity/imoduleinterface.h"

namespace au::trackedit {
class ITrackNavigationController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITrackNavigationController);
public:
    virtual ~ITrackNavigationController() = default;

    virtual void focusTrackByIndex(const muse::actions::ActionData& args) = 0;
    virtual void focusPrevTrack() = 0;
    virtual void focusNextTrack() = 0;
    virtual void navigateUp(const muse::actions::ActionData& args) = 0;
    virtual void navigateDown(const muse::actions::ActionData& args) = 0;
    virtual void trackRangeSelection() = 0;
    virtual void toggleSelectionOnFocusedTrack() = 0;
    virtual void multiSelectionUp() = 0;
    virtual void multiSelectionDown() = 0;
};
}
