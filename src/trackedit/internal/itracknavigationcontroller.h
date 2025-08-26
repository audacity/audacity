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

    virtual void navigateUp() = 0;
    virtual void navigateDown() = 0;
    virtual void toggleSelectionOnFocusedTrack() = 0;
};
}
