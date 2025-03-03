/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

namespace au::trackedit {
class Au3ChangeDetection {
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3ChangeDetection() {
        
    }

    void notifyOfUndoRedo(TracksAndClips& before, TracksAndClips& after);
};
}
