/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_VIDEOSTUBMODULE_H
#define AU_VIDEO_VIDEOSTUBMODULE_H

#include "modularity/imodulesetup.h"

namespace au::video {
//! Stands in when the video module is compiled out, so the application still
//! links and starts. Registers nothing; the video panel simply does not exist.
class VideoStubModule : public muse::modularity::IModuleSetup
{
public:
    std::string moduleName() const override;
};
}

#endif // AU_VIDEO_VIDEOSTUBMODULE_H
