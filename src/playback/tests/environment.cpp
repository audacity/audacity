/*
* Audacity: A Digital Audio Editor
*/

#include "testing/environment.h"

#include "au3wrap/au3wrapmodule.h"

static muse::testing::SuiteEnvironment playback_se
    = muse::testing::SuiteEnvironment()
      .setDependencyModules({ new au::au3::Au3WrapModule() })
      .setPostInit([]() {
});
