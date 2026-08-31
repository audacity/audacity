/*
* Audacity: A Digital Audio Editor
*/

#include "testing/environment.h"

#include "au3wrap/au3wrapmodule.h"

//! Au3WrapModule provides the au3 settings bridge (gPrefs) and the project
//! implementation needed by Au3ProjectAccessor.
static muse::testing::SuiteEnvironment au3audio_se
    = muse::testing::SuiteEnvironment()
      .setDependencyModules({ new au::au3::Au3WrapModule(), });
