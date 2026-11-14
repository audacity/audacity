/*
* Audacity: A Digital Audio Editor
*/

#include "testing/environment.h"

#include "au3wrap/au3wrapmodule.h"

#include "project/tests/mocks/projectconfigurationmock.h"

static muse::testing::SuiteEnvironment effects_base_se
    = muse::testing::SuiteEnvironment()
      .setDependencyModules({ new au::au3::Au3WrapModule() })
      .setPreInit([]() {
    // Au3WrapModule::onInit needs a project configuration (temporary dir).
    std::shared_ptr<au::project::ProjectConfigurationMock> projectConfigurator(
        new au::project::ProjectConfigurationMock(), [](au::project::ProjectConfigurationMock*) {});

    ON_CALL(*projectConfigurator, temporaryDir())
    .WillByDefault(::testing::Return(""));

    // Intentionally immortal (owned by the global IoC with a no-op deleter).
    ::testing::Mock::AllowLeak(projectConfigurator.get());

    muse::modularity::globalIoc()->registerExport<au::project::IProjectConfiguration>("utests", projectConfigurator);
});
