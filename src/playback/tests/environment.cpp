/*
* Audacity: A Digital Audio Editor
*/

#include "testing/environment.h"

#include "au3wrap/au3wrapmodule.h"

#include "project/tests/mocks/projectconfigurationmock.h"

using namespace ::testing;
using namespace au::project;

static muse::testing::SuiteEnvironment playback_se
    = muse::testing::SuiteEnvironment()
      .setDependencyModules({ new au::au3::Au3WrapModule() })
      .setPreInit([](){
    std::shared_ptr<ProjectConfigurationMock> projectConfigurator(new ProjectConfigurationMock(), [](ProjectConfigurationMock*){});

    ON_CALL(*projectConfigurator, temporaryDir())
    .WillByDefault(Return(""));

    muse::modularity::globalIoc()->registerExport<IProjectConfiguration>("utests", projectConfigurator);
}).setPostInit([]() {
}).setDeInit([](){
    std::shared_ptr<IProjectConfiguration> projectConfiguratorPtr
        = muse::modularity::globalIoc()->resolve<IProjectConfiguration>("utests");
    muse::modularity::globalIoc()->unregister<IProjectConfiguration>("utests");

    IProjectConfiguration* projectConfigurator = projectConfiguratorPtr.get();
    delete projectConfigurator;
});
