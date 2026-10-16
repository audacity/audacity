/*
* Audacity: A Digital Audio Editor
*/

#include "testing/environment.h"

#include "au3wrap/au3wrapmodule.h"

#include "project/tests/mocks/projectconfigurationmock.h"

#include "RegisterExportPlugins.h"
#include "RegisterImportPlugins.h"

using namespace ::testing;

static muse::testing::SuiteEnvironment exporter_se
    = muse::testing::SuiteEnvironment()
      .setDependencyModules({ new au::au3::Au3WrapModule() })
      .setPreInit([](){
    std::shared_ptr<au::project::ProjectConfigurationMock> projectConfigurator(new au::project::ProjectConfigurationMock(),
                                                                               [](au::project::ProjectConfigurationMock*){});

    ON_CALL(*projectConfigurator, temporaryDir())
    .WillByDefault(Return(""));

    muse::modularity::globalIoc()->registerExport<au::project::IProjectConfiguration>("utests", projectConfigurator);

    //! NOTE: Au3WrapModule::onAllInited initializes the importer and exporter
    //! registries from what is registered by then
    RegisterExportPlugins();
    RegisterImportPlugins();
}).setPostInit([]() {
}).setDeInit([](){
    std::shared_ptr<au::project::IProjectConfiguration> projectConfiguratorPtr
        = muse::modularity::globalIoc()->resolve<au::project::IProjectConfiguration>("utests");
    muse::modularity::globalIoc()->unregister<au::project::IProjectConfiguration>("utests");

    au::project::IProjectConfiguration* projectConfigurator = projectConfiguratorPtr.get();
    delete projectConfigurator;
});
