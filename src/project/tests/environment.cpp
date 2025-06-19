/*
* Audacity: A Digital Audio Editor
*/

#include "testing/environment.h"

#include "au3wrap/au3wrapmodule.h"
#include "trackedit/trackeditmodule.h"
#include "trackedit/itrackeditproject.h"

#include "projectscene/tests/mocks/projectsceneconfigurationmock.h"

#include "project/tests/mocks/projectconfigurationmock.h"
#include "project/tests/mocks/trackeditprojectcreatormock.h"
#include "project/tests/mocks/projectviewstatecreatormock.h"

using namespace ::testing;
using namespace au::project;

static muse::testing::SuiteEnvironment audacityproject_se
    = muse::testing::SuiteEnvironment()
      .setDependencyModules({ new au::au3::Au3WrapModule(),
                              // new au::trackedit::TrackeditModule()
                            })
      .setPreInit([](){
    std::shared_ptr<ProjectConfigurationMock> projectConfigurator(new ProjectConfigurationMock(), [](ProjectConfigurationMock*){});

    ON_CALL(*projectConfigurator, temporaryDir())
    .WillByDefault(Return(""));

    muse::modularity::globalIoc()->registerExport<IProjectConfiguration>("utests", projectConfigurator);

    // Create the mock instance with a no-op deleter (to avoid deletion)
    std::shared_ptr<au::project::TrackeditProjectCreatorMock> trackeditProjectCreatorMock(
        new au::project::TrackeditProjectCreatorMock(),
        [](au::project::TrackeditProjectCreatorMock*) {} // no-op deleter
        );

    // Set up default behavior for 'create' method
    ON_CALL(*trackeditProjectCreatorMock, create(::testing::_))
    .WillByDefault(::testing::Return(au::trackedit::ITrackeditProjectPtr {}));  // Return empty or dummy ptr

    // Register the mock with the global IOC container under the expected interface type
    muse::modularity::globalIoc()->registerExport<au::trackedit::ITrackeditProjectCreator>(
        "utests",
        trackeditProjectCreatorMock
        );

    // Create the mock instance with a no-op deleter (to avoid deletion)
    std::shared_ptr<au::projectscene::ProjectViewStateCreatorMock> projectViewStateCreatorMock(
        new au::projectscene::ProjectViewStateCreatorMock(),
        [](au::projectscene::ProjectViewStateCreatorMock*) {}       // no-op deleter
        );

    // Set up default behavior for 'createViewState' method
    ON_CALL(*projectViewStateCreatorMock, createViewState(::testing::_))
    .WillByDefault(::testing::Return(au::projectscene::IProjectViewStatePtr {}));          // return nullptr or dummy

    // Register the mock with the global IOC container under the expected interface type
    muse::modularity::globalIoc()->registerExport<au::projectscene::IProjectViewStateCreator>(
        "utests",
        projectViewStateCreatorMock
        );

    std::shared_ptr<NiceMock<au::projectscene::ProjectSceneConfigurationMock> > projectSceneConfigurator(new NiceMock<au::projectscene::ProjectSceneConfigurationMock>(),
                                                                                                         [](au::projectscene::
                                                                                                            ProjectSceneConfigurationMock*)
    {
    });                                                                                                                                                                  // no delete

    static std::vector<std::pair<std::string /*name*/, std::string /*color*/> > colors = {
        { "blue", "#0000FF" },
        { "red", "#FF0000" },
    };

    ON_CALL(*projectSceneConfigurator, clipColors())
    .WillByDefault(ReturnRef(colors));

    muse::modularity::globalIoc()->unregister<au::projectscene::IProjectSceneConfiguration>("utests");
    muse::modularity::globalIoc()->registerExport<au::projectscene::IProjectSceneConfiguration>("utests", projectSceneConfigurator);
}).setPostInit([]() {
}).setDeInit([](){
    std::shared_ptr<IProjectConfiguration> projectConfiguratorPtr
        = muse::modularity::globalIoc()->resolve<IProjectConfiguration>("utests");
    muse::modularity::globalIoc()->unregister<IProjectConfiguration>("utests");

    IProjectConfiguration* projectConfigurator = projectConfiguratorPtr.get();
    delete projectConfigurator;
});
