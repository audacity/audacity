/*
* Audacity: A Digital Audio Editor
*/

#include "testing/environment.h"
#include "testing/testcontext.h"

#include "trackedit/itrackeditproject.h"
#include "au3wrap/au3wrapmodule.h"

#include "projectscene/tests/mocks/projectsceneconfigurationmock.h"

#include "project/tests/mocks/projectconfigurationmock.h"
#include "project/tests/mocks/trackeditprojectcreatormock.h"
#include "project/tests/mocks/projectviewstatecreatormock.h"
#include "trackedit/tests/mocks/clipboardmock.h"

namespace au::project {
static auto s_testCtx = au::testutils::makeTestContext();
static muse::testing::SuiteEnvironment audacityproject_se
    = muse::testing::SuiteEnvironment()
      .setDependencyModules({ new au::au3::Au3WrapModule()
                            })
      .setPreInit([](){
    std::shared_ptr<ProjectConfigurationMock> projectConfigurator(new ProjectConfigurationMock(), [](ProjectConfigurationMock*){});

    ON_CALL(*projectConfigurator, temporaryDir())
    .WillByDefault(::testing::Return(""));

    muse::modularity::globalIoc()->registerExport<IProjectConfiguration>("utests", projectConfigurator);

    std::shared_ptr<TrackeditProjectCreatorMock> trackeditProjectCreatorMock(
        new TrackeditProjectCreatorMock(),
        [](TrackeditProjectCreatorMock*) {}
        );

    ON_CALL(*trackeditProjectCreatorMock, create(::testing::_))
    .WillByDefault(::testing::Return(au::trackedit::ITrackeditProjectPtr {}));

    muse::modularity::globalIoc()->registerExport<au::trackedit::ITrackeditProjectCreator>(
        "utests",
        trackeditProjectCreatorMock
        );

    std::shared_ptr<au::projectscene::ProjectViewStateCreatorMock> projectViewStateCreatorMock(
        new au::projectscene::ProjectViewStateCreatorMock(),
        [](au::projectscene::ProjectViewStateCreatorMock*) {}
        );

    ON_CALL(*projectViewStateCreatorMock, createViewState(::testing::_))
    .WillByDefault(::testing::Return(au::projectscene::IProjectViewStatePtr {}));

    muse::modularity::globalIoc()->registerExport<au::projectscene::IProjectViewStateCreator>(
        "utests",
        projectViewStateCreatorMock
        );

    std::shared_ptr<::testing::NiceMock<au::projectscene::ProjectSceneConfigurationMock> > projectSceneConfigurator(new ::testing::NiceMock<au::projectscene::ProjectSceneConfigurationMock>(),
                                                                                                                    [](au::projectscene::
                                                                                                                       ProjectSceneConfigurationMock
                                                                                                                       *)
    {
    });

    static std::vector<au::projectscene::ClipColorInfo> colorInfos = {
        { "blue", 1 },
        { "red", 2 },
    };

    ON_CALL(*projectSceneConfigurator, clipColorInfos())
    .WillByDefault(::testing::ReturnRef(colorInfos));
    ON_CALL(*projectSceneConfigurator, clipColor(1))
    .WillByDefault(::testing::Return(QColor(0x00, 0x00, 0xFF)));
    ON_CALL(*projectSceneConfigurator, clipColor(2))
    .WillByDefault(::testing::Return(QColor(0xFF, 0x00, 0x00)));
    ON_CALL(*projectSceneConfigurator, clipSelectedColor(1))
    .WillByDefault(::testing::Return(QColor(0x80, 0x80, 0xFF)));
    ON_CALL(*projectSceneConfigurator, clipSelectedColor(2))
    .WillByDefault(::testing::Return(QColor(0xFF, 0x80, 0x80)));

    muse::modularity::globalIoc()->unregister<au::projectscene::IProjectSceneConfiguration>("utests");
    muse::modularity::globalIoc()->registerExport<au::projectscene::IProjectSceneConfiguration>("utests", projectSceneConfigurator);

    std::shared_ptr<au::trackedit::ClipboardMock> clipboardMock(
        new au::trackedit::ClipboardMock(),
        [](au::trackedit::ClipboardMock*) {}
        );

    muse::modularity::ioc(s_testCtx)->registerExport<au::trackedit::ITrackeditClipboard>(
        "utests",
        clipboardMock
        );
}).setPostInit([]() {
}).setDeInit([]()
{
    // Unregister and delete ProjectConfiguration
    {
        std::shared_ptr<IProjectConfiguration> projectConfiguratorPtr
            =muse::modularity::globalIoc()->resolve<IProjectConfiguration>("utests");
        muse::modularity::globalIoc()->unregister<IProjectConfiguration>("utests");

        if (projectConfiguratorPtr) {
            delete projectConfiguratorPtr.get();
        }
    }

    // Unregister and delete TrackeditProjectCreator
    {
        std::shared_ptr<au::trackedit::ITrackeditProjectCreator> trackeditProjectCreatorPtr
            =muse::modularity::globalIoc()->resolve<au::trackedit::ITrackeditProjectCreator>("utests");
        muse::modularity::globalIoc()->unregister<au::trackedit::ITrackeditProjectCreator>("utests");

        if (trackeditProjectCreatorPtr) {
            delete static_cast<TrackeditProjectCreatorMock*>(trackeditProjectCreatorPtr.get());
        }
    }

    // Unregister and delete ProjectViewStateCreator
    {
        std::shared_ptr<au::projectscene::IProjectViewStateCreator> projectViewStateCreatorPtr
            =muse::modularity::globalIoc()->resolve<au::projectscene::IProjectViewStateCreator>("utests");
        muse::modularity::globalIoc()->unregister<au::projectscene::IProjectViewStateCreator>("utests");

        if (projectViewStateCreatorPtr) {
            delete static_cast<au::projectscene::ProjectViewStateCreatorMock*>(projectViewStateCreatorPtr.get());
        }
    }

    // Unregister and delete ProjectSceneConfiguration
    {
        std::shared_ptr<au::projectscene::IProjectSceneConfiguration> projectSceneConfigPtr
            =muse::modularity::globalIoc()->resolve<au::projectscene::IProjectSceneConfiguration>("utests");
        muse::modularity::globalIoc()->unregister<au::projectscene::IProjectSceneConfiguration>("utests");

        if (projectSceneConfigPtr) {
            delete static_cast<au::projectscene::ProjectSceneConfigurationMock*>(projectSceneConfigPtr.get());
        }
    }

    // Unregister and delete ClipboardMock
    {
        std::shared_ptr<au::trackedit::ITrackeditClipboard> clipboardPtr
            =muse::modularity::ioc(s_testCtx)->resolve<au::trackedit::ITrackeditClipboard>("utests");

        muse::modularity::ioc(s_testCtx)->unregister<au::trackedit::ITrackeditClipboard>("utests");

        if (clipboardPtr) {
            delete static_cast<au::trackedit::ClipboardMock*>(clipboardPtr.get());
        }
    }
    muse::modularity::removeIoC(s_testCtx);
});
} // namespace au::project
