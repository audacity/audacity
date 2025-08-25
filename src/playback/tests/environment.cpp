/*
* Audacity: A Digital Audio Editor
*/

#include "testing/environment.h"

#include "au3wrap/au3wrapmodule.h"

#include "project/tests/mocks/projectconfigurationmock.h"
#include "mocks/playbackconfigurationmock.h"

using namespace ::testing;

static muse::testing::SuiteEnvironment playback_se
    = muse::testing::SuiteEnvironment()
      .setDependencyModules({ new au::au3::Au3WrapModule() })
      .setPreInit([](){
    std::shared_ptr<au::playback::PlaybackConfigurationMock> playbackConfigurator(new au::playback::PlaybackConfigurationMock(),
                                                                                  [](au::playback::PlaybackConfigurationMock*){});
    std::shared_ptr<au::project::ProjectConfigurationMock> projectConfigurator(new au::project::ProjectConfigurationMock(),
                                                                               [](au::project::ProjectConfigurationMock*){});

    ON_CALL(*projectConfigurator, temporaryDir())
    .WillByDefault(Return(""));

    muse::modularity::globalIoc()->registerExport<au::playback::IPlaybackConfiguration>("utests", playbackConfigurator);
    muse::modularity::globalIoc()->registerExport<au::project::IProjectConfiguration>("utests", projectConfigurator);
}).setPostInit([]() {
}).setDeInit([](){
    std::shared_ptr<au::playback::IPlaybackConfiguration> playbackConfiguratorPtr
        = muse::modularity::globalIoc()->resolve<au::playback::IPlaybackConfiguration>("utests");
    muse::modularity::globalIoc()->unregister<au::playback::IPlaybackConfiguration>("utests");

    std::shared_ptr<au::project::IProjectConfiguration> projectConfiguratorPtr
        = muse::modularity::globalIoc()->resolve<au::project::IProjectConfiguration>("utests");
    muse::modularity::globalIoc()->unregister<au::project::IProjectConfiguration>("utests");

    au::playback::IPlaybackConfiguration* playbackConfigurator = playbackConfiguratorPtr.get();
    au::project::IProjectConfiguration* projectConfigurator = projectConfiguratorPtr.get();
    delete playbackConfigurator;
    delete projectConfigurator;
});
