/*
* Audacity: A Digital Audio Editor
*/

#include "testing/environment.h"

#include "mocks/workspaceconfigurationmock.h"

using namespace ::testing;

static muse::testing::SuiteEnvironment appshell_se
    = muse::testing::SuiteEnvironment()
      .setPreInit([](){
    auto workspaceConfig = std::make_shared<::testing::NiceMock<au::appshell::WorkspaceConfigurationMock> >();

    ON_CALL(*workspaceConfig, defaultWorkspaceName())
    .WillByDefault(Return("Modern"));

    muse::modularity::globalIoc()->registerExport<muse::workspace::IWorkspaceConfiguration>("utests", workspaceConfig);
}).setDeInit([](){
    muse::modularity::globalIoc()->unregister<muse::workspace::IWorkspaceConfiguration>("utests");
});
