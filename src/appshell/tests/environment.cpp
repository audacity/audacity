/*
* Audacity: A Digital Audio Editor
*/

#include "testing/environment.h"

#include "workspace/tests/mocks/workspaceconfigurationmock.h"

using namespace ::testing;

static muse::testing::SuiteEnvironment appshell_se
    = muse::testing::SuiteEnvironment()
      .setPreInit([](){
    auto workspaceConfig = std::make_shared<::testing::NiceMock<muse::workspace::WorkspaceConfigurationMock> >();

    ON_CALL(*workspaceConfig, defaultWorkspaceName())
    .WillByDefault(Return("Modern"));

    muse::modularity::globalIoc()->registerExport<muse::workspace::IWorkspaceConfiguration>("utests", workspaceConfig);
}).setDeInit([](){
    muse::modularity::globalIoc()->unregister<muse::workspace::IWorkspaceConfiguration>("utests");
});
