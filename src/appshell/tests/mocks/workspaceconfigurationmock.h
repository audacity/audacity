/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_APPSHELL_TESTS_WORKSPACECONFIGURATIONMOCK_H
#define AU_APPSHELL_TESTS_WORKSPACECONFIGURATIONMOCK_H

#include <gmock/gmock.h>

#include "workspace/iworkspaceconfiguration.h"

namespace au::appshell {
class WorkspaceConfigurationMock : public muse::workspace::IWorkspaceConfiguration
{
public:
    MOCK_METHOD(muse::io::paths_t, workspacePaths, (), (const, override));
    MOCK_METHOD(muse::io::paths_t, builtinWorkspacesFilePaths, (), (const, override));
    MOCK_METHOD(muse::io::path_t, userWorkspacesPath, (), (const, override));
    MOCK_METHOD(std::string, defaultWorkspaceName, (), (const, override));
    MOCK_METHOD(std::string, currentWorkspaceName, (), (const, override));
    MOCK_METHOD(void, setCurrentWorkspaceName, (const std::string&), (override));
    MOCK_METHOD(muse::async::Channel<std::string>, currentWorkspaceNameChanged, (), (const, override));
};
}

#endif // AU_APPSHELL_TESTS_WORKSPACECONFIGURATIONMOCK_H
