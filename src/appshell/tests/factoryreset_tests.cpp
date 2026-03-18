/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <QTemporaryDir>

#include "modularity/ioc.h"
#include "multiwindows/imultiwindowsprovider.h"

#include "workspace/internal/workspacemanager.h"
#include "workspace/iworkspaceconfiguration.h"
#include "mocks/workspaceconfigurationmock.h"

using ::testing::Return;
using ::testing::_;

using namespace muse;
using namespace muse::workspace;

// Path to share/workspaces/ in the source tree (set by CMake)
static const std::string BUILTIN_WORKSPACE_DIR = BUILTIN_WORKSPACES_DIR;

namespace muse::mi {
class MultiWindowsProviderMock : public IMultiWindowsProvider
{
public:
    MOCK_METHOD(size_t, windowCount, (), (const, override));
    MOCK_METHOD(bool, isFirstWindow, (), (const, override));
    MOCK_METHOD(bool, isProjectAlreadyOpened, (const io::path_t&), (const, override));
    MOCK_METHOD(void, activateWindowWithProject, (const io::path_t&), (override));
    MOCK_METHOD(bool, isHasWindowWithoutProject, (), (const, override));
    MOCK_METHOD(void, activateWindowWithoutProject, (const QStringList&), (override));
    MOCK_METHOD(bool, openNewWindow, (const QStringList&), (override));
    MOCK_METHOD(bool, isPreferencesAlreadyOpened, (), (const, override));
    MOCK_METHOD(void, activateWindowWithOpenedPreferences, (), (const, override));
    MOCK_METHOD(void, settingsBeginTransaction, (), (override));
    MOCK_METHOD(void, settingsCommitTransaction, (), (override));
    MOCK_METHOD(void, settingsRollbackTransaction, (), (override));
    MOCK_METHOD(void, settingsReset, (), (override));
    MOCK_METHOD(void, settingsSetValue, (const std::string&, const Val&), (override));
    MOCK_METHOD(bool, lockResource, (const std::string&), (override));
    MOCK_METHOD(bool, unlockResource, (const std::string&), (override));
    MOCK_METHOD(void, notifyAboutResourceChanged, (const std::string&), (override));
    MOCK_METHOD(async::Channel<std::string>, resourceChanged, (), (override));
    MOCK_METHOD(void, notifyAboutWindowWasQuited, (), (override));
    MOCK_METHOD(void, quitForAll, (), (override));
    MOCK_METHOD(void, quitAllAndRestartLast, (), (override));
    MOCK_METHOD(void, quitAllAndRunInstallation, (const io::path_t&), (override));
};
}

namespace au::appshell {
class FactoryResetWorkspaceTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_userWorkspacesDir = std::make_unique<QTemporaryDir>();
        ASSERT_TRUE(m_userWorkspacesDir->isValid());

        // Register mock MultiWindowsProvider (Workspace constructor needs it)
        m_multiWindowsProvider = std::make_shared<::testing::NiceMock<mi::MultiWindowsProviderMock> >();
        modularity::globalIoc()->registerExport<mi::IMultiWindowsProvider>("utests", m_multiWindowsProvider);

        // Get the workspace configuration mock registered by the environment
        m_workspaceConfig = std::dynamic_pointer_cast<WorkspaceConfigurationMock>(
            modularity::globalIoc()->resolve<IWorkspaceConfiguration>("utests"));
        ASSERT_NE(m_workspaceConfig, nullptr);

        m_userWorkspacesPath = m_userWorkspacesDir->path().toStdString();
    }

    void TearDown() override
    {
        m_manager.reset();
        modularity::globalIoc()->unregister<mi::IMultiWindowsProvider>("utests");
    }

    void setupBuiltinPaths(const std::vector<std::string>& filenames)
    {
        io::paths_t paths;
        for (const auto& name : filenames) {
            paths.push_back(io::path_t(BUILTIN_WORKSPACE_DIR + "/" + name));
        }

        ON_CALL(*m_workspaceConfig, builtinWorkspacesFilePaths())
        .WillByDefault(Return(paths));

        ON_CALL(*m_workspaceConfig, userWorkspacesPath())
        .WillByDefault(Return(io::path_t(m_userWorkspacesPath)));
    }

    void initManager()
    {
        m_manager = std::make_unique<WorkspaceManager>(modularity::globalCtx());
        m_manager->init();
    }

protected:
    std::unique_ptr<QTemporaryDir> m_userWorkspacesDir;
    std::string m_userWorkspacesPath;

    std::shared_ptr<::testing::NiceMock<mi::MultiWindowsProviderMock> > m_multiWindowsProvider;
    std::shared_ptr<WorkspaceConfigurationMock> m_workspaceConfig;

    std::unique_ptr<WorkspaceManager> m_manager;
};

/**
 * @brief Normal startup: builtin workspace files exist, default workspace loads
 * @details Verifies that WorkspaceManager loads the default "Modern" workspace
 *          from builtin workspace files when no user workspaces exist.
 */
TEST_F(FactoryResetWorkspaceTests, BuiltinWorkspaceLoadsOnInit)
{
    //! [GIVEN] Builtin workspace files exist
    setupBuiltinPaths({ "Modern.mws", "Classic.mws", "Music.mws" });

    //! [GIVEN] Current workspace is "Modern" (the default)
    ON_CALL(*m_workspaceConfig, currentWorkspaceName())
    .WillByDefault(Return("Modern"));

    //! [WHEN] WorkspaceManager is initialized
    initManager();

    //! [THEN] Default workspace is loaded
    EXPECT_NE(m_manager->defaultWorkspace(), nullptr);
    EXPECT_EQ(m_manager->defaultWorkspace()->name(), "Modern");

    //! [THEN] Current workspace matches default
    EXPECT_NE(m_manager->currentWorkspace(), nullptr);
    EXPECT_EQ(m_manager->currentWorkspace()->name(), "Modern");

    //! [THEN] All builtin workspaces are available
    EXPECT_EQ(m_manager->workspaces().size(), 3);

    m_manager->deinit();
}

/**
 * @brief Factory reset scenario: current workspace was deleted, should fall back
 * @details After factory reset, user workspace files are deleted. If settings
 *          still reference a custom workspace that no longer exists, the manager
 *          should fall back to the builtin default workspace.
 */
TEST_F(FactoryResetWorkspaceTests, FallbackToDefaultOnMissingCurrentWorkspace)
{
    //! [GIVEN] Builtin workspace files exist
    setupBuiltinPaths({ "Modern.mws", "Classic.mws", "Music.mws" });

    //! [GIVEN] Current workspace name references a deleted custom workspace
    ON_CALL(*m_workspaceConfig, currentWorkspaceName())
    .WillByDefault(Return("MyDeletedWorkspace"));

    //! [GIVEN] setCurrentWorkspaceName will be called to correct the setting
    EXPECT_CALL(*m_workspaceConfig, setCurrentWorkspaceName("Modern"))
    .Times(1);

    //! [WHEN] WorkspaceManager is initialized
    initManager();

    //! [THEN] Falls back to default workspace
    EXPECT_NE(m_manager->currentWorkspace(), nullptr);
    EXPECT_EQ(m_manager->currentWorkspace()->name(), "Modern");

    //! [THEN] Default workspace is valid
    EXPECT_NE(m_manager->defaultWorkspace(), nullptr);
    EXPECT_EQ(m_manager->defaultWorkspace()->name(), "Modern");

    m_manager->deinit();
}

/**
 * @brief Factory reset with empty user directory: only builtins available
 * @details After factory reset, the user workspace directory is empty.
 *          The manager should load builtin workspaces and use the default.
 */
TEST_F(FactoryResetWorkspaceTests, EmptyUserDirUsesBuiltinWorkspaces)
{
    //! [GIVEN] Builtin workspace files exist
    setupBuiltinPaths({ "Modern.mws", "Classic.mws", "Music.mws" });

    //! [GIVEN] Current workspace is default
    ON_CALL(*m_workspaceConfig, currentWorkspaceName())
    .WillByDefault(Return("Modern"));

    //! [GIVEN] User workspace directory is empty (factory reset cleaned it)
    //! (The temp dir is already empty)

    //! [WHEN] WorkspaceManager is initialized
    initManager();

    //! [THEN] Workspaces are loaded from builtins only
    auto workspaces = m_manager->workspaces();
    EXPECT_EQ(workspaces.size(), 3);

    //! [THEN] Current workspace is loaded and usable
    auto current = m_manager->currentWorkspace();
    ASSERT_NE(current, nullptr);
    EXPECT_EQ(current->name(), "Modern");

    //! [THEN] Reading workspace data does not crash
    auto data = current->rawData("ui_settings");
    EXPECT_TRUE(data.ret);

    m_manager->deinit();
}

/**
 * @brief Workspace fallback when builtin also cannot be found by name
 * @details If current workspace and reload both fail, the manager should
 *          call setupDefaultWorkspace() to establish a fresh default.
 */
TEST_F(FactoryResetWorkspaceTests, FallbackWhenCurrentNotFoundAfterReload)
{
    //! [GIVEN] Only Modern builtin exists
    setupBuiltinPaths({ "Modern.mws" });

    //! [GIVEN] Settings reference a workspace that doesn't exist anywhere
    ON_CALL(*m_workspaceConfig, currentWorkspaceName())
    .WillByDefault(Return("NonExistent"));

    //! [GIVEN] Manager will correct the setting to default
    EXPECT_CALL(*m_workspaceConfig, setCurrentWorkspaceName("Modern"))
    .Times(1);

    //! [WHEN] WorkspaceManager is initialized
    initManager();

    //! [THEN] Current workspace falls back to "Modern"
    EXPECT_NE(m_manager->currentWorkspace(), nullptr);
    EXPECT_EQ(m_manager->currentWorkspace()->name(), "Modern");

    m_manager->deinit();
}
}
