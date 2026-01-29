/*
 * Audacity: A Digital Audio Editor
 */
#include "../internal/projectviewstate.h"

#include "mocks/projectsceneconfigurationmock.h"

#include "au3wrap/tests/mocks/au3projectmock.h"
#include "au3wrap/internal/au3project.h"
#include "au3wrap/internal/domaccessor.h"

#include "playback/tests/mocks/playbackconfigurationmock.h"
#include "context/tests/mocks/globalcontextmock.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/projecthistorymock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "trackedit/trackedittypes.h"

#include <gtest/gtest.h>

using namespace ::testing;
using ::testing::_;

namespace au::projectscene {
class ProjectViewStateTests : public ::testing::Test
{
public:

    void SetUp() override
    {
        m_projectViewState = std::make_shared<ProjectViewState>(muse::modularity::globalCtx());

        m_projectViewState->configuration.set(m_configurationMock);
        m_projectViewState->playbackConfiguration.set(m_playbackConfigurationMock);
        m_projectViewState->globalContext.set(m_globalContextMock);
        m_projectViewState->selectionController.set(m_selectionControllerMock);
        m_projectViewState->projectHistory.set(m_projectHistoryMock);

        ON_CALL(*m_globalContextMock, currentTrackeditProjectChanged())
        .WillByDefault(Return(muse::async::Notification()));

        ON_CALL(*m_projectHistoryMock, historyChanged())
        .WillByDefault(Return(muse::async::Notification()));

        ON_CALL(*m_globalContextMock, currentProject())
        .WillByDefault(Return(m_currentProject));

        ON_CALL(*m_globalContextMock, currentTrackeditProject())
        .WillByDefault(Return(m_trackeditProjectMock));

        initTestProject();

        m_projectViewState->init(m_au3ProjectMock);
    }

    void TearDown() override
    {
        m_au3ProjectAccessor->close();
    }

    trackedit::TrackIdList getAllTracks()
    {
        const auto au3ProjectPtr = reinterpret_cast<au3::Au3Project*>(m_au3ProjectAccessor->au3ProjectPtr());
        trackedit::TrackIdList tracks;
        size_t trackIndex = 0;
        while (true) {
            const auto track = au3::DomAccessor::findTrackByIndex(*au3ProjectPtr, trackIndex);
            if (track == nullptr) {
                break;
            }
            tracks.push_back(track->GetId());
            ++trackIndex;
        }
        return tracks;
    }

    void initTestProject()
    {
        const muse::io::path_t TEST_PROJECT_PATH = muse::String::fromUtf8(projectscene_tests_DATA_ROOT) + "/data/test.aup4";
        constexpr auto discardAutosave = false;
        muse::Ret ret = m_au3ProjectAccessor->load(TEST_PROJECT_PATH, discardAutosave);

        ON_CALL(*m_au3ProjectMock, au3ProjectPtr())
        .WillByDefault(Return(m_au3ProjectAccessor->au3ProjectPtr()));

        ON_CALL(*m_currentProject, au3ProjectPtr())
        .WillByDefault(Return(m_au3ProjectAccessor->au3ProjectPtr()));

        ON_CALL(*m_trackeditProjectMock, trackIdList())
        .WillByDefault(Return(getAllTracks()));
    }

protected:
    std::shared_ptr<ProjectViewState> m_projectViewState = nullptr;

    const std::shared_ptr<NiceMock<au::au3::Au3ProjectMock> > m_au3ProjectMock
        = std::make_shared<NiceMock<au::au3::Au3ProjectMock> >();

    const std::shared_ptr<NiceMock<ProjectSceneConfigurationMock> > m_configurationMock
        = std::make_shared<NiceMock<ProjectSceneConfigurationMock> >();

    const std::shared_ptr<NiceMock<au::playback::PlaybackConfigurationMock> > m_playbackConfigurationMock
        = std::make_shared<NiceMock<au::playback::PlaybackConfigurationMock> >();

    const std::shared_ptr<NiceMock<au::context::GlobalContextMock> > m_globalContextMock
        = std::make_shared<NiceMock<au::context::GlobalContextMock> >();

    const std::shared_ptr<NiceMock<trackedit::SelectionControllerMock> > m_selectionControllerMock
        = std::make_shared<NiceMock<trackedit::SelectionControllerMock> >();

    const std::shared_ptr<NiceMock<trackedit::ProjectHistoryMock> > m_projectHistoryMock
        = std::make_shared<NiceMock<trackedit::ProjectHistoryMock> >();

    const std::shared_ptr<NiceMock<project::AudacityProjectMock> > m_currentProject
        = std::make_shared<NiceMock<project::AudacityProjectMock> >();

    const std::shared_ptr<NiceMock<trackedit::TrackeditProjectMock> > m_trackeditProjectMock
        = std::make_shared<NiceMock<trackedit::TrackeditProjectMock> >();

    const std::shared_ptr<au3::Au3ProjectAccessor> m_au3ProjectAccessor = std::make_shared<au3::Au3ProjectAccessor>();
};

TEST_F(ProjectViewStateTests, tracksInRange)
{
}
} // namespace au::projectscene
