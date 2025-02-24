/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "context/tests/mocks/globalcontextmock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "mocks/trackeditprojectmock.h"

#include "au3wrap/internal/au3project.h"
#include "au3wrap/internal/domaccessor.h"

#include "trackedit/internal/au3/au3trackeditclipboard.h"

using ::testing::Return;
using ::testing::Truly;
using ::testing::_;

using namespace au;
using namespace au::au3;
using namespace au::trackedit;

namespace au::trackedit {
class au3TrackEditClipboardTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_au3TrackEditClipboard = std::make_unique<Au3TrackeditClipboard>();

        m_globalContext = std::make_shared<au::context::GlobalContextMock>();

        m_au3TrackEditClipboard->globalContext.set(m_globalContext);

        m_trackEditProject = std::make_shared<TrackeditProjectMock>();
        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(Return(m_trackEditProject));

        m_currentProject = std::make_shared<project::AudacityProjectMock>();
        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_currentProject));

        initTestProject();
    }

    void initTestProject()
    {
        m_au3ProjectAccessor = std::make_shared<au3::Au3ProjectAccessor>();
        const muse::io::path_t TEST_PROJECT_PATH = muse::String::fromUtf8(trackedit_tests_DATA_ROOT) + "/data/testClipboard.aup3";
        muse::Ret ret = m_au3ProjectAccessor->load(TEST_PROJECT_PATH);

        ON_CALL(*m_currentProject, au3ProjectPtr())
        .WillByDefault(Return(m_au3ProjectAccessor->au3ProjectPtr()));
    }

    Au3Project& projectRef() const
    {
        auto project = reinterpret_cast<Au3Project*>(m_au3ProjectAccessor->au3ProjectPtr());
        return *project;
    }

    void TearDown() override
    {
        m_au3ProjectAccessor->close();
    }

    std::unique_ptr<Au3TrackeditClipboard> m_au3TrackEditClipboard;

    std::shared_ptr<au::context::GlobalContextMock> m_globalContext;
    std::shared_ptr<project::AudacityProjectMock> m_currentProject;
    std::shared_ptr<TrackeditProjectMock> m_trackEditProject;

    std::shared_ptr<au3::Au3ProjectAccessor> m_au3ProjectAccessor;
};

TEST_F(au3TrackEditClipboardTests, trackDataCopy)
{
    //! [GIVEN] There is a project with two tracks, and five clips grouped as follows:
    //          2x2, 1. The first group spans two tracks, the second one.

    int numberOfTracks = 2;

    Au3Project& project = projectRef();
    auto& trackFactory = WaveTrackFactory::Get(projectRef());
    auto& pSampleBlockFactory = trackFactory.GetSampleBlockFactory();

    //! Ensuring TrackData list is populated with the loaded model.

    for (int i = 0; i < numberOfTracks; ++i) {
        const auto au3Track = DomAccessor::findTrackByIndex(project, i);
        TrackId trackId = au3Track->GetId();
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

        auto clipboardTrack = waveTrack->EmptyCopy(pSampleBlockFactory);

        auto waveClips = DomAccessor::waveClipsAsList(waveTrack);

        ClipKeyList selectedTrackClips;
        for (auto& clip : waveClips) {
            selectedTrackClips.push_back({trackId, clip->GetId()});
        }

        std::vector<std::shared_ptr<Au3WaveClip> > intervals;
        for (const auto& clipKey : selectedTrackClips) {
             std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
             clipboardTrack->InsertInterval(waveTrack->CopyClip(*clip, true), false);
        }

        m_au3TrackEditClipboard->addTrackData({clipboardTrack, ClipKey()}); // (Dummy ClipKey)

        m_au3TrackEditClipboard->setMultiSelectionCopy(true);
    }

    auto result = m_au3TrackEditClipboard->trackDataCopy();

    muse::Ret ret;
    EXPECT_EQ(ret, make_ret(muse::Ret::Code::Ok));
}
}