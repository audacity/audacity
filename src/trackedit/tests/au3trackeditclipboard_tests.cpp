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
#include "trackedit/internal/au3/au3trackdata.h"

using ::testing::Return;
using ::testing::Truly;
using ::testing::_;

using namespace au;
using namespace au::au3;
using namespace au::trackedit;

namespace au::trackedit {
class Au3TrackEditClipboardTests : public ::testing::Test
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

        // The testClipboard.aup3 project contains two groups, 0 and 1
        m_oldGroupIds.push_back((0));
        m_oldGroupIds.push_back((1));

        ON_CALL(*m_trackEditProject, groupsIdsList())
        .WillByDefault(Return(m_oldGroupIds));

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

    /**
     *  Populates a Au3TrackDataPtr vector from the loaded project's model.
     */
    std::vector<Au3TrackDataPtr> buildTrackData()
    {
        std::vector<Au3TrackDataPtr> trackDataList;

        Au3Project& project = projectRef();
        auto& trackFactory = WaveTrackFactory::Get(projectRef());
        auto& pSampleBlockFactory = trackFactory.GetSampleBlockFactory();

        for (int i = 0; i < m_numberOfTracks; ++i) {
            const auto au3Track = DomAccessor::findTrackByIndex(project, i);
            TrackId trackId = au3Track->GetId();
            Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

            auto clipboardTrack = waveTrack->EmptyCopy(pSampleBlockFactory);

            auto waveClips = DomAccessor::waveClipsAsList(waveTrack);

            ClipKeyList selectedTrackClips;
            for (auto& clip : waveClips) {
                selectedTrackClips.push_back({ trackId, clip->GetId() });
            }

            std::vector<std::shared_ptr<Au3WaveClip> > intervals;
            for (const auto& clipKey : selectedTrackClips) {
                std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(waveTrack, clipKey.clipId);
                clipboardTrack->InsertInterval(waveTrack->CopyClip(*clip, true), false);
            }

            const auto trackData = std::make_shared<Au3TrackData>(clipboardTrack);
            trackDataList.push_back(trackData);

            m_au3TrackEditClipboard->addTrackData(std::static_pointer_cast<ITrackData>(trackData));
        }

        m_au3TrackEditClipboard->setMultiSelectionCopy(true);

        return trackDataList;
    }

    const int m_numberOfTracks = 2; // The number of tracks in testClipboard.aup3

    std::unique_ptr<Au3TrackeditClipboard> m_au3TrackEditClipboard;

    std::shared_ptr<au::context::GlobalContextMock> m_globalContext;
    std::shared_ptr<project::AudacityProjectMock> m_currentProject;
    std::shared_ptr<TrackeditProjectMock> m_trackEditProject;

    std::shared_ptr<au3::Au3ProjectAccessor> m_au3ProjectAccessor;

    std::vector<int64_t> m_oldGroupIds;
};

TEST_F(Au3TrackEditClipboardTests, groupedTrackDataCopy)
{
    //! [GIVEN] There is a project with two tracks, and five clips grouped as follows:
    //          2x2, 1. The first group spans two tracks, the second group one track.
    //          And the first track has four clips, the second track one clip (in the first group).

    EXPECT_CALL(*m_trackEditProject, createNewGroupID(_))
    .WillOnce(Return(2))
    .WillOnce(Return(3));

    EXPECT_TRUE(m_au3TrackEditClipboard->trackDataEmpty());

    std::vector<Au3TrackDataPtr> trackDataBefore = buildTrackData();

    auto trackDataAfter = m_au3TrackEditClipboard->trackDataCopy();

    EXPECT_FALSE(m_au3TrackEditClipboard->trackDataEmpty());

    EXPECT_EQ(trackDataBefore.size(), trackDataAfter.size());

    int ungrouped = 0;

    //! Iterates over returned Au3TrackDataPtr list and compares to the original one.
    //  Ensures that the same number of clips and tracks are present,
    //  and that the groupID's are new.

    auto tracksBeforeIter = trackDataBefore.begin();
    auto tracksAfterIter = trackDataAfter.begin();
    for (; tracksBeforeIter != trackDataBefore.end() && tracksAfterIter != trackDataAfter.end();
         ++tracksBeforeIter, ++tracksAfterIter) {
        auto waveTrackBefore = dynamic_cast<au3::Au3WaveTrack*>((*tracksBeforeIter)->track().get());
        auto waveTrackAfter = dynamic_cast<au3::Au3WaveTrack*>(std::static_pointer_cast<Au3TrackData>(*tracksAfterIter)->track().get());
        auto clipsBefore = waveTrackBefore->Intervals();
        auto clipsAfter = waveTrackAfter->Intervals();

        int clipCountBefore = clipsBefore.size();
        int clipCountAfter = clipsAfter.size();
        EXPECT_EQ(clipCountBefore, clipCountAfter);

        auto clipsBeforeIter = clipsBefore.begin();
        auto clipsAfterIter = clipsAfter.begin();
        for (; clipsBeforeIter != clipsBefore.end() && clipsAfterIter != clipsAfter.end();
             ++clipsBeforeIter, ++clipsAfterIter) {
            int64_t idBefore = (*clipsBeforeIter).get()->GetGroupId();
            int64_t idAfter = (*clipsAfterIter).get()->GetGroupId();

            if (idBefore == -1 || idAfter == -1) {
                ungrouped++;
            } else {
                EXPECT_TRUE(idBefore != idAfter);
            }
        }
    }

    //! The project file contains exactly one ungrouped clip
    EXPECT_EQ(ungrouped, 1);

    //! NOTE:: The following should ideally be in their own unit-test,
    //         but given how trivial they are it's better to call them here,
    //         instead of doing all the SetUp and project-loading just to check that
    //         accessors, and clearing a vector, work as expected.

    EXPECT_FALSE(m_au3TrackEditClipboard->trackDataEmpty());

    EXPECT_EQ(m_au3TrackEditClipboard->trackDataSize(), 2);

    EXPECT_TRUE(m_au3TrackEditClipboard->isMultiSelectionCopy());

    m_au3TrackEditClipboard->clearTrackData();

    EXPECT_EQ(m_au3TrackEditClipboard->trackDataSize(), 0);

    EXPECT_TRUE(m_au3TrackEditClipboard->trackDataEmpty());
    EXPECT_FALSE(m_au3TrackEditClipboard->isMultiSelectionCopy());
}
}
