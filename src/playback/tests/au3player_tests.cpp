/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "au3audio/tests/mocks/audioenginemock.h"
#include "context/tests/mocks/globalcontextmock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "project/tests/testtools.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/au3project.h"
#include "au3wrap/tests/tracktemplatefactory.h"

#include "au3-time-frequency-selection/ViewInfo.h"

#include "../internal/au3/au3player.h"

using ::testing::_;
using ::testing::DoubleNear;
using ::testing::NiceMock;
using ::testing::Return;

namespace au::playback {
constexpr double SAMPLE_RATE = 44100.0;
constexpr double TRACK_DURATION = 0.5;
constexpr double TIME_TOLERANCE = 1e-3;

class Au3PlayerTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_selectionController = std::make_shared<NiceMock<trackedit::SelectionControllerMock> >();
        m_audioEngine = std::make_shared<NiceMock<audio::AudioEngineMock> >();
        m_currentProject = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        m_trackeditProject = std::make_shared<NiceMock<trackedit::TrackeditProjectMock> >();

        //! NOTE: Au3Player subscribes to the global context in its constructor,
        //! so the mocks must be resolvable from the IoC before construction.
        //! Context-scoped interfaces live in a per-context IoC, global ones in the global IoC.
        m_ctx = std::make_shared<muse::modularity::Context>(301);
        muse::modularity::ioc(m_ctx)->registerExport<context::IGlobalContext>("utests", m_globalContext);
        muse::modularity::ioc(m_ctx)->registerExport<trackedit::ISelectionController>("utests", m_selectionController);
        muse::modularity::globalIoc()->registerExport<audio::IAudioEngine>("utests", m_audioEngine);

        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_currentProject));
        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(Return(m_trackeditProject));

        //! NOTE: a real, headless au3 project provides ViewInfo (play region),
        //! the track list and the audio IO token storage
        m_projectAccessor = std::make_shared<au3::Au3ProjectAccessor>(muse::modularity::globalCtx());

        const std::string source
            = (muse::String::fromUtf8(playback_tests_DATA_ROOT) + "/../../trackedit/tests/data/empty.aup4").toStdString();
        m_workingProjectPath = (muse::String::fromUtf8(playback_tests_DATA_ROOT) + "/au3player_working.aup4").toStdString();
        testtools::removeProjectIfExists(m_workingProjectPath);
        ASSERT_TRUE(testtools::copyFile(source, m_workingProjectPath));

        constexpr auto discardAutosave = false;
        muse::Ret ret = m_projectAccessor->load(muse::io::path_t(m_workingProjectPath), discardAutosave);
        ASSERT_TRUE(ret);

        ON_CALL(*m_currentProject, au3ProjectPtr())
        .WillByDefault(Return(m_projectAccessor->au3ProjectPtr()));

        m_player = std::make_shared<Au3Player>(m_ctx);

        //! [GIVEN] The project has one track with audio
        au3::TrackTemplateFactory factory(projectRef(), SAMPLE_RATE);
        factory.addTrackFromTemplate("track", {
                { 0.0, { { TRACK_DURATION, au3::TrackTemplateFactory::createNoise } } }
            });

        //! [GIVEN] The audio engine is idle and startable by default
        ON_CALL(*m_audioEngine, isBusy())
        .WillByDefault(Return(false));
        ON_CALL(*m_audioEngine, canStopAudioStream(_))
        .WillByDefault(Return(true));
        ON_CALL(*m_audioEngine, startStream(_, _, _, _, _, _))
        .WillByDefault(Return(1));
    }

    void TearDown() override
    {
        m_player.reset();

        au3::Au3TrackList::Get(projectRef()).Clear();
        m_projectAccessor->clearSavedState();
        m_projectAccessor->close();
        m_projectAccessor.reset();
        testtools::removeProjectIfExists(m_workingProjectPath);

        muse::modularity::globalIoc()->unregister<audio::IAudioEngine>("utests");
        muse::modularity::removeIoC(m_ctx);
    }

    au3::Au3Project& projectRef() const
    {
        return *reinterpret_cast<au3::Au3Project*>(m_projectAccessor->au3ProjectPtr());
    }

    //! NOTE: the loop region is au3's ViewInfo::playRegion in its active state
    void setLoopRegion(double start, double end)
    {
        auto& playRegion = ViewInfo::Get(projectRef()).playRegion;
        playRegion.SetAllTimes(start, end);
        playRegion.SetActive(true);
    }

    muse::modularity::ContextPtr m_ctx;
    std::shared_ptr<NiceMock<context::GlobalContextMock> > m_globalContext;
    std::shared_ptr<NiceMock<trackedit::SelectionControllerMock> > m_selectionController;
    std::shared_ptr<NiceMock<audio::AudioEngineMock> > m_audioEngine;
    std::shared_ptr<NiceMock<project::AudacityProjectMock> > m_currentProject;
    std::shared_ptr<NiceMock<trackedit::TrackeditProjectMock> > m_trackeditProject;

    std::shared_ptr<au3::Au3ProjectAccessor> m_projectAccessor;
    std::string m_workingProjectPath;

    std::shared_ptr<Au3Player> m_player;
};

/**
 * @brief Play with an active loop region streams the loop region bounds
 */
TEST_F(Au3PlayerTests, Play_WithActiveLoopRegion_StreamsLoopRegionBounds)
{
    //! [GIVEN] An active loop region inside the track
    setLoopRegion(0.1, 0.2);

    //! [THEN] The stream covers the loop region; the mixer runs to the project end
    EXPECT_CALL(*m_audioEngine, startStream(_,
                                            DoubleNear(0.1, TIME_TOLERANCE),
                                            DoubleNear(0.2, TIME_TOLERANCE),
                                            DoubleNear(TRACK_DURATION, TIME_TOLERANCE), _, _))
    .WillOnce(Return(1));

    //! [WHEN] Play
    m_player->play();

    //! [THEN] The player is running
    EXPECT_EQ(m_player->playbackStatus(), PlaybackStatus::Running);
}

/**
 * @brief Play with an explicit start time forwards it as the stream start
 * @details With an active loop region the stream bounds are the loop region, but the
 *          playhead must start where the caller asked (issue #11074 mechanism)
 */
TEST_F(Au3PlayerTests, Play_WithExplicitStartTime_ForwardsStreamStartTime)
{
    //! [GIVEN] An active loop region inside the track
    setLoopRegion(0.1, 0.2);

    //! [THEN] The stream covers the loop region, but starts at the requested time
    EXPECT_CALL(*m_audioEngine, startStream(_,
                                            DoubleNear(0.1, TIME_TOLERANCE),
                                            DoubleNear(0.2, TIME_TOLERANCE),
                                            _, _,
                                            testing::Field(&audio::IAudioEngine::StartStreamOptions::streamStartTime,
                                                           testing::Optional(DoubleNear(0.05, TIME_TOLERANCE)))))
    .WillOnce(Return(1));

    //! [WHEN] Play from 0.05 secs
    m_player->play(muse::secs_t(0.05));
}

/**
 * @brief Play with an inactive play region streams from the cursor to the track end
 */
TEST_F(Au3PlayerTests, Play_WithInactivePlayRegion_StreamsFromCursorToTrackEnd)
{
    //! [GIVEN] The cursor is at 0.1 secs (an inactive, empty play region)
    m_player->seek(muse::secs_t(0.1));

    //! [THEN] The stream runs from the cursor to the track end
    EXPECT_CALL(*m_audioEngine, startStream(_,
                                            DoubleNear(0.1, TIME_TOLERANCE),
                                            DoubleNear(TRACK_DURATION, TIME_TOLERANCE),
                                            DoubleNear(TRACK_DURATION, TIME_TOLERANCE), _, _))
    .WillOnce(Return(1));

    //! [WHEN] Play
    m_player->play();
}

/**
 * @brief playRange streams the exact range once, ignoring an active loop region
 * @details The mechanism behind "Play the selected time range" with looping enabled
 *          (issue #9393): a non-default policy stream bounded by the range itself
 */
TEST_F(Au3PlayerTests, PlayRange_WithActiveLoopRegion_StreamsExactRangeOnce)
{
    //! [GIVEN] An active loop region elsewhere in the track
    setLoopRegion(0.3, 0.4);

    //! [THEN] The stream covers exactly the requested range with a non-default
    //! (one-shot) policy — the loop region does not leak into the bounds
    EXPECT_CALL(*m_audioEngine, startStream(_,
                                            DoubleNear(0.1, TIME_TOLERANCE),
                                            DoubleNear(0.2, TIME_TOLERANCE),
                                            DoubleNear(0.2, TIME_TOLERANCE),
                                            _,
                                            testing::Field(&audio::IAudioEngine::StartStreamOptions::isDefaultPolicy,
                                                           false)))
    .WillOnce(Return(1));

    //! [WHEN] Play the range 0.1-0.2 secs
    m_player->playRange({ muse::secs_t(0.1), muse::secs_t(0.2) });

    //! [THEN] The player is running
    EXPECT_EQ(m_player->playbackStatus(), PlaybackStatus::Running);
}

/**
 * @brief Seek while a loop region is active does not touch the play region
 */
TEST_F(Au3PlayerTests, Seek_WhileLoopRegionActive_DoesNotChangePlayRegion)
{
    //! [GIVEN] An active loop region
    setLoopRegion(0.1, 0.2);

    //! [WHEN] Seek outside of it
    m_player->seek(muse::secs_t(0.05));

    //! [THEN] The play region is unchanged, only the position moved
    const auto& playRegion = ViewInfo::Get(projectRef()).playRegion;
    EXPECT_NEAR(playRegion.GetStart(), 0.1, TIME_TOLERANCE);
    EXPECT_NEAR(playRegion.GetEnd(), 0.2, TIME_TOLERANCE);
    EXPECT_NEAR(m_player->playbackPosition().to_double(), 0.05, TIME_TOLERANCE);
}

/**
 * @brief A stream that went inactive while running flips the status to stopped
 * @details This is the drain-detection seam of updateStreamState(), reached through
 *          the public updatePlaybackPosition()
 */
TEST_F(Au3PlayerTests, UpdatePlaybackPosition_WhenStreamInactive_WhileRunning_Stops)
{
    //! [GIVEN] Playback is running
    m_player->play();
    ASSERT_EQ(m_player->playbackStatus(), PlaybackStatus::Running);

    //! [GIVEN] The stream has drained on its own
    ON_CALL(*m_audioEngine, isStreamActive(_))
    .WillByDefault(Return(false));

    //! [THEN] The engine stream is fully stopped, releasing the stream token;
    //! otherwise isBusy() would stay true and no new playback could ever start
    EXPECT_CALL(*m_audioEngine, stopStream())
    .Times(1);

    //! [WHEN] The position poll runs
    m_player->updatePlaybackPosition();

    //! [THEN] The player is stopped
    EXPECT_EQ(m_player->playbackStatus(), PlaybackStatus::Stopped);
}

/**
 * @brief A stream that went inactive while paused only updates the status
 * @details E.g. a device change tore the stream down while paused — that flow owns
 *          the stream lifecycle, so no full stop must be issued
 */
TEST_F(Au3PlayerTests, UpdatePlaybackPosition_WhenStreamInactive_WhilePaused_DoesNotStopStream)
{
    //! [GIVEN] Playback is running, then paused
    m_player->play();
    ASSERT_EQ(m_player->playbackStatus(), PlaybackStatus::Running);
    m_player->pause();
    ASSERT_EQ(m_player->playbackStatus(), PlaybackStatus::Paused);

    //! [GIVEN] The stream went inactive
    ON_CALL(*m_audioEngine, isStreamActive(_))
    .WillByDefault(Return(false));

    //! [THEN] No full stop is issued
    EXPECT_CALL(*m_audioEngine, stopStream())
    .Times(0);

    //! [WHEN] The position poll runs
    m_player->updatePlaybackPosition();

    //! [THEN] Only the status is updated
    EXPECT_EQ(m_player->playbackStatus(), PlaybackStatus::Stopped);
}

/**
 * @brief The position poll reads the playback time from the audio engine
 */
TEST_F(Au3PlayerTests, UpdatePlaybackPosition_UsesEngineStreamTime)
{
    //! [GIVEN] Playback is running
    m_player->play();
    ASSERT_EQ(m_player->playbackStatus(), PlaybackStatus::Running);

    //! [GIVEN] The stream is active at 0.123 secs
    ON_CALL(*m_audioEngine, isStreamActive(_))
    .WillByDefault(Return(true));
    ON_CALL(*m_audioEngine, streamTime())
    .WillByDefault(Return(0.123));

    //! [WHEN] The position poll runs
    m_player->updatePlaybackPosition();

    //! [THEN] The playback position reflects the engine's stream time
    EXPECT_NEAR(m_player->playbackPosition().to_double(), 0.123, TIME_TOLERANCE);
}
}
