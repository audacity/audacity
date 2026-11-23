/*
* Audacity: A Digital Audio Editor
*/

#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "../internal/au3/au3audioinput.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "project/tests/testtools.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"
#include "trackedit/tests/mocks/tracknavigationcontrollermock.h"

#include "au3audio/tests/mocks/audioenginemock.h"
#include "audio/tests/mocks/audiodrivercontrollermock.h"
#include "playback/tests/mocks/playbackcontrollermock.h"
#include "mocks/recordconfigurationmock.h"
#include "mocks/recordcontrollermock.h"
#include "mocks/recordmetercontrollermock.h"

#include "au3wrap/internal/au3project.h"
#include "au3wrap/au3types.h"

#include "au3-track/PendingTracks.h"
#include "au3-wave-track/WaveTrack.h"

using ::testing::_;
using ::testing::Invoke;
using ::testing::NiceMock;
using ::testing::Return;

using namespace au::au3;

namespace au::record {
namespace {
const std::string MODULE_NAME("au3audioinput_tests");
constexpr double SAMPLE_RATE = 44100.0;
}

class Au3AudioInputTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_context = std::make_shared<muse::modularity::Context>(301);

        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_currentProject = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        m_trackeditProject = std::make_shared<NiceMock<trackedit::TrackeditProjectMock> >();
        m_playbackController = std::make_shared<NiceMock<playback::PlaybackControllerMock> >();
        m_recordController = std::make_shared<NiceMock<RecordControllerMock> >();
        m_selectionController = std::make_shared<NiceMock<trackedit::SelectionControllerMock> >();
        m_trackNavigationController = std::make_shared<NiceMock<trackedit::TrackNavigationControllerMock> >();
        m_recordConfiguration = std::make_shared<NiceMock<RecordConfigurationMock> >();
        m_audioEngine = std::make_shared<NiceMock<audio::AudioEngineMock> >();
        m_meterController = std::make_shared<NiceMock<RecordMeterControllerMock> >();
        m_audioDriverController = std::make_shared<NiceMock<audio::AudioDriverControllerMock> >();

        ON_CALL(*m_globalContext, currentProjectChanged()).WillByDefault(Return(m_projectChanged));
        ON_CALL(*m_globalContext, currentProject()).WillByDefault(Return(m_currentProject));
        ON_CALL(*m_globalContext, currentTrackeditProject()).WillByDefault(Return(m_trackeditProject));
        ON_CALL(*m_trackeditProject, trackMoved()).WillByDefault(Return(m_trackMoved));

        ON_CALL(*m_playbackController, isPlaying()).WillByDefault(Invoke([this]() { return m_playing; }));
        ON_CALL(*m_playbackController, isPlayingChanged()).WillByDefault(Return(m_playingChanged));
        ON_CALL(*m_recordController, isRecording()).WillByDefault(Invoke([this]() { return m_recording; }));
        ON_CALL(*m_recordController, isRecordingChanged()).WillByDefault(Return(m_recordingChanged));
        ON_CALL(*m_trackNavigationController, focusedTrack()).WillByDefault(Return(trackedit::INVALID_TRACK));
        ON_CALL(*m_audioEngine, setMonitoredTracks(_)).WillByDefault(Invoke([this](const std::vector<audio::MonitoredTrack>& tracks) {
            m_monitoredTracks = tracks;
            ++m_setMonitoredTracksCalls;
        }));

        ON_CALL(*m_audioDriverController, inputDevices())
        .WillByDefault(Return(std::vector<std::string> { "Test Input Device" }));
        ON_CALL(*m_audioDriverController, inputChannelsAvailable()).WillByDefault(Return(2));
        audio::AudioConfiguration audioConfiguration;
        audioConfiguration.inputChannels = 2;
        ON_CALL(*m_audioDriverController, configuration()).WillByDefault(Return(audioConfiguration));

        auto ioc = muse::modularity::ioc(m_context);
        ioc->registerExport<context::IGlobalContext>(MODULE_NAME, m_globalContext);
        ioc->registerExport<playback::IPlaybackController>(MODULE_NAME, m_playbackController);
        ioc->registerExport<IRecordController>(MODULE_NAME, m_recordController);
        ioc->registerExport<trackedit::ISelectionController>(MODULE_NAME, m_selectionController);
        ioc->registerExport<trackedit::ITrackNavigationController>(MODULE_NAME, m_trackNavigationController);

        auto globalIoc = muse::modularity::globalIoc();
        globalIoc->registerExport<IRecordConfiguration>(MODULE_NAME, m_recordConfiguration);
        globalIoc->registerExport<audio::IAudioEngine>(MODULE_NAME, m_audioEngine);
        globalIoc->registerExport<IRecordMeterController>(MODULE_NAME, m_meterController);
        globalIoc->registerExport<audio::IAudioDriverController>(MODULE_NAME, m_audioDriverController);

        initTestProject();
    }

    void TearDown() override
    {
        m_input.reset();

        if (m_au3ProjectAccessor && m_au3ProjectAccessor->au3ProjectPtr()) {
            PendingTracks::Get(projectRef()).ClearPendingTracks();
            Au3TrackList::Get(projectRef()).Clear();
            m_au3ProjectAccessor->clearSavedState();
            m_au3ProjectAccessor->close();
        }
        testtools::removeProjectIfExists(m_workingProjectPath);

        auto globalIoc = muse::modularity::globalIoc();
        globalIoc->unregister<IRecordConfiguration>(MODULE_NAME);
        globalIoc->unregister<audio::IAudioEngine>(MODULE_NAME);
        globalIoc->unregister<IRecordMeterController>(MODULE_NAME);
        globalIoc->unregister<audio::IAudioDriverController>(MODULE_NAME);
        muse::modularity::removeIoC(m_context);
    }

    void initTestProject()
    {
        m_au3ProjectAccessor = std::make_shared<Au3ProjectAccessor>(muse::modularity::globalCtx());

        const std::string source = std::string(record_tests_DATA_ROOT) + "/../../trackedit/tests/data/empty.aup4";
        m_workingProjectPath = std::string(record_tests_DATA_ROOT) + "/audioinput_working.aup4";
        testtools::removeProjectIfExists(m_workingProjectPath);
        ASSERT_TRUE(testtools::copyFile(source, m_workingProjectPath));

        constexpr auto discardAutosave = false;
        ASSERT_TRUE(m_au3ProjectAccessor->load(muse::io::path_t(m_workingProjectPath), discardAutosave));

        ON_CALL(*m_currentProject, au3ProjectPtr())
        .WillByDefault(Return(m_au3ProjectAccessor->au3ProjectPtr()));
    }

    Au3Project& projectRef() const
    {
        return *reinterpret_cast<Au3Project*>(m_au3ProjectAccessor->au3ProjectPtr());
    }

    Au3WaveTrack* addTrack(size_t channels, bool selected)
    {
        auto track = Au3WaveTrackFactory::Get(projectRef()).Create(channels, sampleFormat::floatSample, SAMPLE_RATE);
        Au3TrackList::Get(projectRef()).Add(track, ::TrackList::DoAssignId::Yes,
                                            ::TrackList::EventPublicationSynchrony::Synchronous);
        track->SetSelected(selected);
        return track.get();
    }

    void openProject()
    {
        m_input = std::make_shared<Au3AudioInput>(m_context);
        m_projectChanged.notify();
    }

    static audio::MonitoredTrack monitored(const Au3WaveTrack* track)
    {
        return { track->GetId(), track->NChannels() };
    }

    using MonitoredTracks = std::vector<audio::MonitoredTrack>;

    std::shared_ptr<Au3AudioInput> m_input;

    muse::modularity::ContextPtr m_context;
    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<project::AudacityProjectMock> m_currentProject;
    std::shared_ptr<trackedit::TrackeditProjectMock> m_trackeditProject;
    std::shared_ptr<playback::PlaybackControllerMock> m_playbackController;
    std::shared_ptr<RecordControllerMock> m_recordController;
    std::shared_ptr<trackedit::SelectionControllerMock> m_selectionController;
    std::shared_ptr<trackedit::TrackNavigationControllerMock> m_trackNavigationController;
    std::shared_ptr<RecordConfigurationMock> m_recordConfiguration;
    std::shared_ptr<audio::AudioEngineMock> m_audioEngine;
    std::shared_ptr<RecordMeterControllerMock> m_meterController;
    std::shared_ptr<audio::AudioDriverControllerMock> m_audioDriverController;

    muse::async::Notification m_projectChanged;
    muse::async::Notification m_playingChanged;
    muse::async::Notification m_recordingChanged;
    muse::async::Channel<trackedit::Track, int> m_trackMoved;
    bool m_playing = false;
    MonitoredTracks m_monitoredTracks;
    int m_setMonitoredTracksCalls = 0;
    bool m_recording = false;

    std::shared_ptr<Au3ProjectAccessor> m_au3ProjectAccessor;
    std::string m_workingProjectPath;
};

TEST_F(Au3AudioInputTests, SelectedTracksAreMonitoredInTrackOrder)
{
    //! [GIVEN] Two selected mono tracks
    Au3WaveTrack* first = addTrack(1, true);
    Au3WaveTrack* second = addTrack(1, true);

    //! [WHEN] The project is opened
    openProject();

    //! [THEN] Both tracks are monitored, in track order
    EXPECT_EQ(m_monitoredTracks, (MonitoredTracks { monitored(first), monitored(second) }));
}

TEST_F(Au3AudioInputTests, FocusedTrackIsMonitoredWhenNoTrackIsSelected)
{
    //! [GIVEN] A focused mono track, an unselected stereo track and a stereo input
    Au3WaveTrack* focused = addTrack(1, false);
    addTrack(2, false);
    ON_CALL(*m_trackNavigationController, focusedTrack()).WillByDefault(Return(trackedit::TrackId(focused->GetId())));

    //! [WHEN] The project is opened
    openProject();

    //! [THEN] Only the focused track is monitored
    EXPECT_EQ(m_monitoredTracks, (MonitoredTracks { monitored(focused) }));
}

TEST_F(Au3AudioInputTests, MovingTracksChangesTheirMonitoringOrder)
{
    //! [GIVEN] Two selected mono tracks
    Au3WaveTrack* first = addTrack(1, true);
    Au3WaveTrack* second = addTrack(1, true);
    openProject();

    //! [WHEN] The second track is moved above the first one
    Au3TrackList::Get(projectRef()).MoveUp(*second);
    m_trackMoved.send(trackedit::Track {}, 0);

    //! [THEN] The monitoring order follows the new track order
    EXPECT_EQ(m_monitoredTracks, (MonitoredTracks { monitored(second), monitored(first) }));
}

TEST_F(Au3AudioInputTests, UnchangedTracksAreNotSentAgain)
{
    //! [GIVEN] A monitored track
    addTrack(1, true);
    openProject();
    const int calls = m_setMonitoredTracksCalls;

    //! [WHEN] A track event leaves the tracks to record into unchanged
    m_trackMoved.send(trackedit::Track {}, 0);

    //! [THEN] The audio engine is not updated again
    EXPECT_EQ(m_setMonitoredTracksCalls, calls);
}

TEST_F(Au3AudioInputTests, NoTrackIsMonitoredWhilePlaying)
{
    //! [GIVEN] A selected track being monitored
    Au3WaveTrack* track = addTrack(1, true);
    openProject();
    ASSERT_FALSE(m_monitoredTracks.empty());

    //! [WHEN] Playback starts
    m_playing = true;
    m_playingChanged.notify();

    //! [THEN] No track is monitored
    EXPECT_TRUE(m_monitoredTracks.empty());

    //! [WHEN] Playback stops
    m_playing = false;
    m_playingChanged.notify();

    //! [THEN] The track is monitored again
    EXPECT_EQ(m_monitoredTracks, (MonitoredTracks { monitored(track) }));
}

TEST_F(Au3AudioInputTests, NoTrackIsMonitoredWhileRecording)
{
    //! [GIVEN] A selected track being monitored
    addTrack(1, true);
    openProject();
    ASSERT_FALSE(m_monitoredTracks.empty());

    //! [WHEN] Recording starts
    m_recording = true;
    m_recordingChanged.notify();

    //! [THEN] No track is monitored
    EXPECT_TRUE(m_monitoredTracks.empty());
}
}
