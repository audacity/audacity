/*
* Audacity: A Digital Audio Editor
*/

#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "effects/effects_base/internal/realtimeeffectservice.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "project/tests/mocks/dummyeffectinstancefactory.h"

#include "testing/testcontext.h"

#include "au3-project/Project.h"
#include "au3-track/Track.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-realtime-effects/RealtimeEffectList.h"
#include "au3-realtime-effects/RealtimeEffectState.h"

using namespace ::testing;

namespace au::effects {
class Effects_RealtimeEffectServiceTests : public ::testing::Test, public muse::async::Asyncable
{
protected:
    void SetUp() override
    {
        m_ctx = testutils::makeTestContext();

        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        muse::modularity::ioc(m_ctx)->registerExport<context::IGlobalContext>("utests", m_globalContext);

        m_au3Project = ::AudacityProject::Create();
        m_project = std::make_shared<NiceMock<project::AudacityProjectMock> >();

        ON_CALL(*m_project, au3ProjectPtr())
        .WillByDefault(Return(reinterpret_cast<uintptr_t>(m_au3Project.get())));
        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_project));
        ON_CALL(*m_globalContext, currentProjectChanged())
        .WillByDefault(Return(m_currentProjectChanged));

        m_service = std::make_shared<RealtimeEffectService>(m_ctx);
    }

    void TearDown() override
    {
        m_service.reset();
        muse::modularity::ioc(m_ctx)->unregister<context::IGlobalContext>("utests");
    }

    muse::modularity::ContextPtr m_ctx;
    std::shared_ptr<NiceMock<context::GlobalContextMock> > m_globalContext;
    std::shared_ptr<NiceMock<project::AudacityProjectMock> > m_project;
    std::shared_ptr<::AudacityProject> m_au3Project;
    muse::async::Notification m_currentProjectChanged;
    std::shared_ptr<RealtimeEffectService> m_service;
};

TEST_F(Effects_RealtimeEffectServiceTests, EffectStackGoneOnTrackDeletionWhileStatesStillAlive)
{
    RealtimeEffectState::EffectFactory::Scope factoryScope {
        [](const PluginID&) -> const EffectInstanceFactory* { return &project::dummyFactory(); }
    };

    // A track with one realtime effect, as after loading a project.
    auto& trackList = ::TrackList::Get(*m_au3Project);
    auto waveTrack = ::WaveTrackFactory::Get(*m_au3Project).Create();
    trackList.Add(waveTrack);
    const TrackId trackId = waveTrack->GetId();

    auto state = RealtimeEffectState::make_shared(wxString::FromUTF8("au-test:fake-effect"));
    const std::weak_ptr<RealtimeEffectState> weakState = state;
    ASSERT_TRUE(RealtimeEffectList::Get(*waveTrack).AddState(state));

    // The track (via its effect list) must be the sole owner of the state.
    state.reset();
    waveTrack.reset();

    m_service->init();

    ASSERT_TRUE(m_service->effectStack(trackId).has_value());
    ASSERT_EQ(m_service->effectStack(trackId)->size(), 1u);
    ASSERT_TRUE(m_service->effectStack(IRealtimeEffectService::masterTrackId).has_value());

    // UI models re-read the stack from within the stack-changed notification.
    bool notified = false;
    bool stackGoneDuringNotification = false;
    bool stateAliveDuringNotification = false;
    m_service->realtimeEffectStackChanged().onReceive(this, [&](TrackId id) {
        if (id != trackId) {
            return;
        }
        notified = true;
        stackGoneDuringNotification = !m_service->effectStack(id).has_value();
        stateAliveDuringNotification = !weakState.expired();
    });

    trackList.Clear();

    EXPECT_TRUE(notified);
    EXPECT_TRUE(stackGoneDuringNotification);
    EXPECT_TRUE(stateAliveDuringNotification);

    // After the clear the states are destroyed, and the track's stack stays gone.
    EXPECT_TRUE(weakState.expired());
    EXPECT_FALSE(m_service->effectStack(trackId).has_value());

    // The master list belongs to the project and remains registered.
    EXPECT_TRUE(m_service->effectStack(IRealtimeEffectService::masterTrackId).has_value());
}
}
