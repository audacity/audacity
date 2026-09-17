/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "project/tests/mocks/dummyeffectinstancefactory.h"

#include "au3-project/Project.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-wave-track/WaveTrackUtilities.h"
#include "au3-realtime-effects/RealtimeEffectList.h"
#include "au3-realtime-effects/RealtimeEffectState.h"

namespace au::effects {
TEST(Effects_RealtimeEffectsCopyTests, EmptyCopyRefSharesStatesAndDeepCopiesThem)
{
    RealtimeEffectState::EffectFactory::Scope factoryScope {
        [](const PluginID&) -> const EffectInstanceFactory* { return &project::dummyFactory(); }
    };
    const auto project = ::AudacityProject::Create();
    const auto track = ::WaveTrackFactory::Get(*project).Create();
    const auto state = RealtimeEffectState::make_shared(wxString::FromUTF8("au-test:effect"));
    ASSERT_TRUE(RealtimeEffectList::Get(*track).AddState(state));
    RealtimeEffectList::Get(*track).SetActive(false);

    const auto ref = WaveTrackUtilities::EmptyCopy(*track, WaveTrackUtilities::RealtimeEffectsCopy::Ref);
    EXPECT_EQ(RealtimeEffectList::Get(*ref).GetStateAt(0), state);
    EXPECT_FALSE(RealtimeEffectList::Get(*ref).IsActive());

    const auto deep = WaveTrackUtilities::EmptyCopy(*track, 1, WaveTrackUtilities::RealtimeEffectsCopy::Deep);
    const auto copy = RealtimeEffectList::Get(*deep).GetStateAt(0);
    ASSERT_NE(copy, nullptr);
    EXPECT_NE(copy, state);
    EXPECT_EQ(copy->GetID(), state->GetID());
    EXPECT_FALSE(RealtimeEffectList::Get(*deep).IsActive());
    EXPECT_EQ(deep->NChannels(), 1u);
}
}
