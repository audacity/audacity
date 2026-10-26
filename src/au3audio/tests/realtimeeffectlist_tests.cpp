/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <atomic>
#include <chrono>
#include <thread>

#include "au3-realtime-effects/RealtimeEffectList.h"
#include "au3-realtime-effects/RealtimeEffectState.h"

namespace au::au3audio {
namespace {
void addState(RealtimeEffectList& list)
{
    // The XML path adds a state without needing a loadable effect
    list.HandleXMLChild(RealtimeEffectState::XMLTag());
}
}

// The audio thread processes effects inside Visit; a list change on the main
// thread must not free what the visitor is iterating
TEST(RealtimeEffectListTests, ChangeWaitsUntilVisitorHasLeft)
{
    using namespace std::chrono_literals;

    RealtimeEffectList list;
    addState(list);
    addState(list);

    std::atomic<bool> visiting = false;
    std::atomic<bool> changeDone = false;
    std::atomic<bool> changeDoneWhileVisiting = false;

    std::thread visitor([&] {
        bool first = true;
        list.Visit([&](RealtimeEffectState&, bool) {
            if (first) {
                first = false;
                visiting = true;
                std::this_thread::sleep_for(100ms);
                changeDoneWhileVisiting = changeDone.load();
            }
        });
    });

    while (!visiting) {
        std::this_thread::yield();
    }
    list.Clear();
    changeDone = true;
    visitor.join();

    EXPECT_FALSE(changeDoneWhileVisiting);
}

TEST(RealtimeEffectListTests, VisitSeesTheChangedList)
{
    RealtimeEffectList list;
    addState(list);
    addState(list);

    size_t count = 0;
    list.Visit([&](RealtimeEffectState&, bool) { ++count; });
    EXPECT_EQ(count, 2u);

    list.Clear();
    count = 0;
    list.Visit([&](RealtimeEffectState&, bool) { ++count; });
    EXPECT_EQ(count, 0u);
}
}
