/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <algorithm>

#include "../internal/busgraph.h"

using namespace au::au4audio;

namespace {
constexpr int TEST_MASTER_LATENCY = 64;
constexpr int TEST_SHORT_LATENCY = 16;
constexpr int TEST_LONG_LATENCY = 256;

size_t positionOf(const BusGraph& graph, size_t bus)
{
    const auto& order = graph.order();
    return static_cast<size_t>(std::find(order.begin(), order.end(), bus) - order.begin());
}
}

TEST(BusGraphTests, OrdersABusAfterWhatGoesOrIsSentToIt)
{
    constexpr size_t group = 0;
    constexpr size_t submix = 1;
    constexpr size_t sender = 2;
    std::vector<BusDestinations> destinations(3);
    destinations[group].output = submix;
    destinations[sender].sends = { group };

    const BusGraph graph { destinations, std::vector<int>(destinations.size(), 0), TEST_MASTER_LATENCY };

    EXPECT_LT(positionOf(graph, sender), positionOf(graph, group));
    EXPECT_LT(positionOf(graph, group), positionOf(graph, submix));
}

TEST(BusGraphTests, ReadsWhatGoesToBussesAheadByItsLongestWayToTheMaster)
{
    constexpr size_t shortBus = 0;
    constexpr size_t longBus = 1;
    const BusGraph graph { std::vector<BusDestinations>(2), { TEST_SHORT_LATENCY, TEST_LONG_LATENCY }, TEST_MASTER_LATENCY };

    EXPECT_EQ(graph.lead({}), TEST_MASTER_LATENCY);
    EXPECT_EQ(graph.lead({ shortBus, {} }), TEST_MASTER_LATENCY + TEST_SHORT_LATENCY);
    EXPECT_EQ(graph.lead({ shortBus, { longBus } }), TEST_MASTER_LATENCY + TEST_LONG_LATENCY);
}

TEST(BusGraphTests, AddsTheLatenciesOfTheBussesOnTheWay)
{
    constexpr size_t group = 0;
    constexpr size_t submix = 1;
    std::vector<BusDestinations> destinations(2);
    destinations[group].output = submix;

    const BusGraph graph { destinations, { TEST_SHORT_LATENCY, TEST_LONG_LATENCY }, TEST_MASTER_LATENCY };

    EXPECT_EQ(graph.lead({ group, {} }), TEST_MASTER_LATENCY + TEST_LONG_LATENCY + TEST_SHORT_LATENCY);
}

TEST(BusGraphTests, ReachesTheBussesOnTheWayByOutputsAndSends)
{
    constexpr size_t group = 0;
    constexpr size_t submix = 1;
    constexpr size_t reverb = 2;
    constexpr size_t unused = 3;
    std::vector<BusDestinations> destinations(4);
    destinations[group].output = submix;

    const BusGraph graph { destinations, std::vector<int>(destinations.size(), 0), TEST_MASTER_LATENCY };

    EXPECT_EQ(graph.reachable({ group, { reverb } }), (std::vector<size_t> { group, submix, reverb }));
    EXPECT_EQ(std::count(graph.order().begin(), graph.order().end(), unused), 1);
}

TEST(BusGraphTests, SendsALoopToTheMaster)
{
    constexpr size_t first = 0;
    constexpr size_t second = 1;
    std::vector<BusDestinations> destinations(2);
    destinations[first].output = second;
    destinations[second].output = first;

    const BusGraph graph { destinations, std::vector<int>(destinations.size(), 0), TEST_MASTER_LATENCY };

    EXPECT_EQ(graph.order().size(), destinations.size());
    EXPECT_TRUE(!graph.destinations(first).output || !graph.destinations(second).output);
}
