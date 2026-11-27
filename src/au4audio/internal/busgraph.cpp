/*
 * Audacity: A Digital Audio Editor
 */
#include "busgraph.h"

#include <algorithm>

using namespace au::au4audio;

namespace {
std::vector<size_t> destinationsOf(const BusDestinations& destinations)
{
    auto result = destinations.sends;
    if (destinations.output) {
        result.push_back(*destinations.output);
    }
    return result;
}
}

BusGraph::BusGraph(std::vector<BusDestinations> destinations, std::vector<int> latencies, int masterLatency)
    : m_destinations{std::move(destinations)}, m_latencies{std::move(latencies)}, m_masterLatency{masterLatency}
{
    const auto numBusses = m_destinations.size();
    // Ignores what isn't a bus
    for (auto& bus : m_destinations) {
        if (bus.output && *bus.output >= numBusses) {
            bus.output.reset();
        }
        bus.sends.erase(std::remove_if(bus.sends.begin(), bus.sends.end(), [&](size_t b) { return b >= numBusses; }),
                        bus.sends.end());
    }
    // Those that go to no bus left, first: as Kahn's, from what nothing goes to
    std::vector<size_t> numInputs(numBusses, 0);
    for (const auto& bus : m_destinations) {
        for (const auto b : destinationsOf(bus)) {
            ++numInputs[b];
        }
    }
    std::vector<bool> ordered(numBusses, false);
    while (m_order.size() < numBusses) {
        std::optional<size_t> next;
        for (size_t b = 0; b < numBusses && !next; ++b) {
            if (!ordered[b] && numInputs[b] == 0) {
                next = b;
            }
        }
        if (!next) {
            // A loop: the first bus left goes nowhere but to the master
            for (size_t b = 0; b < numBusses && !next; ++b) {
                if (!ordered[b]) {
                    next = b;
                }
            }
            for (size_t b = 0; b < numBusses; ++b) {
                if (ordered[b]) {
                    continue;
                }
                auto& bus = m_destinations[b];
                if (bus.output && *bus.output == *next) {
                    bus.output.reset();
                    --numInputs[*next];
                }
                const auto sends = std::count(bus.sends.begin(), bus.sends.end(), *next);
                bus.sends.erase(std::remove(bus.sends.begin(), bus.sends.end(), *next), bus.sends.end());
                numInputs[*next] -= static_cast<size_t>(sends);
            }
            continue;
        }
        ordered[*next] = true;
        m_order.push_back(*next);
        for (const auto b : destinationsOf(m_destinations[*next])) {
            --numInputs[b];
        }
    }
    // From the master up
    m_leads.assign(numBusses, 0);
    for (auto it = m_order.rbegin(); it != m_order.rend(); ++it) {
        m_leads[*it] = lead(m_destinations[*it]);
    }
}

int BusGraph::lead(const BusDestinations& to) const
{
    int result = to.output ? 0 : m_masterLatency;
    for (const auto b : destinationsOf(to)) {
        if (b < m_latencies.size()) {
            result = std::max(result, m_latencies[b] + m_leads[b]);
        }
    }
    return result;
}

std::vector<size_t> BusGraph::reachable(const BusDestinations& to) const
{
    std::vector<bool> reached(m_destinations.size(), false);
    std::vector<size_t> next = destinationsOf(to);
    while (!next.empty()) {
        const auto b = next.back();
        next.pop_back();
        if (b >= reached.size() || reached[b]) {
            continue;
        }
        reached[b] = true;
        for (const auto after : destinationsOf(m_destinations[b])) {
            next.push_back(after);
        }
    }
    std::vector<size_t> result;
    for (size_t b = 0; b < reached.size(); ++b) {
        if (reached[b]) {
            result.push_back(b);
        }
    }
    return result;
}
