/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstddef>
#include <optional>
#include <vector>

namespace au::au4audio {
//! Where a track or a bus goes, by the indices of busses: its output, none for the master, and what it sends to
struct BusDestinations {
    std::optional<size_t> output;
    std::vector<size_t> sends;
};

//! The busses, as they go to each other and to the master
/*!
 What closes a loop, which the UI doesn't allow, is dropped: an output then goes to the master.
 */
class BusGraph
{
public:
    //! @param destinations of each bus
    //! @param latencies of each bus's effects
    BusGraph(std::vector<BusDestinations> destinations, std::vector<int> latencies, int masterLatency);

    //! All busses, those that go to a bus before it
    const std::vector<size_t>& order() const { return m_order; }
    const BusDestinations& destinations(size_t bus) const { return m_destinations[bus]; }

    //! How far ahead of what is heard what goes to `to` is read: as far as its longest way to the master needs
    int lead(const BusDestinations& to) const;
    //! The busses that what goes to `to` goes through
    std::vector<size_t> reachable(const BusDestinations& to) const;

private:
    std::vector<BusDestinations> m_destinations;
    const std::vector<int> m_latencies;
    const int m_masterLatency;
    std::vector<size_t> m_order;
    //! Of what each bus outputs: as `lead` of its destinations
    std::vector<int> m_leads;
};
}
