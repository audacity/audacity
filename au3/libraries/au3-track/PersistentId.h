/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstdint>
#include <random>

//! Identifies a track or clip across sessions, processes and machines, unlike
//! the in-session TrackId and WaveClip::GetId().
//! Random, so that ids created independently (e.g. by collaborators) don't
//! collide; below 2^53 so that they survive a round trip through a double
//! (JavaScript, QML).
using PersistentId = int64_t;

inline PersistentId NewPersistentId()
{
    static thread_local std::mt19937_64 generator{ std::random_device {}() };
    std::uniform_int_distribution<PersistentId> distribution(1, (PersistentId { 1 } << 53) - 1);
    return distribution(generator);
}
