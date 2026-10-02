/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  PlaybackTempoScale.h

  Operation-scoped playback tempo scale used by Play-at-Speed when
  "Preserve pitch" is enabled. DefaultPlaybackPolicy writes the project's
  shared scale; ClipSegment reads the scale owned by its StretchingSequence
  so StaffPad time-stretches. Offline Create() paths use an isolated 1.0
  scale so concurrent preserve-pitch playback cannot affect export/render.

**********************************************************************/
#pragma once

#include <atomic>
#include <memory>

namespace PlaybackTempoScale {
using Ptr = std::shared_ptr<std::atomic<double> >;

//! Create an operation-owned scale (default 1.0 = no extra tempo change).
inline Ptr Create(double scale = 1.0)
{
    if (!(scale > 0.0)) {
        scale = 1.0;
    }
    return std::make_shared<std::atomic<double> >(scale);
}

inline double Get(const Ptr& scale)
{
    if (!scale) {
        return 1.0;
    }
    return scale->load(std::memory_order_relaxed);
}

inline void Set(const Ptr& scale, double value)
{
    if (!scale) {
        return;
    }
    if (!(value > 0.0)) {
        value = 1.0;
    }
    scale->store(value, std::memory_order_relaxed);
}
}
