/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "trackedit/dom/clip.h"

namespace au::trackedit::utils {
std::vector<const Clip*> clipSetDifference(const Clips& set1, const Clips& set2);
}
