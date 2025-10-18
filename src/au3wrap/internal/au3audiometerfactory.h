/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3audiometer.h"
#include <memory>

namespace au::au3 {
std::shared_ptr<Meter> createAudioMeter();
}
