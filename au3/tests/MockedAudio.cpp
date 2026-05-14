/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MockedAudio.cpp

  Dmitry Vedenko

**********************************************************************/
#include "MockedAudio.h"

// The audio backend initializes itself per-stream; this fixture has no
// global setup to perform.

MockedAudio::MockedAudio()  = default;
MockedAudio::~MockedAudio() = default;
