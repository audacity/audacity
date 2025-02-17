/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MockedAudio.cpp

  Dmitry Vedenko

**********************************************************************/
#include "MockedAudio.h"

#include <portaudio.h>

MockedAudio::MockedAudio()
{
    Pa_Initialize();
}

MockedAudio::~MockedAudio()
{
    Pa_Terminate();
}
