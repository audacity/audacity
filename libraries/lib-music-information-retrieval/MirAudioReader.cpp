/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MirAudioReader.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "MirAudioReader.h"

namespace MIR
{
double MirAudioReader::GetDuration() const
{
   return 1. * GetNumSamples() / GetSampleRate();
}
} // namespace MIR
