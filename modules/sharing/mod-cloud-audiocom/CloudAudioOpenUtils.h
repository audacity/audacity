/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudAudioOpenUtils.h

  Dmitry Vedenko / Dmitry Makarenko

**********************************************************************/
#pragma once

#include <string_view>

class AudacityProject;

namespace audacity::cloud::audiocom::sync
{

AudacityProject* OpenAudioFromCloud(std::string_view audioId);

bool HandleAudioLink(std::string_view link);

} // namespace audacity::cloud::audiocom::sync
