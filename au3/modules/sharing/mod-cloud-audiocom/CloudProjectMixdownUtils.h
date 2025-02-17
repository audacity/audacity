/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectMixdownUtils.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <functional>
#include <string_view>

class AudacityProject;

namespace audacity::cloud::audiocom::sync {
class UploadUrls;
enum class MixdownState : uint32_t;

bool HandleMixdownLink(std::string_view link);

void UploadMixdown(
    AudacityProject& project, std::function<void(AudacityProject&, MixdownState)> onComplete);
} // namespace audacity::cloud::audiocom::sync
