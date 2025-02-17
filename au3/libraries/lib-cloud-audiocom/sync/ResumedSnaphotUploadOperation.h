/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ResumedSnaphotUploadOperation.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <functional>

enum class AudiocomTrace;

namespace audacity::cloud::audiocom::sync {
class ProjectCloudExtension;

void CLOUD_AUDIOCOM_API ResumeProjectUpload(
    ProjectCloudExtension& projectCloudExtension, std::function<void(AudiocomTrace)> onBeforeUploadStarts);
} // namespace audacity::cloud::audiocom::sync
