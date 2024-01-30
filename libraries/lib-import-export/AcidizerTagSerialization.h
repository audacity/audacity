/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AcidizerTagSerialization.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AcidizerTags.h"

#include <optional>
#include <string>

namespace LibImportExport
{
std::string IMPORT_EXPORT_API
AcidizerTagsToString(const LibFileFormats::AcidizerTags& tags);

std::optional<LibFileFormats::AcidizerTags>
   IMPORT_EXPORT_API StringToAcidizerTags(const std::string& str);
} // namespace LibImportExport
