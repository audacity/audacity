/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <optional>
#include <string>

#include "framework/global/types/retval.h"

#include "../../iexporter.h"
#include "../../types/exportformatsettings.h"

namespace au::importexport {
muse::RetVal<ExportParameters> au3ExportParameters(const ExportFormatSettings& settings, int sampleRate);
int au3DefaultSampleRate(const ExportFormatSettings& settings, int projectRate);
std::optional<std::string> au3OptionLabel(const ExportOptionValue& value);
}
