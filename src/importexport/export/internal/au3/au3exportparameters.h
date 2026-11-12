/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/types/retval.h"

#include "../../iexporter.h"
#include "../../types/exportformatsettings.h"

namespace au::importexport {
muse::RetVal<ExportParameters> au3ExportParameters(const ExportFormatSettings& settings, int sampleRate);
}
