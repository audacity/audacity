/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/serialization/json.h"
#include "framework/global/types/retval.h"

#include "macromanager/macromanagertypes.h"

namespace au::macromanager {
muse::JsonObject toJson(const Macro& macro);
muse::RetVal<Macro> macroFromJson(const muse::JsonObject& object);
}
