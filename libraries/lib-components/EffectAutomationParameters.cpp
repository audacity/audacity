/**********************************************************************

  Audacity: A Digital Audio Editor

  @file EffectAutomationParameters.cpp

**********************************************************************/
#include "EffectAutomationParameters.h"

#include "ToChars.h"

#include <cassert>
#include <cmath>

namespace {
template<typename Float> wxString DoFormatValue(Float value)
{
   // Not handled by ToChars
   if (!std::isfinite(value))
      return wxString::Format(wxT("%g"), static_cast<double>(value));

   // The longest results are like -2.2250738585072014e-308
   char buffer[32];
   const auto result = ToChars(buffer, buffer + sizeof(buffer), value);
   assert(result.ec == std::errc());
   return wxString::FromUTF8(buffer, result.ptr - buffer);
}
}

CommandParameters::~CommandParameters() = default;

wxString CommandParameters::FormatValue(double value)
{
   // A subnormal value would be rejected when read back
   if (std::fpclassify(value) == FP_SUBNORMAL)
      value = 0;
   return DoFormatValue(value);
}

wxString CommandParameters::FormatValue(float value)
{
   return DoFormatValue(value);
}
