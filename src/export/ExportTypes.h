/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportTypes.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include <variant>
#include <string>
#include <vector>
#include "Internat.h"

using ExportOptionID = int;

using ExportValue = std::variant<
      bool,
      int,
      double,
      std::string>;

struct ExportOption
{
   enum Flags : int
   {
      TypeMask         = 0xff,
      TypeRange        = 1,
      TypeEnum         = 2,

      ReadOnly         = 0x100,
      Hidden           = 0x200,

      Default          = 0
   };
   
   ExportOptionID id;
   TranslatableString title;
   ExportValue defaultValue;
   int flags { Default };
   std::vector<ExportValue> values {};//interpretation depends on type
   TranslatableStrings names {};
};
