/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Ports.cpp

  Paul Licameli split from LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2013 Audacity Team.
  License: GPL v2 or later.  See License.txt.

*********************************************************************/

#include "LV2Ports.h"

size_t LV2ControlPort::Discretize(float value) const
{
   auto s = mScaleValues.size();
   for (; s > 0 && --s;)
      if (value >= mScaleValues[s])
         break;
   return s;
}
