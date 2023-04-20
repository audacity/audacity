/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportProgressUI.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include <future>

#include "ExportTypes.h"

class ExportPluginDelegate;

namespace ExportProgressUI
{
   ExportResult Show(ExportTask exportTask);
}
