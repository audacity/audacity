/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorPanelCommon.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "DynamicRangeProcessorPanelCommon.h"
#include <wx/dcclient.h>
#include <wx/graphics.h>
#include <wx/pen.h>

namespace DynamicRangeProcessorPanel
{
std::unique_ptr<wxGraphicsContext> MakeGraphicsContext(const wxPaintDC& dc)
{
   auto gc = wxGraphicsContext::Create(dc);
   gc->SetAntialiasMode(wxANTIALIAS_DEFAULT);
   gc->SetInterpolationQuality(wxINTERPOLATION_BEST);
   return std::unique_ptr<wxGraphicsContext>(gc);
}
} // namespace DynamicRangeProcessorPanel
