/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorPanelCommon.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <memory>
#include <wx/colour.h>

class wxGraphicsContext;
class wxPaintDC;

namespace DynamicRangeProcessorPanel
{
static const wxColour backgroundColor { 229, 234, 255 };
static const wxColour attackColor { 255, 0, 0, 100 };
static const wxColour releaseColor { 190, 120, 255, 100 };

std::unique_ptr<wxGraphicsContext> MakeGraphicsContext(const wxPaintDC& dc);
} // namespace DynamicRangeProcessorPanel
