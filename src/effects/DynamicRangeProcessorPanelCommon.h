/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorPanelCommon.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <memory>
#include <wx/colour.h>
#include <wx/platinfo.h>

class wxGraphicsContext;
class wxPaintDC;

namespace DynamicRangeProcessorPanel
{
static const wxColour backgroundColor { 200, 210, 255 };
static const wxColour attackColor { 255, 0, 0, 100 };
static const wxColour releaseColor { 190, 120, 255, 100 };
static const wxColour lineColor { 72, 72, 72 };
static const wxColour targetCompressionColor { lineColor };
static constexpr auto targetCompressionLineWidth = 2;
static constexpr auto compressorMeterRangeDb = 12.f;

std::unique_ptr<wxGraphicsContext> MakeGraphicsContext(const wxPaintDC& dc);

wxColor GetColorMix(const wxColor& a, const wxColor& b, double aWeight);
} // namespace DynamicRangeProcessorPanel
