/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorPanelCommon.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <memory>
#include <wx/colour.h>
#include <wx/graphics.h>

class wxGraphicsContext;
class wxPaintDC;
class wxPanelWrapper;

namespace DynamicRangeProcessorPanel {
static const wxColour backgroundColor { 43, 43, 75 };
static const wxColour attackColor { 255, 0, 0, 100 };
static const wxColour releaseColor { 190, 120, 255, 100 };
static const wxColour lineColor { 72, 72, 72 };
static const wxColour targetCompressionColor { 255, 255, 255 };
static const wxColour inputColor { 86, 86, 149 };
static const wxColour outputColor { 182, 182, 244, 70 };
static const wxColour actualCompressionColor { 252, 220, 151 };

static constexpr auto targetCompressionLineWidth = 2;
static constexpr auto outputLineWidth = 2;
static constexpr auto graphMinHeight = 270;
static constexpr auto graphMinRangeDb = 20.f;

std::unique_ptr<wxGraphicsContext> MakeGraphicsContext(const wxPaintDC& dc);

wxColor GetColorMix(const wxColor& a, const wxColor& b, double aWeight);

float GetGraphDbRange(int height);

wxGraphicsBrush GetGraphBackgroundBrush(wxGraphicsContext& gc, int height);

wxRect GetPanelRect(const wxPanelWrapper& panel);
} // namespace DynamicRangeProcessorPanel
