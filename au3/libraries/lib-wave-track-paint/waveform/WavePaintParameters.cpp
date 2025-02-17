/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WavePaintParameters.cpp

  Dmitry Vedenko

**********************************************************************/
#include "WavePaintParameters.h"

WavePaintParameters& WavePaintParameters::SetDisplayParameters(
    int height, double zoomMin, double zoomMax, bool showClipping) noexcept
{
    Height = height;
    Min = zoomMin;
    Max = zoomMax;

    ShowClipping = showClipping;

    return *this;
}

WavePaintParameters&
WavePaintParameters::SetDBParameters(double dbRange, bool dbScale) noexcept
{
    DBRange = dbRange;
    DBScale = dbScale;

    return *this;
}

WavePaintParameters&
WavePaintParameters::SetBlankColor(graphics::Color color) noexcept
{
    BlankColor = color;

    return *this;
}

WavePaintParameters& WavePaintParameters::SetZeroLineColor(graphics::Color color) noexcept
{
    ZeroLineColor = color;
    return *this;
}

WavePaintParameters& WavePaintParameters::SetBackgroundColors(
    graphics::Color normal, graphics::Color selected) noexcept
{
    BackgroundColors = { normal, selected };
    return *this;
}

WavePaintParameters& WavePaintParameters::SetSampleColors(
    graphics::Color normal, graphics::Color selected) noexcept
{
    SampleColors = { normal, selected };
    return *this;
}

WavePaintParameters& WavePaintParameters::SetRMSColors(
    graphics::Color normal, graphics::Color selected) noexcept
{
    RMSColors = { normal, selected };
    return *this;
}

WavePaintParameters& WavePaintParameters::SetClippingColors(
    graphics::Color normal, graphics::Color selected) noexcept
{
    ClippingColors = { normal, selected };
    return *this;
}

WavePaintParameters&
WavePaintParameters::SetEnvelope(const Envelope& envelope) noexcept
{
    AttachedEnvelope = &envelope;
    return *this;
}

WavePaintParameters& WavePaintParameters::ResetEnvelope() noexcept
{
    AttachedEnvelope = nullptr;
    return *this;
}

WavePaintParameters& WavePaintParameters::SetShowRMS(bool showRMS) noexcept
{
    ShowRMS = showRMS;
    return *this;
}

bool operator==(const ColorPair& lhs, const ColorPair& rhs) noexcept
{
    return lhs.Normal == rhs.Normal && lhs.Selected == rhs.Selected;
}

bool operator!=(const ColorPair& lhs, const ColorPair& rhs) noexcept
{
    return !(lhs == rhs);
}

bool operator==(
    const WavePaintParameters& lhs,
    const WavePaintParameters& rhs) noexcept
{
    return lhs.AttachedEnvelope == rhs.AttachedEnvelope
           && lhs.Height == rhs.Height && lhs.Min == rhs.Min
           && lhs.Max == rhs.Max && lhs.DBRange == rhs.DBRange
           && lhs.DBScale == rhs.DBScale && lhs.ShowClipping == rhs.ShowClipping
           && lhs.ShowRMS == rhs.ShowRMS
           && lhs.BlankColor == rhs.BlankColor
           && lhs.BackgroundColors == rhs.BackgroundColors
           && lhs.SampleColors == rhs.SampleColors
           && lhs.RMSColors == rhs.RMSColors
           && lhs.ClippingColors == rhs.ClippingColors;
}

bool operator!=(
    const WavePaintParameters& lhs,
    const WavePaintParameters& rhs) noexcept
{
    return !(lhs == rhs);
}
