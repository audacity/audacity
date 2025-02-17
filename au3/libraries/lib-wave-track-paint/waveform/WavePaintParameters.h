/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WavePaintParameters.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "graphics/Color.h"

class Envelope;

//! Pair of colors for waveform painting
struct WAVE_TRACK_PAINT_API ColorPair final
{
    //! Color for the normal state
    graphics::Color Normal;
    //! Color for the selected state
    graphics::Color Selected;

    friend WAVE_TRACK_PAINT_API bool
    operator==(const ColorPair& lhs, const ColorPair& rhs) noexcept;
    friend WAVE_TRACK_PAINT_API bool
    operator!=(const ColorPair& lhs, const ColorPair& rhs) noexcept;
};

//! Parameters for the waveform painting
struct WAVE_TRACK_PAINT_API WavePaintParameters final
{
    //! Attached volume envelope, if any
    const Envelope* AttachedEnvelope { nullptr };

    //! Height of the of clip on screen
    int Height { 0 };

    //! Min value used to clip the output
    double Min { -1.0 };
    //! Max value used to clip the output
    double Max { 1.0 };

    //! Decibel range
    double DBRange { 60.0 };
    //! True, if we paint in dB scale
    bool DBScale { false };

    //! True, if we mark clipped values
    bool ShowClipping { false };

    //! True, if we paint RMS values on top of min and max
    bool ShowRMS { false };

    //! Color outside the waveform area
    graphics::Color BlankColor;
    graphics::Color ZeroLineColor;

    //! Waveform background color
    ColorPair BackgroundColors;
    //! Color of the (min, max) line
    ColorPair SampleColors;
    //! Color of the (-rms, +rms) line
    ColorPair RMSColors;
    //! Color for the columns where clipping has occurred
    ColorPair ClippingColors;

    //! Sets the basic painting parameters
    WavePaintParameters& SetDisplayParameters(int height, double zoomMin, double zoomMax, bool showClipping) noexcept;
    //! Sets the dB scale parameters
    WavePaintParameters& SetDBParameters(double dbRange, bool dbScale) noexcept;
    //! Sets the blank color
    WavePaintParameters& SetBlankColor(graphics::Color color) noexcept;
    //! Sets the horizontal zero line color
    WavePaintParameters& SetZeroLineColor(graphics::Color color) noexcept;
    //! Sets the background colors
    WavePaintParameters& SetBackgroundColors(
        graphics::Color normal, graphics::Color selected) noexcept;
    //! Sets the sample colors
    WavePaintParameters&
    SetSampleColors(graphics::Color normal, graphics::Color selected) noexcept;
    //! Sets the RMS colors
    WavePaintParameters&
    SetRMSColors(graphics::Color normal, graphics::Color selected) noexcept;
    //! Sets the clipping colors
    WavePaintParameters&
    SetClippingColors(graphics::Color normal, graphics::Color selected) noexcept;
    //! Sets volume envelope
    WavePaintParameters& SetEnvelope(const Envelope& envelope) noexcept;
    //! Resets the envelope
    WavePaintParameters& ResetEnvelope() noexcept;
    //! Sets the ShowRMS flag
    WavePaintParameters& SetShowRMS(bool showRMS) noexcept;

    friend WAVE_TRACK_PAINT_API bool operator==(
        const WavePaintParameters& lhs, const WavePaintParameters& rhs) noexcept;

    friend WAVE_TRACK_PAINT_API bool operator!=(
        const WavePaintParameters& lhs, const WavePaintParameters& rhs) noexcept;
};
