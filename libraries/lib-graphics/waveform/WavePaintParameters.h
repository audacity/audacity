/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WavePaintParameters.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "graphics/Color.h"

class Envelope;

struct GRAPHICS_API ColorPair final
{
   graphics::Color Normal;
   graphics::Color Selected;

   friend GRAPHICS_API bool
   operator==(const ColorPair& lhs, const ColorPair& rhs) noexcept;
   friend GRAPHICS_API bool
   operator!=(const ColorPair& lhs, const ColorPair& rhs) noexcept;
};

struct GRAPHICS_API WavePaintParameters final
{
   const Envelope* AttachedEnvelope { nullptr };

   int Height { 0 };

   double Min { -1.0 };
   double Max { 1.0 };

   double DBRange { 60.0 };
   bool DBScale { false };

   bool ShowClipping { false };

   graphics::Color BlankColor;

   ColorPair BackgroundColors;
   ColorPair SampleColors;
   ColorPair RMSColors;
   ColorPair ClippingColors;

   WavePaintParameters& SetDisplayParameters(int height, double zoomMin, double zoomMax, bool showClipping) noexcept;
   WavePaintParameters& SetDBParameters(double dbRange, bool dbScale) noexcept;
   WavePaintParameters& SetBlankColor(graphics::Color color) noexcept;
   WavePaintParameters& SetBackgroundColors(
      graphics::Color normal, graphics::Color selected) noexcept;
   WavePaintParameters&
   SetSampleColors(graphics::Color normal, graphics::Color selected) noexcept;
   WavePaintParameters&
   SetRMSColors(graphics::Color normal, graphics::Color selected) noexcept;
   WavePaintParameters&
   SetClippingColors(graphics::Color normal, graphics::Color selected) noexcept;
   WavePaintParameters& SetEnvelope(const Envelope& envelope) noexcept;
   WavePaintParameters& ResetEnvelope() noexcept;

   friend GRAPHICS_API bool operator==(
      const WavePaintParameters& lhs,
      const WavePaintParameters& rhs) noexcept;

   friend GRAPHICS_API bool operator!=(
      const WavePaintParameters& lhs,
      const WavePaintParameters& rhs) noexcept;
};
