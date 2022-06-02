/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DPainter.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <vector>

#include "graphics/Painter.h"
#include "graphics/RendererID.h"
#include "graphics/FontInfo.h"

namespace graphics::d2d
{
class D2DRenderer;
class D2DRenderTarget;

class D2DPainter final : public Painter
{
public:
   D2DPainter(
      D2DRenderer& renderer, std::shared_ptr<D2DRenderTarget> target,
      const FontInfo& defaultFont);

   D2DRenderTarget* GetCurrentRenderTarget() const;

private:
   RendererID GetRendererID() const override;

   Size GetSize() const override;

   std::shared_ptr<PainterFont> CreateFont(const FontInfo& fontInfo) override;

   std::shared_ptr<PainterFont> GetDefaultFont() const override;

   std::shared_ptr<PainterImage> CreateImage(
      PainterImageFormat format, uint32_t width, uint32_t height,
      const void* data = nullptr, const void* alphaData = nullptr) override;

   std::shared_ptr<PainterImage> GetSubImage(
      const std::shared_ptr<PainterImage>& image, uint32_t x, uint32_t y,
      uint32_t width, uint32_t height) override;

   std::shared_ptr<PainterImage> CreateDeviceImage(
      PainterImageFormat format, uint32_t width, uint32_t height) override;

   void Flush() override;

   void BeginPaint() override;

   void EndPaint() override;

   void DoClear(const Rect& rect, Color color) override;

   void UpdateBrush(const Brush& brush) override;

   void UpdatePen(const Pen& pen) override;

   void UpdateTransform(const Transform& transform) override;

   void UpdateClipRect(const Rect& rect) override;

   bool UpdateAntiAliasingState(bool enabled) override;

   void UpdateFont(const std::shared_ptr<PainterFont>& font) override;

   void DoDrawPolygon(const Point* pts, size_t count) override;

   void DoDrawLines(const Point* ptr, size_t count) override;

   void DoDrawRect(const Rect& rect) override;

   void DoDrawRoundedRect(const Rect& rect, float radius) override;

   void DoDrawEllipse(const Rect& rect) override;

   void DoDrawImage(
      const PainterImage& image, const Rect& destRect,
      const Rect& imageRect) override;

   void DoDrawText(
      Point origin, const PainterFont& font, Brush backgroundBrush,
      const std::string_view& text) override;

   void DoDrawRotatedText(
      Point origin, float angle, const PainterFont& font, Brush backgroundBrush,
      const std::string_view& text) override;

   Size DoGetTextSize(
      const PainterFont& font, const std::string_view& text,
      bool gridFitted) const override;

   void PushPaintTarget(const std::shared_ptr<PainterImage>& image) override;

   void PopPaintTarget(const std::shared_ptr<PainterImage>& image) override;

   std::shared_ptr<PainterPath> CreatePath() override;

   void DrawPath(const PainterPath& path) override;

   D2DRenderer& mRenderer;
   FontInfo mDefaultFont;

   std::vector<std::shared_ptr<D2DRenderTarget>> mRenderTargetStack;
};

} // namespace graphics::d2d
