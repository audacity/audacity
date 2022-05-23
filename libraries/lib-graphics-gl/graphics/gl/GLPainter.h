/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  GLPainter.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "graphics/Painter.h"

namespace graphics::gl
{

class Context;
class GLRenderer;

class PaintTarget;
class PaintTargetsStack;

class GLPainter final : public Painter
{
public:
   GLPainter(GLRenderer& renderer, Context& context, const FontInfo& defaultFont);
   GLPainter(GLRenderer& renderer, std::unique_ptr<Context> context, const FontInfo& defaultFont);

   ~GLPainter();
   
   RendererID GetRendererID() const override;

   Size GetSize() const override;

   std::shared_ptr<PainterFont> CreateFont(const FontInfo& fontInfo) override;

   std::shared_ptr<PainterFont> GetDefaultFont() const override;

   std::shared_ptr<PainterPath> CreatePath() override;

   void DrawPath(const PainterPath& path) override;

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
      const PainterFont& font, const std::string_view& text) const override;

   void PushPaintTarget(const std::shared_ptr<PainterImage>& image) override;

   void PopPaintTarget(const std::shared_ptr<PainterImage>& image) override;

private:
   GLRenderer& mRenderer;
   Context& mContext;

   std::unique_ptr<Context> mOwnedContext;
   
   std::shared_ptr<PainterFont> mDefaultFont;

   std::unique_ptr<PaintTargetsStack> mTargetsStack;
   PaintTarget* mCurrentPaintTarget { nullptr };

   Transform mCurrentTransform;

   Brush mCurrentBrush;
   Pen mCurrentPen;

   bool mInPaint { false };
}; // class GLPainter

} // namespace graphics::gl
