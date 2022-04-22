/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WXGraphicsContextPainter.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>
#include <vector>

#include "graphics/Painter.h"

class wxWindow;
class wxDC;
class wxGraphicsRenderer;
class wxGraphicsContext;
class wxGraphicsPath;
class wxPoint2DDouble;
class wxFont;

GRAPHICS_WX_API RendererID WXGraphicsContextPainterRendererID();

class GRAPHICS_WX_API WXGraphicsContextPainter final : public Painter
{
public:
   WXGraphicsContextPainter(
      wxGraphicsRenderer* renderer, wxWindow* window, const wxFont& defaultFont);
   WXGraphicsContextPainter(
      wxGraphicsRenderer* renderer, wxDC* dc, const wxFont& defaultFont);
   WXGraphicsContextPainter(
      wxGraphicsRenderer* renderer, const wxFont& defaultFont);

   ~WXGraphicsContextPainter();

   WXGraphicsContextPainter(const WXGraphicsContextPainter&) = delete;
   WXGraphicsContextPainter(WXGraphicsContextPainter&&) = delete;
   WXGraphicsContextPainter& operator =(const WXGraphicsContextPainter&) = delete;
   WXGraphicsContextPainter& operator =(WXGraphicsContextPainter&&) = delete;

   Size GetSize() const override;   

   RendererID GetRendererID() const override;

   std::shared_ptr<PainterFont>
   CreateFont(const std::string_view& faceName, float pixelSize) override;

   std::shared_ptr<PainterFont>
   CreateFontFromWX(const wxFont& font);

   std::unique_ptr<PainterImage> CreateImage(
      PainterImageFormat format, uint32_t width, uint32_t height,
      const void* data = nullptr, const void* alphaData = nullptr) override;

   std::unique_ptr<PainterImage> GetSubImage(
      const PainterImage& image, uint32_t x, uint32_t y, uint32_t width,
      uint32_t height) override;

   std::unique_ptr<PainterImage> CreateDeviceImage(
      PainterImageFormat format, uint32_t width, uint32_t height) override;

   void Flush() override;

private:
   void BeginPaint() override;
   void EndPaint() override;

   void DoClear(const Rect& rect, Color color) override;

   void UpdateBrush(const Brush& brush) override;
   void UpdatePen(const Pen& pen) override;

   void UpdateTransform(const Transform& transform) override;

   void UpdateClipRect(const Rect& rect) override;

   bool UpdateAntiAliasingState(bool enabled) override;

   size_t BeginPath() override;
   void MoveTo(size_t pathIndex, Point pt) override;
   void LineTo(size_t pathIndex, Point pt) override;
   void AddRect(size_t pathIndex, const Rect& rect) override;
   void EndPath(size_t pathIndex) override;

   void DoDrawPolygon(const Point* pts, size_t count) override;
   void DoDrawLines(const Point* ptr, size_t count) override;
   void DoDrawRect(const Rect& rect) override;
   void DoDrawEllipse(const Rect& rect) override;

   void DoDrawText(
      Point origin, const PainterFont& font, Brush backgroundBrush,
      const std::string_view& text) override;

   void DoDrawRotatedText(
      Point origin, float angle, const PainterFont& font, Brush backgroundBrush,
      const std::string_view& text) override;

   void DoDrawImage(
      const PainterImage& image, const Rect& rect,
      const Rect& imageRect) override;

   std::shared_ptr<PainterFont> GetDefaultFont() const override;

   Size DoGetTextSize(
      const PainterFont& font, const std::string_view& text) const override;

   void UpdateFont(const std::shared_ptr<PainterFont>& font) override;

   void DoDrawRoundedRect(const Rect& rect, float radius) override;

   void PushPaintTarget(PainterImage& image) override;
   void PopPaintTarget(PainterImage& image) override;

   class PaintTargetStack;

   std::unique_ptr<PaintTargetStack> mPaintTargetStack;

   std::vector<std::unique_ptr<wxGraphicsPath>> mPaths;
   std::vector<wxPoint2DDouble> mPoints;
   std::vector<wxPoint2DDouble> mEndPoints;
   std::shared_ptr<PainterFont> mDefaultFont;

   // wxGraphicsContext seems to use paths to draw lines,
   // at least on Windows.
   wxGraphicsPath& GetCachedPath();
   void FlushCachedPath();
   
   std::unique_ptr<wxGraphicsPath> mCachedPath;
};
