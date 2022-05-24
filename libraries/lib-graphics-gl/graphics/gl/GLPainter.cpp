/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  GLPainter.cpp

  Dmitry Vedenko

**********************************************************************/
#include "GLPainter.h"

#include <cassert>

#include "graphics/fonts/FontLibrary.h"
#include "graphics/fonts/Font.h"
#include "GLFontRenderer.h"

#include "Context.h"
#include "GLRenderer.h"
#include "Texture.h"

#include "PaintTarget.h"
#include "PaintTargetsStack.h"

#include "Path.h"
#include "StrokeGenerator.h"

namespace graphics::gl
{

extern const RendererID OpenGLRendererID;

GLPainter::GLPainter(
   GLRenderer& renderer, Context& context, const FontInfo& defaultFont)
    : mRenderer(renderer)
    , mContext(context)
    , mDefaultFont(fonts::GetFontLibrary().GetFont(defaultFont, 96))
    , mTargetsStack(std::make_unique<PaintTargetsStack>(renderer, context))
    , mStrokeGenerator(std::make_unique<StrokeGenerator>())
{
   assert(mDefaultFont != nullptr);
}

GLPainter::GLPainter(
   GLRenderer& renderer, std::unique_ptr<Context> context,
   const FontInfo& defaultFont)
    : mRenderer(renderer)
    , mContext(*context)
    , mOwnedContext(std::move(context))
    , mDefaultFont(fonts::GetFontLibrary().GetFont(defaultFont, 96))
    , mTargetsStack(std::make_unique<PaintTargetsStack>(renderer, mContext))
    , mStrokeGenerator(std::make_unique<StrokeGenerator>())
{
   assert(mOwnedContext != nullptr);
   assert(mDefaultFont != nullptr);
}

GLPainter::~GLPainter()
{
}

RendererID GLPainter::GetRendererID() const
{
   return OpenGLRendererID;
}

Size GLPainter::GetSize() const
{
   return mContext.GetSize();
}

std::shared_ptr<PainterFont> GLPainter::CreateFont(const FontInfo& fontInfo)
{
   return fonts::GetFontLibrary().GetFont(fontInfo, 96);
}

std::shared_ptr<PainterFont> GLPainter::GetDefaultFont() const
{
   return mDefaultFont;
}

std::shared_ptr<PainterPath> GLPainter::CreatePath()
{
   return std::make_shared<Path>();
}

void GLPainter::DrawPath(const PainterPath& path)
{
   if (!mInPaint || path.GetRendererID() != OpenGLRendererID)
      return;
   
   static_cast<const Path&>(path).Draw(*this, *mCurrentPaintTarget);
}

std::shared_ptr<PainterImage> GLPainter::CreateImage(
   PainterImageFormat format, uint32_t width, uint32_t height,
   const void* data /*= nullptr*/, const void* alphaData /*= nullptr*/)
{
   std::vector<uint8_t> mergedData;

   if (data != nullptr && width > 0 && height > 0)
   {
      if (format == PainterImageFormat::RGB888)
      {
         const size_t rowStride = width * 3;
         const size_t mergedDataSize = rowStride * height;
         
         mergedData.reserve(mergedDataSize);

         const uint8_t* inputData = static_cast<const uint8_t*>(data);
         uint8_t* outputRow = mergedData.data();

         for (int32_t row = height - 1; row >= 0; --row)
         {
            std::memcpy(outputRow, inputData + row * rowStride, rowStride);
            outputRow += rowStride;
         }
      }
      else if (format == PainterImageFormat::RGBA8888)
      {
         const size_t mergedDataSize = 4 * width * height;
         mergedData.reserve(4 * width * height);

         uint8_t* outPtr = mergedData.data();

         const uint8_t* dataPtr = static_cast<const uint8_t*>(data);
         const uint8_t* alphaPtr = static_cast<const uint8_t*>(alphaData);

         const size_t inputDataStride = alphaData != nullptr ? 3 * width : 4 * width;

         for (int32_t row = height - 1; row >= 0; --row)
         {
            const uint8_t* dataRow = dataPtr + row * inputDataStride;
            const uint8_t* alphaRow =
               alphaPtr != nullptr ? (alphaPtr + row * width) : nullptr;
            
            for (int32_t col = 0; col < width; ++col)
            {
               *outPtr++ = *dataRow++;
               *outPtr++ = *dataRow++;
               *outPtr++ = *dataRow++;
               *outPtr++ = alphaData != nullptr ? *alphaRow++ : *dataRow++;
            }
         }
      }
   }
   
   return std::make_shared<Texture>(
      mRenderer, width, height, format, true, mergedData.data());
}

std::shared_ptr<PainterImage> GLPainter::GetSubImage(
   const std::shared_ptr<PainterImage>& image, uint32_t x, uint32_t y,
   uint32_t width, uint32_t height)
{
   if (image == nullptr || image->GetRendererID() != OpenGLRendererID)
      return nullptr;

   return std::make_shared<Texture>(
      static_cast<const Texture&>(*image),
      RectType<uint32_t> { PointType<uint32_t> { x, y },
                           SizeType<uint32_t> { width, height } });
}

std::shared_ptr<PainterImage> GLPainter::CreateDeviceImage(
   PainterImageFormat format, uint32_t width, uint32_t height)
{
   return std::make_shared<Texture>(mRenderer, width, height, format, true);
}

void GLPainter::Flush()
{
   if (!mInPaint)
      return;

   mContext.GetFunctions().Flush();
}

void GLPainter::BeginPaint()
{
   assert(!mInPaint);

   if (mInPaint)
      return;

   mRenderer.BeginRendering(mContext);
   
   mCurrentPaintTarget = mTargetsStack->PushTarget(nullptr);
   mCurrentPaintTarget->SetTransform(GetCurrentTransform());

   mInPaint = true;
}

void GLPainter::EndPaint()
{
   if (!mInPaint)
      return;

   mTargetsStack->PopTarget();
   mRenderer.EndRendering();

   mInPaint = false;
}

void GLPainter::DoClear(const Rect& rect, Color color)
{
   if (!mInPaint)
      return;

   mContext.Clear(rect, color);
}

void GLPainter::UpdateBrush(const Brush& brush)
{
   mCurrentBrush = brush;
}

void GLPainter::UpdatePen(const Pen& pen)
{
   mCurrentPen = pen;
}

void GLPainter::UpdateTransform(const Transform& transform)
{
   if (mCurrentPaintTarget)
   {
      mCurrentTransform = transform;
      mCurrentPaintTarget->SetTransform(transform);
   }
}

void GLPainter::UpdateClipRect(const Rect& rect)
{
   if (rect == NoClippingRect)
      mContext.ResetClipRect();
   else
      mContext.SetClipRect(rect);
}

bool GLPainter::UpdateAntiAliasingState(bool enabled)
{
   return false;
}

void GLPainter::UpdateFont(const std::shared_ptr<PainterFont>&)
{
}

namespace
{
const IndexType triIndices[] = { 0, 1, 2 };
const size_t triIndicesCount = 3;

const IndexType quadIndices[] = { 0, 1, 2, 2, 3, 0 };
const size_t quadIndicesCount = 6;

const IndexType lineStripQuadIndices[] = { 0, 1, 2, 3 };

bool SimpleIntersects(
   const Point& p0, const Point& p1, const Point& p2, const Point& p3) noexcept
{
   const Point d1 = p1 - p0;
   const Point d2 = p3 - p2;

   const Point n { -d1.y, d1.x };
   
   const float denom = DotProduct(d2, n);

   if (denom == 0.0f)
      return false;

   const float proj = DotProduct(p0 - p2, n) / denom;

   return proj >= 0 && proj <= 1;
}

bool IsRect(const Point& p0, const Point& p1, const Point& p2, const Point& p3) noexcept
{
   return !SimpleIntersects(p0, p1, p2, p3) &&
          !SimpleIntersects(p0, p3, p1, p2);
}
}

void GLPainter::DoDrawPolygon(const Point* pts, size_t count)
{
   if (!mInPaint)
      return;

   if (count < 2)
      return;

   if (count == 2)
   {
      DoDrawLines(pts, count);
   }
   else if (
      count == 3 || (count == 4 && IsRect(pts[0], pts[1], pts[2], pts[3])))
   {
      const Color color = mCurrentBrush.GetColor();

      Vertex vertices[] = {
         { pts[0], PointType<int16_t> {}, Colors::Transparent, color },
         { pts[1], PointType<int16_t> {}, Colors::Transparent, color },
         { pts[2], PointType<int16_t> {}, Colors::Transparent, color },
         { pts[count - 1], PointType<int16_t> {}, Colors::Transparent, color },
      };
         
      if (mCurrentBrush.GetStyle() == BrushStyle::Solid)
      {
         mCurrentPaintTarget->Append(
            GLenum::TRIANGLES, vertices, count,
            count == 3 ? triIndices : quadIndices,
            count == 3 ? triIndicesCount : quadIndicesCount);
      }

      if (mCurrentPen.GetStyle() != PenStyle::None)
      {
         mStrokeGenerator->StartStroke();
         mStrokeGenerator->AddPoints(pts, count);
         mStrokeGenerator->EndStroke(*mCurrentPaintTarget, mCurrentPen, true);
      }
   }
   else
   {
      Path path;

      path.MoveTo(pts[0]);

      for (size_t i = 1; i < count; ++i)
         path.LineTo(pts[i]);

      path.EndFigure(true);

      path.Draw(*this, *mCurrentPaintTarget);
   }

}

void GLPainter::DoDrawLines(const Point* pts, size_t count)
{
   if (!mInPaint || mCurrentPen.GetStyle() == PenStyle::None)
      return;

   assert(count % 2 == 0);

   const Color penColor = mCurrentPen.GetColor();

   const Point halfPixel { 0.5f, 0.5f };

   for (size_t i = 0; i < count / 2; ++i)
   {
      mStrokeGenerator->StartStroke();
      mStrokeGenerator->AddPoints(&pts[2 * i], 2);
      mStrokeGenerator->EndStroke(*mCurrentPaintTarget, mCurrentPen, false);
   }
}

void GLPainter::DoDrawRect(const Rect& rect)
{
   if (!mInPaint || !rect.IsValid())
      return;
   
   const Point points[] {
      Point { rect.Origin.x, rect.Origin.y },
      Point { rect.Origin.x + rect.Size.width, rect.Origin.y },
      Point { rect.Origin.x + rect.Size.width,
              rect.Origin.y + rect.Size.height },
      Point { rect.Origin.x, rect.Origin.y + rect.Size.height }
   };
   
   DoDrawPolygon(points, 4);
}

void GLPainter::DoDrawRoundedRect(const Rect& rect, float radius)
{
   if (!mInPaint || !rect.IsValid() || radius <= 0.0f)
      return;

   Path path;
   path.AddRoundedRect(rect, radius);
   path.Draw(*this, *mCurrentPaintTarget);
}

void GLPainter::DoDrawEllipse(const Rect& rect)
{
   if (!mInPaint || !rect.IsValid())
      return;
   
   Path path;

   const float horizontalRadius = rect.Size.width / 2;
   const float verticalRadius = rect.Size.height / 2;
   
   path.AddEllipse(
      { rect.Origin.x + horizontalRadius, rect.Origin.y + verticalRadius },
      horizontalRadius, verticalRadius, 0.0f, static_cast<float>(2 * M_PI));

   path.Draw(*this, *mCurrentPaintTarget);
}

void GLPainter::DoDrawImage(
   const PainterImage& image, const Rect& destRect, const Rect& imageRect)
{
   if (!mInPaint || image.GetRendererID() != OpenGLRendererID)
      return;

   auto& texture = static_cast<const Texture&>(image);

   const auto textureRect = texture.GetTextureCoords(rect_cast<uint32_t>(imageRect));

   const Point topLeft     { destRect.Origin.x,                       destRect.Origin.y };
   const Point topRight    { destRect.Origin.x + destRect.Size.width, destRect.Origin.y };
   const Point bottomRight { destRect.Origin.x + destRect.Size.width, destRect.Origin.y + destRect.Size.height };
   const Point bottomLeft  { destRect.Origin.x,                       destRect.Origin.y + destRect.Size.height };

   const PointType<int16_t> uvTopLeft     { textureRect.left,  textureRect.top };
   const PointType<int16_t> uvTopRight    { textureRect.right, textureRect.top};
   const PointType<int16_t> uvBottomRight { textureRect.right, textureRect.bottom };
   const PointType<int16_t> uvBottomLeft  { textureRect.left,  textureRect.bottom };


   const Vertex vertices[] {
      { topLeft,     uvTopLeft,     Colors::White, Colors::Transparent },
      { topRight,    uvTopRight,    Colors::White, Colors::Transparent },
      { bottomRight, uvBottomRight, Colors::White, Colors::Transparent },
      { bottomLeft,  uvBottomLeft,  Colors::White, Colors::Transparent },
   };


   mContext.BindTexture(const_cast<Texture&>(texture).shared_from_this(), 0);

   mCurrentPaintTarget->Append(
      GLenum::TRIANGLES, vertices, 4, quadIndices, quadIndicesCount);
}

void GLPainter::DoDrawText(
   Point origin, const PainterFont& font, Brush backgroundBrush,
   const std::string_view& text)
{
   if (!mInPaint)
      return;

   DoDrawRotatedText(origin, 0.0f, font, backgroundBrush, text);
}

void GLPainter::DoDrawRotatedText(
   Point origin, float angle, const PainterFont& font, Brush backgroundBrush,
   const std::string_view& text)
{
   if (!mInPaint || font.GetRendererID() == OpenGLRendererID)
      return;

   const bool hasRotation = std::abs(angle) >= std::numeric_limits<float>::epsilon();

   if (hasRotation)
   {
      auto transform = FullTransform(mCurrentTransform)
                          .Transform(FullTransform::Translation(origin))
                          .Transform(FullTransform::Rotation(angle));

      mCurrentPaintTarget->SetTransform(transform);
   }
   else
   {
      auto transform = mCurrentTransform.Transformed(
         Transform::Translation(std::round(origin.x), std::round(origin.y)));

      mCurrentPaintTarget->SetTransform(transform);
   }

   auto& ftFont = static_cast<const fonts::Font&>(font);

   if (backgroundBrush.GetStyle() != BrushStyle::None)
   {
      auto size = ftFont.GetTextSize(text);
      DoDrawRect(Rect { {}, size });
   }
         
   mRenderer.GetFontRenderer().SetHintingEnabled(!hasRotation);

   ftFont.DrawText(mRenderer.GetFontRenderer(), text, mCurrentBrush.GetColor());

   mCurrentPaintTarget->SetTransform(mCurrentTransform);
}

Size GLPainter::DoGetTextSize(
   const PainterFont& font, const std::string_view& text) const
{
   return font.GetTextSize(text);
}

void GLPainter::PushPaintTarget(const std::shared_ptr<PainterImage>& image)
{
   if (!mInPaint || image->GetRendererID() != OpenGLRendererID)
      return;

   auto texture = std::static_pointer_cast<Texture>(image);

   auto framebuffer = texture->GetFramebuffer(mContext);

   mCurrentPaintTarget = mTargetsStack->PushTarget(framebuffer);
}

void GLPainter::PopPaintTarget(const std::shared_ptr<PainterImage>&)
{
   if (!mInPaint)
      return;

   mCurrentPaintTarget = mTargetsStack->PopTarget();
}

StrokeGenerator& GLPainter::GetStrokeGenerator()
{
   return *mStrokeGenerator;
}

float GLPainter::GetScale() const noexcept
{
   return mContext.GetScaleFactor();
}

} // namespace graphics::gl
