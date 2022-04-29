/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Painter.cpp

  Dmitry Vedenko

**********************************************************************/
#include "Painter.h"

#include <cassert>

Painter::Painter()
{
   mStateStack.emplace_back();
   mTransformStack.emplace_back();
   mClipStateStack.emplace_back(NoClippingRect);
}

Painter::~Painter() noexcept
{
}

PainterStateMutator Painter::GetStateMutator()
{
   return PainterStateMutator(*this);
}

Brush Painter::GetCurrentBrush() const
{
   assert(!mStateStack.empty());
   if (mStateStack.empty())
      return {};

   return mStateStack.back().Brush;
}

Pen Painter::GetCurrentPen() const
{
   assert(!mStateStack.empty());
   if (mStateStack.empty())
      return {};

   return mStateStack.back().Pen;
}

bool Painter::GetAntiAliasingEnabled() const
{
   assert(!mStateStack.empty());
   if (mStateStack.empty())
      return {};

   return mStateStack.back().Antialiasing;
}

std::shared_ptr<PainterFont> Painter::GetCurrentFont() const
{
   assert(!mStateStack.empty());
   if (mStateStack.empty())
      return {};

   auto font = mStateStack.back().Font;

   return  font != nullptr ? font : GetDefaultFont();
}

PainterTransformMutator Painter::GetTransformMutator()
{
   return PainterTransformMutator(*this);
}

Transform Painter::GetCurrentTransform() const
{
   assert(!mTransformStack.empty());
   if (mTransformStack.empty())
      return {};

   return mTransformStack.back();
}

PainterClipStateMutator Painter::GetClipStateMutator()
{
   return PainterClipStateMutator(*this);
}

Rect Painter::GetCurrentClipRect() const
{
   assert(!mClipStateStack.empty());
   if (mClipStateStack.empty())
      return {};

   return mClipStateStack.back();
}

bool Painter::HasClipping() const noexcept
{
   return GetCurrentClipRect() != NoClippingRect;
}

void Painter::DrawPolygon(const Point* pts, size_t count)
{
   DoDrawPolygon(pts, count);
}

void Painter::DrawLines(const Point* ptr, size_t count)
{
   DoDrawLines(ptr, count);
}

void Painter::DrawLine(Point start, Point end)
{
   Point pts[2] = { start, end };
   DoDrawLines(pts, 2);
}

void Painter::DrawLine(float sx, float sy, float ex, float ey)
{
   DrawLine(Point { sx, sy }, Point { ex, ey });
}

void Painter::DrawRect(const Rect& rect)
{
   DoDrawRect(rect);
}

void Painter::DrawRect(Point topLeft, Size size)
{
   DoDrawRect(Rect { topLeft, size });
}

void Painter::DrawRect(float left, float top, float width, float height)
{
   DrawRect(Point { left, top }, Size { width, height });
}

void Painter::DrawLinearGradientRect(
   const Rect& rect, Color from, Color to, LinearGradientDirection direction)
{
   DoDrawLinearGradientRect(rect, from, to, direction);
}

void Painter::DrawLinearGradientRect(
   Point topLeft, Size size, Color from, Color to,
   LinearGradientDirection direction)
{
   DoDrawLinearGradientRect(Rect { topLeft, size }, from, to, direction);
}

void Painter::DrawLinearGradientRect(
   float left, float top, float width, float height, Color from, Color to,
   LinearGradientDirection direction)
{
   DoDrawLinearGradientRect(
      Rect { Point { left, top }, Size { width, height } }, from, to,
      direction);
}

void Painter::DrawRoundedRect(const Rect& rect, float radius)
{
   DoDrawRoundedRect(rect, radius);
}

void Painter::DrawRoundedRect(Point topLeft, Size size, float radius)
{
   DoDrawRoundedRect(Rect { topLeft, size }, radius);
}

void Painter::DrawRoundedRect(
   float left, float top, float width, float height, float radius)
{
   DrawRoundedRect(Point { left, top }, Size { width, height }, radius);
}

void Painter::DrawEllipse(const Rect& rect)
{
   DoDrawEllipse(rect);
}

void Painter::DrawEllipse(Point topLeft, Size size)
{
   DoDrawEllipse(Rect { topLeft, size });
}

void Painter::DrawEllipse(float left, float top, float width, float height)
{
   DrawEllipse(Point { left, top }, Size { width, height });
}

void Painter::DrawCircle(Point center, float radius)
{
   DrawEllipse(
      center.x - radius, center.y - radius, 2.0f * radius, 2.0f * radius);
}

void Painter::DrawCircle(float cx, float cy, float radius)
{
   DrawEllipse(cx - radius, cy - radius, 2.0f * radius, 2.0f * radius);
}

void Painter::DrawText(
   Point origin, const PainterFont& font, const std::string_view& text)
{
   DoDrawText(origin, font, Brush::NoBrush, text);
}

void Painter::DrawText(
   float x, float y, const PainterFont& font, const std::string_view& text)
{
   DoDrawText(Point { x, y }, font, Brush::NoBrush, text);
}

void Painter::DrawText(Point origin, const std::string_view& text)
{
   DoDrawText(origin, *GetCurrentFont(), Brush::NoBrush, text);
}

void Painter::DrawText(float x, float y, const std::string_view& text)
{
   DoDrawText(Point { x, y }, *GetCurrentFont(), Brush::NoBrush, text);
}


void Painter::DrawText(
   Rect rect, const PainterFont& font, const std::string_view& text,
   PainterHorizontalAlignment horizontalAlignement,
   PainterVerticalAlignment verticalAlignment)
{
   Point origin;

   const auto size = GetTextSize(font, text);

   switch (horizontalAlignement)
   {
   case PainterHorizontalAlignment::Left:
      origin.x = rect.Origin.x;
      break;
   case PainterHorizontalAlignment::Center:
      origin.x = rect.Origin.x + (rect.Size.width - size.width) / 2.0f;
      break;
   case PainterHorizontalAlignment::Right:
      origin.x = rect.Origin.x + (rect.Size.width - size.width);
      break;
   }

   switch (verticalAlignment)
   {
   case PainterVerticalAlignment::Bottom:
      origin.y = rect.Origin.y + (rect.Size.height - size.height);
      break;
   case PainterVerticalAlignment::Center:
      origin.y = rect.Origin.y + (rect.Size.height - size.height) / 2.0;
      break;
   case PainterVerticalAlignment::Top:
      origin.y = rect.Origin.y;
      break;
   }

   DrawText(origin, font, text);
}

void Painter::DrawText(
   Point origin, Size size, const PainterFont& font,
   const std::string_view& text,
   PainterHorizontalAlignment horizontalAlignement,
   PainterVerticalAlignment verticalAlignment)
{
   DrawText(Rect { origin, size }, font, text, horizontalAlignement, verticalAlignment);
}

void Painter::DrawText(
   float x, float y, float w, float h, const PainterFont& font,
   const std::string_view& text,
   PainterHorizontalAlignment horizontalAlignement,
   PainterVerticalAlignment verticalAlignment)
{
   DrawText(
      Rect { Point { x, y }, Size { w, h } }, font, text,
      horizontalAlignement,
      verticalAlignment);
}

void Painter::DrawText(
   Rect rect, const std::string_view& text,
   PainterHorizontalAlignment horizontalAlignement,
   PainterVerticalAlignment verticalAlignment)
{
   DrawText(
      rect, *GetCurrentFont(), text, horizontalAlignement, verticalAlignment);
}

void Painter::DrawText(
   Point origin, Size size, const std::string_view& text,
   PainterHorizontalAlignment horizontalAlignement,
   PainterVerticalAlignment verticalAlignment)
{
   DrawText(
      Rect { origin, size }, *GetCurrentFont(), text, horizontalAlignement,
      verticalAlignment);
}

void Painter::DrawText(
   float x, float y, float w, float h, const std::string_view& text,
   PainterHorizontalAlignment horizontalAlignement,
   PainterVerticalAlignment verticalAlignment)
{
   DrawText(
      Rect { Point { x, y }, Size { w, h } }, *GetCurrentFont(), text,
      horizontalAlignement, verticalAlignment);
}

void Painter::DrawRotatedText(
   Point origin, float angle, const PainterFont& font,
   const std::string_view& text)
{
   DoDrawRotatedText(origin, angle, font, Brush::NoBrush, text);
}

void Painter::DrawRotatedText(
   float x, float y, float angle, const PainterFont& font,
   const std::string_view& text)
{
   DoDrawRotatedText(Point { x, y }, angle, font, Brush::NoBrush, text);
}

void Painter::DrawRotatedText(
   Point origin, float angle, const std::string_view& text)
{
   DoDrawRotatedText(origin, angle, *GetCurrentFont(), Brush::NoBrush, text);
}

void Painter::DrawRotatedText(
   float x, float y, float angle, const std::string_view& text)
{
   DoDrawRotatedText(
      Point { x, y }, angle, *GetCurrentFont(), Brush::NoBrush, text);
}

Size Painter::GetTextSize(
   const PainterFont& font, const std::string_view& text) const
{
   return DoGetTextSize(font, text);
}

Size Painter::GetTextSize(const std::string_view& text) const
{
   return DoGetTextSize(*GetCurrentFont(), text);
}

void Painter::DrawImage(const PainterImage& image, const Rect& rect)
{
   DoDrawImage(image, rect, GetImageRect(image));
}

void Painter::DrawImage(const PainterImage& image, Point topLeft, Size size)
{
   DoDrawImage(image, Rect { topLeft, size }, GetImageRect(image));
}

void Painter::DrawImage(
   const PainterImage& image, float left, float top, float width, float height)
{
   DoDrawImage(
      image, Rect { Point { left, top }, Size { width, height } },
      GetImageRect(image));
}

void Painter::DrawImage(const PainterImage& image, Point topLeft)
{
   DoDrawImage(
      image, Rect { topLeft, Size { float(image.GetWidth()), float(image.GetHeight()) } },
      GetImageRect(image));
}

void Painter::DrawImage(
   const PainterImage& image, float left, float top)
{
   DoDrawImage(
      image, Rect { Point { left, top }, Size { float(image.GetWidth()), float(image.GetHeight()) } },
      GetImageRect(image));
}

void Painter::DrawImage(
   const PainterImage& image, const Rect& destRect, const Rect& sourceRect)
{
   DoDrawImage(image, destRect, sourceRect);
}

void Painter::DrawImage(
   const PainterImage& image, Point destTopLeft, Size destSize,
   Point sourceTopLeft, Size sourceSize)
{
   DoDrawImage(
      image, Rect { destTopLeft, destSize },
      Rect { sourceTopLeft, sourceSize });
}

void Painter::DrawImage(
   const PainterImage& image, float destLeft, float destTop, float destWidth,
   float destHeight, float sourceLeft, float sourceTop, float sourceWidth,
   float sourceHeight)
{
   DoDrawImage(
      image,
      Rect { Point { destLeft, destTop }, Size { destWidth, destHeight } },
      Rect { Point { sourceLeft, sourceTop },
             Size { sourceWidth, sourceHeight } });
}

void Painter::DrawImage(
   const PainterImage& image, const Rect& destRect, Point sourceTopLeft)
{
   DrawImage(
      image, destRect.Origin.x, destRect.Origin.y, destRect.Size.width,
      destRect.Size.height, sourceTopLeft.x, sourceTopLeft.y);
}

void Painter::DrawImage(
   const PainterImage& image, Point destTopLeft, Point sourceTopLeft)
{
   const float imageWidth = image.GetWidth();
   const float imageHeight = image.GetHeight();

   const float sourceWidth = imageWidth - sourceTopLeft.x;
   const float sourceHeight = imageHeight - sourceTopLeft.y;

   if (
      sourceWidth < std::numeric_limits<float>::epsilon() ||
      sourceHeight <= std::numeric_limits<float>::epsilon())
      return;

   DrawImage(
      image, destTopLeft.x, destTopLeft.y, sourceWidth, sourceHeight,
      sourceTopLeft.x, sourceTopLeft.y, sourceWidth, sourceHeight);
}

void Painter::DrawImage(
   const PainterImage& image, Point destTopLeft, Size destSize,
   Point sourceTopLeft)
{
   DrawImage(
      image, destTopLeft.x, destTopLeft.y, destSize.width, destSize.height,
      sourceTopLeft.x, sourceTopLeft.y);
}

void Painter::DrawImage(
   const PainterImage& image, float destLeft, float destTop, float destWidth,
   float destHeight, float sourceLeft, float sourceTop)
{
   const float imageWidth = image.GetWidth();
   const float imageHeight = image.GetHeight();

   const float sourceWidth = std::min(destWidth, imageWidth - sourceLeft);
   const float sourceHeight = std::min(destHeight, imageHeight - sourceTop);

   if (
      sourceWidth < std::numeric_limits<float>::epsilon() ||
      sourceHeight <= std::numeric_limits<float>::epsilon())
      return;

   DrawImage(
      image,
      Rect { Point { destLeft, destTop }, Size { destWidth, destHeight } },
      Rect { Point { sourceLeft, sourceTop },
             Size { sourceWidth, sourceHeight } });
}

void Painter::Clear(Color color)
{
   DoClear(Rect { Point {}, GetSize() }, color);
}

void Painter::Clear(float x, float y, float width, float height, Color color)
{
   DoClear(Rect { Point { x, y }, Size { width, height } }, color);
}

void Painter::Clear(Point origin, Size size, Color color)
{
   DoClear(Rect { origin, size }, color);
}

void Painter::Clear(const Rect& rect, Color color)
{
   DoClear(rect, color);
}

PaintEventHolder Painter::Paint()
{
   return PaintEventHolder(*this);
}

PainterOffscreenHolder
Painter::PaintOn(const std::shared_ptr<PainterImage>& image)
{
   return PainterOffscreenHolder(image, *this);
}

PainterStateMutator::PainterStateMutator(Painter& painter)
    : mPainter(painter)
{
   mPainter.mStateStack.push_back(mPainter.mStateStack.back());
}

PainterStateMutator::~PainterStateMutator()
{
   mPainter.mStateStack.pop_back();

   assert(!mPainter.mStateStack.empty());

   if (!mPainter.mStateStack.empty())
   {
      mPainter.UpdateBrush(GetBrush());

      mPainter.UpdatePen(GetPen());

      mPainter.mStateStack.back().Antialiasing =
         mPainter.UpdateAntiAliasingState(
            mPainter.mStateStack.back().Antialiasing);
   }
}

void PainterStateMutator::SetBrush(const Brush& brush)
{
   mPainter.mStateStack.back().Brush = brush;
   mPainter.UpdateBrush(brush);
}

Brush PainterStateMutator::GetBrush() const
{
   return mPainter.mStateStack.back().Brush;
}

void PainterStateMutator::SetPen(const Pen& pen)
{
   mPainter.mStateStack.back().Pen = pen;
   mPainter.UpdatePen(pen);
}

Pen PainterStateMutator::GetPen() const
{
   return mPainter.mStateStack.back().Pen;
}

void PainterStateMutator::SetAntiAliasingEnabled(bool enabled)
{
   mPainter.mStateStack.back().Antialiasing =
      mPainter.UpdateAntiAliasingState(enabled);
}

bool PainterStateMutator::GetAntiAliasingEnabled() const
{
   return mPainter.mStateStack.back().Antialiasing;
}

void PainterStateMutator::SetFont(std::shared_ptr<PainterFont> font)
{
   mPainter.mStateStack.back().Font =
      font != nullptr ? std::move(font) : mPainter.GetDefaultFont();

   mPainter.UpdateFont(mPainter.mStateStack.back().Font);
}

std::shared_ptr<PainterFont> PainterStateMutator::GetFont() const
{
   return mPainter.mStateStack.back().Font;
}

Painter& PainterStateMutator::GetPainter() noexcept
{
   return mPainter;
}

const Painter& PainterStateMutator::GetPainter() const noexcept
{
   return mPainter;
}

PainterTransformMutator::PainterTransformMutator(Painter& painter)
    : mPainter(painter)
{
   mPainter.mTransformStack.push_back(mPainter.mTransformStack.back());
}

PainterTransformMutator::~PainterTransformMutator()
{
   mPainter.mTransformStack.pop_back();

   assert(!mPainter.mTransformStack.empty());

   if (!mPainter.mTransformStack.empty())
   {
      mPainter.UpdateTransform(GetTransform());
   }
}

void PainterTransformMutator::Translate(float x, float y)
{
   mPainter.mTransformStack.back().Translate(x, y);
   mPainter.UpdateTransform(mPainter.mTransformStack.back());
}

void PainterTransformMutator::Translate(Point pt)
{
   mPainter.mTransformStack.back().Translate(pt);
   mPainter.UpdateTransform(mPainter.mTransformStack.back());
}

void PainterTransformMutator::Scale(float scale)
{
   mPainter.mTransformStack.back().Scale(scale);
   mPainter.UpdateTransform(mPainter.mTransformStack.back());
}

void PainterTransformMutator::Scale(float scx, float scy)
{
   mPainter.mTransformStack.back().Scale(scx, scy);
   mPainter.UpdateTransform(mPainter.mTransformStack.back());
}

void PainterTransformMutator::Scale(Point scale)
{
   mPainter.mTransformStack.back().Scale(scale);
   mPainter.UpdateTransform(mPainter.mTransformStack.back());
}

void PainterTransformMutator::SetTransform(const Transform& transform)
{
   mPainter.mTransformStack.back() = transform;
   mPainter.UpdateTransform(mPainter.mTransformStack.back());
}

Transform PainterTransformMutator::GetTransform() const
{
   return mPainter.mTransformStack.back();
}

Painter& PainterTransformMutator::GetPainter() noexcept
{
   return mPainter;
}

const Painter& PainterTransformMutator::GetPainter() const noexcept
{
   return mPainter;
}

PainterClipStateMutator::PainterClipStateMutator(Painter& painter)
    : mPainter(painter)
{
   mPainter.mClipStateStack.push_back(mPainter.mClipStateStack.back());
}

PainterClipStateMutator::~PainterClipStateMutator()
{
   mPainter.mClipStateStack.pop_back();

   assert(!mPainter.mClipStateStack.empty());

   if (!mPainter.mClipStateStack.empty())
   {
      mPainter.UpdateClipRect(GetClipRect());
   }
}

void PainterClipStateMutator::SetClipRect(const Rect& rect, bool intersect)
{
   const auto resultingRect =
      intersect ? Intersect(mPainter.mClipStateStack.back(), rect) : rect;

   mPainter.mClipStateStack.back() = resultingRect;
   mPainter.UpdateClipRect(resultingRect);
}

void PainterClipStateMutator::SetClipRect(
   Point origin, Size size, bool intersect)
{
   SetClipRect(Rect { origin, size }, intersect);
}

void PainterClipStateMutator::SetClipRect(
   float x, float y, float w, float h, bool intersect)
{
   SetClipRect(Rect { Point { x, y }, Size { w, h } }, intersect);
}

Rect PainterClipStateMutator::GetClipRect() const
{
   return mPainter.mClipStateStack.back();
}

void PainterClipStateMutator::ResetClipRect()
{
   SetClipRect(NoClippingRect, false);
}

Painter& PainterClipStateMutator::GetPainter() noexcept
{
   return mPainter;
}

const Painter& PainterClipStateMutator::GetPainter() const noexcept
{
   return mPainter;
}

PainterPath::PainterPath(const Painter& painter)
    : PainterObject(painter)
{
}

PainterPath::PainterPath(const RendererID& rendererID)
    : PainterObject(rendererID)
{
}

void PainterPath::LineTo(float x, float y)
{
   DoLineTo(Point { x, y });
}

void PainterPath::LineTo(Point pt)
{
   DoLineTo(pt);
}

void PainterPath::MoveTo(float x, float y)
{
   DoMoveTo(Point { x, y });
}

void PainterPath::MoveTo(Point pt)
{
   DoMoveTo(pt);
}

void PainterPath::AddRect(const Rect& rect)
{
   DoAddRect(rect);
}

void PainterPath::AddRect(Point topLeft, Size size)
{
   DoAddRect(Rect { topLeft, size });
}

void PainterPath::AddRect(float left, float top, float width, float height)
{
   DoAddRect(Rect { Point { left, top }, Size { width, height } });
}

RendererID PainterObject::GetRendererID() const noexcept
{
   return mRendererID;
}

PainterObject::PainterObject(const Painter& painter)
    : mRendererID(painter.GetRendererID())
{
}

PainterObject::PainterObject(const RendererID& rendererId)
    : mRendererID(rendererId)
{
}

PainterImage::PainterImage(Painter& painter)
    : PainterObject(painter)
{
}

PainterImage::PainterImage(const RendererID& rendererId)
    : PainterObject(rendererId)
{
}

bool PainterImage::IsValid(Painter&) const
{
   return true;
}

PainterFont::PainterFont(Painter& painter)
    : PainterObject(painter)
{
}

PainterFont::PainterFont(const RendererID& rendererId)
    : PainterObject(rendererId)
{
}

PainterOffscreenHolder::~PainterOffscreenHolder()
{
   mPainter.PopPaintTarget(mSurface);
}

PainterOffscreenHolder::PainterOffscreenHolder(
   const std::shared_ptr<PainterImage>& surface, Painter& painter)
    : mPainter(painter)
    , mSurface(surface)
    , mStateMutator(painter.GetStateMutator())
    , mClipStateMutator(painter.GetClipStateMutator())
    , mTransforfmMutator(painter.GetTransformMutator())
{
   mPainter.PushPaintTarget(mSurface);

   mClipStateMutator.ResetClipRect();
   mTransforfmMutator.SetTransform({});
}

void Painter::DoDrawLinearGradientRect(
   const Rect& rect, Color from, Color to, LinearGradientDirection direction)
{
   float fromX = rect.Origin.x;
   float fromY = rect.Origin.y;

   float toX = fromX + rect.Size.width;
   float toY = fromY + rect.Size.height;

   switch (direction)
   {
   case Painter::LinearGradientDirection::RightToLeft:
      std::swap(fromX, toX);
      toY = fromY;
      break;
   case Painter::LinearGradientDirection::LeftToRight:
      toY = fromY;
      break;
   case Painter::LinearGradientDirection::TopToBottom:
      toX = fromX;
      break;
   case Painter::LinearGradientDirection::BottomToTop:
      toX = fromX;
      std::swap(fromY, toY);
      break;
   default:
      assert(false);
      break;
   }

   const Brush brush(fromX, fromY, toX, toY, from, to);

   UpdateBrush(brush);
   DoDrawRect(rect);
   UpdateBrush(GetCurrentBrush());
}

Rect Painter::GetImageRect(const PainterImage& image) const
{
   return { Point {}, Size { static_cast<float>(image.GetWidth()),
                             static_cast<float>(image.GetHeight()) } };
}

PaintEventHolder::~PaintEventHolder()
{
   mPainter.EndPaint();
}

PaintEventHolder::PaintEventHolder(Painter& painter)
    : mPainter(painter)
{
   mPainter.BeginPaint();
}
