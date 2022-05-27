/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DPathGeometry.h

  Dmitry Vedenko

**********************************************************************/
#include "D2DPathGeometry.h"

#include "D2DRenderer.h"
#include "D2DRenderTarget.h"

namespace graphics::d2d
{
   
D2DPathGeometry::D2DPathGeometry(D2DRenderer& renderer)
    : PainterPath(renderer.GetRendererID())
    , D2DTrackedResource(renderer)
{
}

void D2DPathGeometry::EndFigure(bool closed)
{
   if (!mFigureOpen)
      return;

   auto sink = GetSink();

   if (sink == nullptr)
      return;

   sink->EndFigure(closed ? D2D1_FIGURE_END_CLOSED : D2D1_FIGURE_END_OPEN);
   mFigureOpen = false;
}

void D2DPathGeometry::DoLineTo(Point pt)
{
   if (!mFigureOpen)
      return;

   auto sink = GetSink();

   if (sink == nullptr)
      return;

   sink->AddLine({ pt.x, pt.y });
}

void D2DPathGeometry::DoMoveTo(Point pt)
{
   auto sink = GetSink();

   if (sink == nullptr)
      return;

   if (mFigureOpen)
      sink->EndFigure(D2D1_FIGURE_END_OPEN);

   sink->BeginFigure({ pt.x, pt.y }, D2D1_FIGURE_BEGIN_FILLED);
   mFigureOpen = true;
}

void D2DPathGeometry::DoAddRect(const Rect& rect)
{
   auto sink = GetSink();

   if (sink == nullptr)
      return;

   if (mFigureOpen)
   {
      sink->EndFigure(D2D1_FIGURE_END_OPEN);
      mFigureOpen = false;
   }

   sink->BeginFigure(
      { rect.origin.x, rect.origin.y }, D2D1_FIGURE_BEGIN_FILLED);

   sink->AddLine({ rect.origin.x + rect.size.width, rect.origin.y });

   sink->AddLine(
      { rect.origin.x + rect.size.width, rect.origin.y + rect.size.height });

   sink->AddLine({ rect.origin.x, rect.origin.y + rect.size.height });

   sink->EndFigure(D2D1_FIGURE_END_CLOSED);
}

void D2DPathGeometry::Draw(D2DRenderTarget& target)
{
   if (mFigureOpen)
   {
      mSink->EndFigure(D2D1_FIGURE_END_OPEN);
      mFigureOpen = false;
   }

   if (mSink)
   {
      mSink->Close();
      mSink.Reset();
   }

   if (mPathGeometry)
   {
      if (target.GetCurrentD2DBrush() != nullptr)
      {
         target.GetD2DRenderTarget()->FillGeometry(
            mPathGeometry.Get(), target.GetCurrentD2DBrush());
      }

      if (target.GetCurrentPen().GetStyle() != PenStyle::None)
      {
         target.GetD2DRenderTarget()->DrawGeometry(
            mPathGeometry.Get(), target.GetCurrentD2DPen(),
            target.GetCurrentPen().GetWidth(),
            mRenderer.GetStrokeStyle(target.GetCurrentPen()));
      }
   }
}

void D2DPathGeometry::Draw(D2DRenderTarget& target) const
{
   const_cast<D2DPathGeometry*>(this)->Draw(target);
}

ID2D1GeometrySink* D2DPathGeometry::GetSink()
{
   if (mSink != nullptr)
      return mSink.Get();

   auto result = mRenderer.GetD2DFactory()->CreatePathGeometry(
      mPathGeometry.ReleaseAndGetAddressOf());

   if (result != S_OK)
      return {};

   result = mPathGeometry->Open(mSink.GetAddressOf());

   if (result != S_OK)
      return {};

   return mSink.Get();
}

void D2DPathGeometry::CleanupDirect2DResources()
{
   mSink.Reset();
   mPathGeometry.Reset();
}

} // namespace graphics::d2d
