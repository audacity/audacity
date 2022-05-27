/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  D2DRenderer.cpp

  Dmitry Vedenko

**********************************************************************/
#include "D2DRenderer.h"

#include "D2DPainter.h"
#include "D2DFont.h"
#include "D2DPathGeometry.h"

#include "graphics/RendererID.h"

#include "D2DFontCollection.h"

#include "bitmaps/D2DWICBitmap.h"

#include "render_targets/D2DWindowRenderTarget.h"
#include "render_targets/D2DDCRenderTarget.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <d2d1.h>
#include <dwrite.h>
#include <wincodec.h>
#include <wrl.h>

#ifdef CreateFont
#  undef CreateFont
#endif

namespace graphics::d2d
{

namespace
{
auto rendererId = RegisterRenderer("Direct2DRenderer");

class MeasuringPainter : public Painter
{
public:
   MeasuringPainter(
      D2DRenderer& renderer, uint32_t dpi, const FontInfo& defaultFont)
       : mRenderer(renderer)
       , mDefaultFont(defaultFont)
       , mDPI(dpi)
   {
   }

   RendererID GetRendererID() const override
   {
      return rendererId;
   }

   Size GetSize() const override
   {
      return {};
   }

   std::shared_ptr<PainterFont> CreateFont(const FontInfo& fontInfo) override
   {
      return mRenderer.GetFontCollection()->GetFont(fontInfo, mDPI);
   }

   std::shared_ptr<PainterFont> GetDefaultFont() const override
   {
      return mRenderer.GetFontCollection()->GetFont(mDefaultFont, mDPI);
   }

   std::shared_ptr<PainterImage> CreateImage(
      PainterImageFormat, uint32_t, uint32_t, const void*, const void*) override
   {
      return {};
   }

   std::shared_ptr<PainterImage> GetSubImage(
      const std::shared_ptr<PainterImage>&, uint32_t, uint32_t, uint32_t,
      uint32_t) override
   {
      return {};
   }

   std::shared_ptr<PainterImage>
   CreateDeviceImage(PainterImageFormat, uint32_t, uint32_t) override
   {
      return {};
   }

   void Flush() override
   {
   }

   std::shared_ptr<PainterPath> CreatePath() override
   {
      return {};
   }

   void DrawPath(const PainterPath&) override
   {
   }

protected:
   void BeginPaint() override
   {
   }

   void EndPaint() override
   {
   }

   void DoClear(const Rect&, Color) override
   {
   }

   void UpdateBrush(const Brush&) override
   {
   }

   void UpdatePen(const Pen&) override
   {
   }

   void UpdateTransform(const Transform&) override
   {
   }

   void UpdateClipRect(const Rect&) override
   {
   }

   bool UpdateAntiAliasingState(bool) override
   {
      return false;
   }

   void UpdateFont(const std::shared_ptr<PainterFont>&) override
   {
   }

   void DoDrawPolygon(const Point*, size_t) override
   {
   }

   void DoDrawLines(const Point*, size_t) override
   {
   }

   void DoDrawRect(const Rect&) override
   {
   }

   void DoDrawRoundedRect(const Rect&, float) override
   {
   }

   void DoDrawEllipse(const Rect&) override
   {
   }

   void DoDrawImage(const PainterImage&, const Rect&, const Rect&) override
   {
   }

   void DoDrawText(
      Point, const PainterFont&, Brush, const std::string_view&) override
   {
   }

   void DoDrawRotatedText(
      Point, float, const PainterFont&, Brush, const std::string_view&) override
   {
   }

   Size DoGetTextSize(
      const PainterFont& font, const std::string_view& text) const override
   {
      return font.GetTextSize(text);
   }

   void PushPaintTarget(const std::shared_ptr<PainterImage>&) override
   {
   }

   void PopPaintTarget(const std::shared_ptr<PainterImage>&) override
   {
   }

private:
   D2DRenderer& mRenderer;
   FontInfo mDefaultFont;
   uint32_t mDPI;
};
} // namespace

class D2DRenderer::D2DRendererImpl final
{
public:
   using D2D1CreateFactoryFn = HRESULT(STDAPICALLTYPE*)(
      D2D1_FACTORY_TYPE factoryType, REFIID riid,
      const D2D1_FACTORY_OPTIONS* pFactoryOptions, ID2D1Factory** ppIFactory);

   using DWriteCreateFactoryFn = HRESULT(STDAPICALLTYPE*)(
      DWRITE_FACTORY_TYPE factoryType, REFIID iid, IDWriteFactory** factory);

   D2DRendererImpl()
   {
      if (!CreateD2D1Factory())
         return;

      if (!CreateDWriteFactory())
         return;

      if (!CreateWICFactory())
         return;

      mFontCollection =
         std::make_unique<D2DFontCollection>(rendererId, mDWriteFactory.Get());

      FillStrokeStyles();
   }

   bool CreateD2D1Factory()
   {
      mDirect2DLibrary = LoadLibrary("d2d1.dll");

      if (mDirect2DLibrary == nullptr)
         return false;

      auto d2d1CreateFactory = reinterpret_cast<D2D1CreateFactoryFn>(
         GetProcAddress(mDirect2DLibrary, "D2D1CreateFactory"));

      if (d2d1CreateFactory == nullptr)
         return false;

      D2D1_FACTORY_OPTIONS d2d1FactoryOptions {
#ifdef NDEBUG
         D2D1_DEBUG_LEVEL_NONE,
#else
         D2D1_DEBUG_LEVEL_INFORMATION
#endif
      };

      HRESULT result = d2d1CreateFactory(
         D2D1_FACTORY_TYPE_SINGLE_THREADED, __uuidof(ID2D1Factory),
         &d2d1FactoryOptions, mD2D1Factory.ReleaseAndGetAddressOf());

      return result == S_OK;
   }

   bool CreateDWriteFactory()
   {
      mDirectWriteLibrary = LoadLibrary("dwrite.dll");

      if (mDirectWriteLibrary == nullptr)
         return false;

      auto dWriteCreateFactory = reinterpret_cast<DWriteCreateFactoryFn>(
         GetProcAddress(mDirectWriteLibrary, "DWriteCreateFactory"));

      if (dWriteCreateFactory == nullptr)
         return false;

      HRESULT result = dWriteCreateFactory(
         DWRITE_FACTORY_TYPE_SHARED, __uuidof(IDWriteFactory),
         mDWriteFactory.ReleaseAndGetAddressOf());

      return result == S_OK;
   }

   template <typename T>
   bool CreateInstance(REFCLSID clsid, Microsoft::WRL::ComPtr<T>& ptr)
   {
      HRESULT result = CoCreateInstance(
         clsid, nullptr, CLSCTX_INPROC_SERVER, __uuidof(T),
         reinterpret_cast<void**>(ptr.ReleaseAndGetAddressOf()));

      return result == S_OK;
   }

   bool CreateWICFactory()
   {
      (void)CoInitialize(nullptr);
      return CreateInstance(CLSID_WICImagingFactory, mWICImagingFactory);
   }

   ~D2DRendererImpl()
   {
      if (mWICImagingFactory)
      {
         mWICImagingFactory.Reset();
         CoUninitialize();
      }

      if (mDirectWriteLibrary != nullptr)
      {
         mFontCollection.reset();
         mDWriteFactory.Reset();
         FreeLibrary(mDirectWriteLibrary);
      }

      if (mDirect2DLibrary != nullptr)
      {
         mStrokeStyles = {};
         mD2D1Factory.Reset();
         FreeLibrary(mDirect2DLibrary);
      }
   }

   bool IsAvailable() const noexcept
   {
      return mD2D1Factory != nullptr && mDWriteFactory != nullptr &&
             mWICImagingFactory != nullptr;
   }

   D2DFontCollection& GetFontCollection()
   {
      return *mFontCollection;
   }

   template <bool rgba, bool interleavedAlpha>
   void CopyData(
      BYTE* outPtr, uint32_t outStride, uint32_t width, uint32_t height,
      const uint8_t* data, const uint8_t* alphaData)
   {
      for (uint32_t row = 0; row < height; ++row)
      {
         BYTE* dst = outPtr + row * outStride;

         for (uint32_t column = 0; column < width; ++column)
         {
            const BYTE r = *data++;
            const BYTE g = *data++;
            const BYTE b = *data++;

            if constexpr (rgba)
            {
               const uint8_t a = interleavedAlpha ? *data++ : *alphaData++;

               *dst++ = static_cast<BYTE>(b * a / 255);
               *dst++ = static_cast<BYTE>(g * a / 255);
               *dst++ = static_cast<BYTE>(r * a / 255);
               *dst++ = a;
            }
            else
            {
               *dst++ = b;
               *dst++ = g;
               *dst++ = r;
               *dst++ = 255;
            }
         }
      }
   };

   std::shared_ptr<D2DWICBitmap> CreateImage(
      D2DRenderer& renderer, PainterImageFormat format, uint32_t width,
      uint32_t height, const void* data, const void* alphaData)
   {
      Microsoft::WRL::ComPtr<IWICBitmap> wicBitmap;

      auto result = mWICImagingFactory->CreateBitmap(
         width, height, GUID_WICPixelFormat32bppPBGRA, WICBitmapCacheOnLoad,
         wicBitmap.ReleaseAndGetAddressOf());

      if (result != S_OK)
         return {};

      WICRect lockRegion = { 0, 0, static_cast<INT>(width),
                             static_cast<INT>(height) };

      Microsoft::WRL::ComPtr<IWICBitmapLock> lock;

      result =
         wicBitmap->Lock(&lockRegion, WICBitmapLockWrite, lock.GetAddressOf());

      if (result != S_OK)
         return {};

      UINT bufferSize = 0;
      UINT stride = 0;
      BYTE* outPtr = NULL;

      result = lock->GetStride(&stride);

      if (result != S_OK)
         return {};

      result = lock->GetDataPointer(&bufferSize, &outPtr);

      if (result != S_OK)
         return {};

      if (format == PainterImageFormat::RGB888)
      {
         CopyData<false, false>(
            outPtr, stride, width, height, static_cast<const uint8_t*>(data),
            nullptr);
      }
      else
      {
         if (alphaData)
         {
            CopyData<true, false>(
               outPtr, stride, width, height, static_cast<const uint8_t*>(data),
               static_cast<const uint8_t*>(alphaData));
         }
         else
         {
            CopyData<true, true>(
               outPtr, stride, width, height, static_cast<const uint8_t*>(data),
               nullptr);
         }
      }

      return std::make_shared<D2DWICBitmap>(
         renderer, wicBitmap, format == PainterImageFormat::RGBA8888);
      ;
   }

   ID2D1Factory* GetD2DFactory() const
   {
      return mD2D1Factory.Get();
   }

   void FillStrokeStyles()
   {
      constexpr D2D1_DASH_STYLE dashStyle[] = {
         D2D1_DASH_STYLE_SOLID, D2D1_DASH_STYLE_SOLID, D2D1_DASH_STYLE_DOT,
         D2D1_DASH_STYLE_DASH,  D2D1_DASH_STYLE_DASH,  D2D1_DASH_STYLE_DASH_DOT
      };

      for (size_t i = 1; i <= size_t(PenStyle::DotDash); ++i)
      {
         const D2D1_STROKE_STYLE_PROPERTIES props { D2D1_CAP_STYLE_FLAT,
                                                    D2D1_CAP_STYLE_FLAT,
                                                    D2D1_CAP_STYLE_FLAT,
                                                    D2D1_LINE_JOIN_ROUND,
                                                    0,
                                                    dashStyle[i],
                                                    0.0f };

         mD2D1Factory->CreateStrokeStyle(
            props, nullptr, 0, mStrokeStyles[i].ReleaseAndGetAddressOf());
      }
   }

   ID2D1StrokeStyle* GetStrokeStyle(PenStyle style) const
   {
      return mStrokeStyles[size_t(style)].Get();
   }

private:
   HMODULE mDirect2DLibrary { nullptr };
   HMODULE mDirectWriteLibrary { nullptr };

   Microsoft::WRL::ComPtr<ID2D1Factory> mD2D1Factory;
   Microsoft::WRL::ComPtr<IDWriteFactory> mDWriteFactory;
   Microsoft::WRL::ComPtr<IWICImagingFactory> mWICImagingFactory;

   std::unique_ptr<D2DFontCollection> mFontCollection;

   std::array<
      Microsoft::WRL::ComPtr<ID2D1StrokeStyle>, size_t(PenStyle::DotDash) + 1>
      mStrokeStyles;
};

D2DRenderer::D2DRenderer()
    : mImpl(std::make_unique<D2DRendererImpl>())
{
   if (!mImpl->IsAvailable())
      Shutdown();
}

D2DRenderer::~D2DRenderer()
{
   Shutdown();
}

bool D2DRenderer::IsAvailable() const noexcept
{
   return mImpl != nullptr && mImpl->IsAvailable();
}

void D2DRenderer::Shutdown()
{
   if (mImpl != nullptr)
   {
      Publish({});
      mImpl.reset();
   }
}

std::unique_ptr<Painter>
D2DRenderer::CreateHWNDPainter(void* window, const FontInfo& defaultFont)
{
   auto renderTarget =
      std::make_shared<D2DWindowRenderTarget>(*this, static_cast<HWND>(window));

   if (!renderTarget->IsValid())
      return {};

   return std::make_unique<D2DPainter>(*this, renderTarget, defaultFont);
}

std::unique_ptr<Painter>
D2DRenderer::CreateHDCPainter(void* dc, const FontInfo& defaultFont)
{
   auto renderTarget =
      std::make_shared<D2DDCRenderTarget>(*this, static_cast<HDC>(dc));

   if (!renderTarget->IsValid())
      return {};

   return std::make_unique<D2DPainter>(*this, renderTarget, defaultFont);
}

std::unique_ptr<Painter>
D2DRenderer::CreateMeasuringPainter(const FontInfo& defaultFont)
{
   return std::make_unique<MeasuringPainter>(*this, 96, defaultFont);
}

std::shared_ptr<PainterImage> D2DRenderer::CreateImage(
   PainterImageFormat format, uint32_t width, uint32_t height, const void* data,
   const void* alphaData)
{
   return mImpl ?
             mImpl->CreateImage(*this, format, width, height, data, alphaData) :
             nullptr;
}

RendererID D2DRenderer::GetRendererID() const noexcept
{
   return rendererId;
}

D2DFontCollection* D2DRenderer::GetFontCollection()
{
   if (mImpl)
      return &mImpl->GetFontCollection();

   return nullptr;
}

ID2D1Factory* D2DRenderer::GetD2DFactory() const
{
   return mImpl ? mImpl->GetD2DFactory() : nullptr;
}

ID2D1StrokeStyle* D2DRenderer::GetStrokeStyle(const Pen& pen)
{
   return mImpl ? mImpl->GetStrokeStyle(pen.GetStyle()) : nullptr;
}

std::shared_ptr<D2DPathGeometry> D2DRenderer::CreatePathGeometry()
{
   return std::make_shared<D2DPathGeometry>(*this);
}

D2DRenderer& SharedD2DRenderer()
{
   static D2DRenderer renderer;
   return renderer;
}

} // namespace graphics::d2d
