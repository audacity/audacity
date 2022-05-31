/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Texture.cpp

  Dmitry Vedenko

**********************************************************************/
#include "Texture.h"

#include <cstring>
#include <map>

#include "Context.h"
#include "GLRenderer.h"
#include "Framebuffer.h"

namespace graphics::gl
{
namespace
{
RectType<uint32_t>
RectFromSubRect(const RectType<uint32_t>& parentRect, const RectType<uint32_t>& subRect)
{
   const auto parentWidth = parentRect.size.width;
   const auto parentHeight = parentRect.size.height;

   const auto width =
      subRect.origin.x < parentWidth ?
         std::min(parentWidth - subRect.origin.x, subRect.size.width) :
         0;

   const auto height =
      subRect.origin.y < parentHeight ?
         std::min(parentHeight - subRect.origin.y, subRect.size.height) :
         0;

   return { parentRect.origin + subRect.origin,
            SizeType<uint32_t> { width, height } };
}
}

extern const RendererID OpenGLRendererID;

class Texture::SharedGLObject final :
    public Observer::Publisher<TextureDestroyedMessage>
{
public:
   SharedGLObject(
      GLRenderer& renderer, uint32_t width, uint32_t height,
      PainterImageFormat format, bool isStatic, const uint8_t* data)
       : mRenderer(renderer)
       , mFormat(format)
       , mWidth(width)
       , mHeight(height)
   {
      auto& context = mRenderer.GetResourceContext();
      auto& functions = context.GetFunctions();

      functions.GenTextures(1, &mTextureID);

      if (isStatic)
      {
         auto currrentTexture = context.GetCurrentTexture(1);
         context.SetClientActiveTexture(1);

         const bool rebound = currrentTexture != nullptr &&
                              currrentTexture->mSharedGLObject.get() != this;

         functions.BindTexture(GLenum::TEXTURE_2D, mTextureID);

         if (data != nullptr)
         {
            context.SetBestUnpackAlignment(
               width * (format == PainterImageFormat::RGB888 ? 3 : 4));
         }

         functions.TexImage2D(
            GLenum::TEXTURE_2D, 0,
            format == PainterImageFormat::RGB888 ? GLenum::RGB8 : GLenum::RGBA8,
            mWidth, mHeight, 0,
            format == PainterImageFormat::RGB888 ? GLenum::RGB : GLenum::RGBA,
            GLenum::UNSIGNED_BYTE, data);

         if (rebound)
         {
            functions.BindTexture(
               GLenum::TEXTURE_2D,
               currrentTexture->mSharedGLObject->mTextureID);
         }
      }
      else
      {
         const auto bytesPerPixel =
            format == PainterImageFormat::RGB888 ? 3 : 4;

         const auto size = mWidth * mHeight * bytesPerPixel;

         if (data != nullptr)
            mDataBuffer.insert(mDataBuffer.begin(), data, data + size);
         else
            mDataBuffer.resize(size);
      }

      mContextDestroyedSubscription = renderer.Subscribe(
         [this](RendererDestroyedMessage) { mTextureID = 0; });
   }

   ~SharedGLObject()
   {
      DestroyTexture();
   }

   void DestroyTexture()
   {
      if (mTextureID == 0)
         return;

      Publish(TextureDestroyedMessage {});

      auto& functions = mRenderer.GetResourceContext().GetFunctions();
      functions.DeleteTextures(1, &mTextureID);

      mTextureID = 0;
   }

   bool IsDirty() const noexcept
   {
      return mDirty;
   }

   void Bind(Context& context, GLenum target)
   {
      auto& functions = context.GetFunctions();

      functions.BindTexture(target, mTextureID);
   }

   void PerformUpdate(Context& ctx)
   {
      if (!mDirty)
         return;

      mDirty = false;

      auto& functions = mRenderer.GetResourceContext().GetFunctions();

      const auto internalFormat =
         mFormat == PainterImageFormat::RGB888 ? GLenum::RGB8 : GLenum::RGBA8;

      const auto format =
         mFormat == PainterImageFormat::RGB888 ? GLenum::RGB : GLenum::RGBA;

      functions.TexImage2D(
         GLenum::TEXTURE_2D, 0, internalFormat, mWidth, mHeight, 0, format,
         GLenum::UNSIGNED_BYTE, nullptr);

      functions.TexImage2D(
         GLenum::TEXTURE_2D, 0, internalFormat, mWidth, mHeight, 0, format,
         GLenum::UNSIGNED_BYTE, mDataBuffer.data());
   }

   TextureCoords
   Update(GLenum target, const RectType<uint32_t>& rect, const uint8_t* data)
   {
      if (mDataBuffer.empty())
         return {};

      if (rect.size.width == 0 || rect.size.height == 0)
         return {};

      const auto bytesPerPixel = mFormat == PainterImageFormat::RGB888 ? 3 : 4;
      const auto outStride = mWidth * bytesPerPixel;

      uint8_t* outData = mDataBuffer.data() + rect.origin.y * outStride +
                         rect.origin.x * bytesPerPixel;

      const uint8_t* inData = data;

      const auto inStride = rect.size.width * bytesPerPixel;

      for (uint32_t row = 0; row < rect.size.height; ++row)
      {
         std::memcpy(outData, inData, inStride);

         inData += inStride;
         outData += outStride;
      }

      mDirty = true;

      return GetFlippedTextureCoords(rect);
   }

   GLuint GetTextureID() const noexcept
   {
      return mTextureID;
   }

   TextureCoords GetTextureCoords(const RectType<uint32_t>& rect) const noexcept
   {
      auto left = rect.origin.x;
      auto right = left + rect.size.width;
      auto top = mHeight - rect.origin.y;
      auto bottom = top - rect.size.height;

      constexpr auto multiplier = std::numeric_limits<int16_t>::max();

      return { static_cast<int16_t>(left * multiplier / mWidth),
               static_cast<int16_t>(top * multiplier / mHeight),
               static_cast<int16_t>(right * multiplier / mWidth),
               static_cast<int16_t>(bottom * multiplier / mHeight) };
   }

   TextureCoords GetFlippedTextureCoords(const RectType<uint32_t>& rect) const noexcept
   {
      auto left = rect.origin.x;
      auto right = left + rect.size.width;
      auto top = rect.origin.y;
      auto bottom = top + rect.size.height;

      constexpr auto multiplier = std::numeric_limits<int16_t>::max();

      return { static_cast<int16_t>(left * multiplier / mWidth),
               static_cast<int16_t>(top * multiplier / mHeight),
               static_cast<int16_t>(right * multiplier / mWidth),
               static_cast<int16_t>(bottom * multiplier / mHeight) };
   }

   RectType<uint32_t> GetOpenGLRect(RectType<uint32_t> rect) const noexcept
   {
      rect.origin.y = mHeight - rect.origin.y - rect.size.height;
      return rect;
   }

   std::shared_ptr<Framebuffer>
   GetCachedFramebuffer(const Context* context) const
   {
      auto it = mFramebuffersCache.find(context);

      if (it == mFramebuffersCache.end() || !it->second->IsOk())
         return {};

      return it->second;
   }

   void CacheFramebuffer(const Context* context, const std::shared_ptr<Framebuffer>& framebuffer)
   {
      mFramebuffersCache[context] = framebuffer;
   }

private:
   GLRenderer& mRenderer;

   GLuint mTextureID;

   PainterImageFormat mFormat;
   uint32_t mWidth;
   uint32_t mHeight;

   Observer::Subscription mContextDestroyedSubscription;

   std::map<const Context*, std::shared_ptr<Framebuffer>> mFramebuffersCache;

   std::vector<uint8_t> mDataBuffer;
   bool mDirty { false };
};

Texture::Texture(
   GLRenderer& renderer, uint32_t width, uint32_t height,
   PainterImageFormat format, bool isStatic, const void* data)
    : PainterImage(OpenGLRendererID)
    , mSharedGLObject(std::make_shared<SharedGLObject>(
         renderer, width, height, format, isStatic,
         static_cast<const uint8_t*>(data)))
    , mTextureRect { PointType<uint32_t> {},
                     SizeType<uint32_t> { width, height } }
{
}

Texture::Texture(const Texture& baseTexture, const RectType<uint32_t>& rect)
    : PainterImage(OpenGLRendererID)
    , mSharedGLObject(baseTexture.mSharedGLObject)
    , mTextureRect(RectFromSubRect(baseTexture.mTextureRect, rect))
{
}

Texture::~Texture()
{
}

void Texture::Bind(Context& context, const Texture* texture)
{
   if (texture == nullptr || texture->mSharedGLObject != mSharedGLObject)
      mSharedGLObject->Bind(context, GLenum::TEXTURE_2D);
}

bool Texture::IsDirty() const
{
   return mSharedGLObject->IsDirty();
}

void Texture::PerformUpdate(Context& ctx)
{
   ctx.BindTexture(shared_from_this(), 1);
   mSharedGLObject->PerformUpdate(ctx);
}

uint32_t Texture::GetWidth() const
{
   return mTextureRect.size.width;
}

uint32_t Texture::GetHeight() const
{
   return mTextureRect.size.height;
}

Texture::TextureCoords
Texture::Update(const RectType<uint32_t>& rect, const void* data)
{
   return mSharedGLObject->Update(
      GLenum::TEXTURE_2D, RectFromSubRect(mTextureRect, rect),
      static_cast<const uint8_t*>(data));
}

FramebufferPtr Texture::GetFramebuffer(Context& context)
{
   auto cachedFramebuffer = mSharedGLObject->GetCachedFramebuffer(&context);

   if (cachedFramebuffer != nullptr)
      return cachedFramebuffer;

   auto rect = mSharedGLObject->GetOpenGLRect(mTextureRect);

   auto framebuffer = std::shared_ptr<Framebuffer>(new Framebuffer(
      context, *this, mSharedGLObject->GetTextureID(), GLenum::TEXTURE_2D,
      mTextureRect));

   if (framebuffer->IsOk())
   {
      mSharedGLObject->CacheFramebuffer(&context, framebuffer);
      return framebuffer;
   }

   return nullptr;
}

Observer::Publisher<TextureDestroyedMessage>&
Texture::GetTextureDestroyedMessagePublisher()
{
   return *mSharedGLObject;
}

Texture::TextureCoords
Texture::GetTextureCoords(const RectType<uint32_t>& rect) const noexcept
{
   return mSharedGLObject->GetTextureCoords(RectFromSubRect(mTextureRect, rect));
}

}
