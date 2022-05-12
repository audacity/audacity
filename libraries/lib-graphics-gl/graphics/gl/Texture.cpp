/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Texture.cpp

  Dmitry Vedenko

**********************************************************************/
#include "Texture.h"

#include "Context.h"
#include "GLRenderer.h"

namespace graphics::gl
{
namespace
{
RectType<uint32_t>
RectFromSubRect(const RectType<uint32_t>& parentRect, const RectType<uint32_t>& subRect)
{
   const auto parentWidth = parentRect.Size.width;
   const auto parentHeight = parentRect.Size.height;

   const auto width =
      subRect.Origin.x < parentWidth ?
         std::max(parentWidth - subRect.Origin.x, subRect.Size.width) :
         0;

   const auto height =
      subRect.Origin.y < parentHeight ?
         std::max(parentHeight - subRect.Origin.y, subRect.Size.height) :
         0;

   return { parentRect.Origin + subRect.Origin,
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

         const bool rebound = currrentTexture == nullptr ||
                              currrentTexture->mSharedGLObject.get() != this;

         if (rebound)
            functions.BindTexture(GLenum::TEXTURE_2D, mTextureID);

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
               currrentTexture == nullptr ?
                  0 :
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
   }

   ~SharedGLObject()
   {
      Publish(TextureDestroyedMessage {});
      
      auto& functions = mRenderer.GetResourceContext().GetFunctions();

      if (mTextureID != 0)
         functions.DeleteTextures(1, &mTextureID);
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
         return ;

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

   bool Update(
      GLenum target, const RectType<uint32_t>& rect,
      const uint8_t* data)
   {
      if (mDataBuffer.empty())
         return false;

      if (rect.Size.width == 0 || rect.Size.height == 0)
         return true;

      const auto bytesPerPixel = mFormat == PainterImageFormat::RGB888 ? 3 : 4;
      const auto outStride = mWidth * bytesPerPixel;

      uint8_t* outData = mDataBuffer.data() + rect.Origin.y * outStride +
                         rect.Origin.x * bytesPerPixel;

      const uint8_t* inData = data;

      const auto inStride = rect.Size.width * bytesPerPixel;

      for (uint32_t row = 0; row < rect.Size.height; ++row)
      {
         std::memcpy(outData, inData, inStride);

         inData += inStride;
         outData += outStride;
      }

      mDirty = true;

      return true;
   }

   FramebufferPtr GetFramebuffer(Context& context)
   {
      return {};
   }

private:
   GLRenderer& mRenderer;

   GLuint mTextureID;

   PainterImageFormat mFormat;
   uint32_t mWidth;
   uint32_t mHeight;

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

void Texture::Bind(Context& context)
{
   mSharedGLObject->Bind(context, GLenum::TEXTURE_2D);
}

bool Texture::IsDirty() const
{
   return mSharedGLObject->IsDirty();
}

void Texture::PerformUpdate(Context& ctx)
{
   mSharedGLObject->PerformUpdate(ctx);
}

uint32_t Texture::GetWidth() const
{
   return mTextureRect.Size.width;
}

uint32_t Texture::GetHeight() const
{
   return mTextureRect.Size.width;
}

bool Texture::Update(
   const RectType<uint32_t>& rect, const void* data)
{
   return mSharedGLObject->Update(
      GLenum::TEXTURE_2D, RectFromSubRect(mTextureRect, rect),
      static_cast<const uint8_t*>(data));
}

FramebufferPtr Texture::GetFramebuffer(Context& context)
{
   return mSharedGLObject->GetFramebuffer(context);
}

Observer::Publisher<TextureDestroyedMessage>&
Texture::GetTextureDestroyedMessagePublisher()
{
   return *mSharedGLObject;
}

}
