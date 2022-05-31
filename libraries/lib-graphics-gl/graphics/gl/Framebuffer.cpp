/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Framebuffer.cpp

  Dmitry Vedenko

**********************************************************************/
#include "Framebuffer.h"

#include "Context.h"
#include "Texture.h"

namespace graphics::gl
{
Framebuffer::Framebuffer(
   Context& context, Texture& texture, GLuint glTexture, GLenum textureTarget,
   const RectType<uint32_t>& framebufferRect)
    : mContext(&context)
    , mTextureDestroyedSubscription(
         texture.GetTextureDestroyedMessagePublisher().Subscribe(
            [this](auto) { ReleaseFramebuffer(); }))
    , mContextDestroyedSubscription(context.Subscribe([this](ContextDestroyedMessage){
            mFramebuffer = 0;
            mContext = nullptr;
         }))
    , mFramebufferRect(framebufferRect)
{
   auto& functions = context.GetFunctions();

   functions.GenFramebuffers(1, &mFramebuffer);
   functions.BindFramebuffer(GLenum::FRAMEBUFFER, mFramebuffer);
   functions.FramebufferTexture2D(GLenum::FRAMEBUFFER, GLenum::COLOR_ATTACHMENT0, textureTarget, glTexture, 0);

   const auto status = functions.CheckFramebufferStatus(GLenum::FRAMEBUFFER);

   if (status != GLenum::FRAMEBUFFER_COMPLETE)
   {
      functions.DeleteFramebuffers(1, &mFramebuffer);
      mFramebuffer = 0;
   }
}

void Framebuffer::ReleaseFramebuffer()
{
   if (mContext != nullptr && mFramebuffer != 0)
   {
      mContext->ReleaseContextResource(
         ContextResourceType::Framebuffer, mFramebuffer);

      mFramebuffer = 0;
   }
}

Framebuffer::~Framebuffer()
{
   ReleaseFramebuffer();
}

bool Framebuffer::IsOk() const noexcept
{
   return mFramebuffer != 0;
}

void Framebuffer::Bind(Context& context)
{
   auto& functions = context.GetFunctions();

   functions.BindFramebuffer(GLenum::FRAMEBUFFER, mFramebuffer);
}

void Framebuffer::Unbind(Context& context)
{
   auto& functions = context.GetFunctions();
   functions.BindFramebuffer(GLenum::FRAMEBUFFER, 0);
}

uint32_t Framebuffer::GetWidth() const noexcept
{
   return mFramebufferRect.size.width;
}

uint32_t Framebuffer::GetHeight() const noexcept
{
   return mFramebufferRect.size.height;
}

float Framebuffer::GetScaleFactor() const noexcept
{
   return 1.0f;
}

uint32_t Framebuffer::GetDPI() const noexcept
{
   return 96;
}

} // namespace graphics::gl
