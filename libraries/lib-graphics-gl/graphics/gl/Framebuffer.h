/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Framebuffer.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <memory>

#include "GLFunctions.h"
#include "Observer.h"
#include "graphics/Rect.h"

namespace graphics::gl
{
class Context;
class Texture;

class Framebuffer final
{
public:
   ~Framebuffer();

   bool IsOk() const noexcept;

   void Bind(Context& context);
   void Unbind(Context& context);

   uint32_t GetWidth() const noexcept;
   uint32_t GetHeight() const noexcept;

private:
   Framebuffer(
      Context& context, Texture& texture, GLuint glTexture,
      GLenum textureTarget, const RectType<uint32_t>& framebufferRect);

   void ReleaseFramebuffer();
   
   Context* mContext { nullptr };

   GLuint mFramebuffer { 0 };

   Observer::Subscription mTextureDestroyedSubscription;
   Observer::Subscription mContextDestroyedSubscription;

   RectType<uint32_t> mFramebufferRect;

   friend class Texture;
};
} // namespace graphics::gl
