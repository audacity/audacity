/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Texture.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <memory>

#include "graphics/Painter.h"

#include "GLFunctions.h"

#include "Observer.h"

namespace graphics::gl
{

class GLRenderer;
class Context;
class Framebuffer;

using FramebufferPtr = std::shared_ptr<Framebuffer>;

struct TextureDestroyedMessage : Observer::Message {};

class Texture /* not final */ :
    public PainterImage,
    public std::enable_shared_from_this<Texture>
{
public:
   struct TextureCoords final
   {
      int16_t left;
      int16_t top;
      int16_t right;
      int16_t bottom;
   };

   Texture(
      GLRenderer& renderer, uint32_t width, uint32_t height,
      PainterImageFormat format, bool isStatic, const void* data = nullptr);

   Texture(const Texture& baseTexture, const RectType<uint32_t>& rect);

   ~Texture();

   virtual void Bind(Context& ctx, const Texture* texture);

   virtual bool IsDirty() const;
   virtual void PerformUpdate(Context& ctx);

   uint32_t GetWidth() const override;
   uint32_t GetHeight() const override;

   virtual TextureCoords
   Update(const RectType<uint32_t>& rect, const void* data);

   FramebufferPtr GetFramebuffer(Context& context);

   Observer::Publisher<TextureDestroyedMessage>&
   GetTextureDestroyedMessagePublisher();

   TextureCoords GetTextureCoords(const RectType<uint32_t>& rect) const noexcept;

   bool UsesSameObject(const Texture& other) const noexcept;
private:

   class SharedGLObject;
   std::shared_ptr<SharedGLObject> mSharedGLObject;

   RectType<uint32_t> mTextureRect;
};

} // namespace graphics::gl
