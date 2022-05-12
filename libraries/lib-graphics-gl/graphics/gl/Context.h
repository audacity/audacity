/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Context.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <array>
#include <memory>
#include <vector>

#include "graphics/Color.h"
#include "GLFunctions.h"

#include "Observer.h"

namespace graphics::gl
{
class Texture;
using TexturePtr = std::shared_ptr<Texture>;

class VertexArray;
using VertexArrayPtr = std::shared_ptr<VertexArray>;

class Framebuffer;
using FramebufferPtr = std::shared_ptr<Framebuffer>;

struct ContextDestroyedMessage : Observer::Message {};

enum class ContextResourceType
{
   Framebuffer,
};

class Context /* not final */ :
    public Observer::Publisher<ContextDestroyedMessage>
{
public:
   static constexpr size_t MAX_TEXTURE_UNITS = 2;
   
   explicit Context(GLFunctions& functions);
   virtual ~Context();
   
   const GLFunctions& GetFunctions() const;

   virtual void Clear(Color color);

   TexturePtr GetCurrentTexture(uint32_t textureUnitIndex) const;
   bool BindTexture(const TexturePtr& texture, uint32_t textureUnitIndex);

   void ResetVertexArrayState();
   bool BindVertexArray(const VertexArrayPtr& vertexArray);

   void ReleaseContextResource(ContextResourceType framebuffer, GLuint resourceId);

protected:
   virtual void SetupContext();
   void DoProcessReleaseQueue();
   virtual void ProcessReleaseQueue() = 0;

private:
   GLFunctions& mFunctions;

   Color mClearColor;

   std::array<TexturePtr, 2> mCurrentTexture;
   GLenum mCurrentClientTexture { GLenum::TEXTURE0 };

   VertexArrayPtr mCurrentVertexArray;

   std::vector<std::pair<ContextResourceType, GLuint>> mReleaseQueue;
}; // class Context;
} // namespace graphics::gl
