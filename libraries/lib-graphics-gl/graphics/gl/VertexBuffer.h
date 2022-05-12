/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  VertexBuffer.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <vector>

#include "GLFunctions.h"

namespace graphics::gl
{

class GLRenderer;
class Context;

class VertexBuffer final
{
public:
   VertexBuffer(GLRenderer& renderer, size_t bufferSize);
   ~VertexBuffer();

   size_t GetSize() const noexcept;

   bool Update(size_t initialOffset, const void* data, size_t size);

   void Bind(Context& context, GLenum target);

   bool IsDirty() const;
   void PerformUpdate(Context& context);

private:
   GLRenderer& mRenderer;

   GLuint mBuffer { 0 };

   std::vector<uint8_t> mBufferData;

   GLenum mCurrentTarget { GLenum::INVALID };
   bool mDirty { true };
};
   
}
