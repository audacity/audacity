/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  VertexArray.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <memory>
#include <vector>

#include "GLFunctions.h"

namespace graphics::gl
{
class GLRenderer;
class Context;

class VertexBuffer;
using VertexBufferPtr = std::shared_ptr<VertexBuffer>;

class VertexArray final
{
public:
   ~VertexArray();

   bool IsDirty() const noexcept;
   void PerformUpdate(Context& context);
   
   void Bind(Context& context);

private:
   VertexArray(
      GLRenderer& renderer, std::vector<VertexBufferPtr> buffers, GLuint vao);
   
   GLRenderer& mRenderer;

   std::vector<VertexBufferPtr> mAssignedBuffers;
   GLuint mVertexArray { 0 };

   friend class VertexArrayBuilder;
};

class VertexArrayBuilder final
{
public:
   explicit VertexArrayBuilder(GLRenderer& renderer);
   ~VertexArrayBuilder();

   std::shared_ptr<VertexArray> Build();

   VertexArrayBuilder& AddPointer(
      GLint componentsCount, GLenum type, GLboolean normalized, GLsizei stride,
      const VertexBufferPtr& buffer, size_t offset);

private:
   GLRenderer& mRenderer;
   const GLFunctions& mFunctions;

   std::vector<VertexBufferPtr> mAssignedBuffers;
   VertexBufferPtr mCurrentBuffer;

   GLuint mVertexArray { 0 };

   uint32_t mAttribIndex { 0 };
};

}
