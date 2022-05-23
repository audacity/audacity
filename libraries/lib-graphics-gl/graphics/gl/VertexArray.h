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

#include "Observer.h"

namespace graphics::gl
{
class Context;

class VertexBuffer;
using VertexBufferPtr = std::shared_ptr<VertexBuffer>;

class VertexArray final
{
public:
   ~VertexArray();
   
   void Bind(Context& context);

private:
   VertexArray(
      Context& context, std::vector<VertexBufferPtr> buffers, GLuint vao);
   
   Context& mContext;

   std::vector<VertexBufferPtr> mAssignedBuffers;
   GLuint mVertexArray { 0 };

   Observer::Subscription mContextDestroyedSubscription;

   friend class VertexArrayBuilder;
};

class VertexArrayBuilder final
{
public:
   explicit VertexArrayBuilder(Context& context);
   ~VertexArrayBuilder();

   std::shared_ptr<VertexArray> Build();

   VertexArrayBuilder& AddPointer(
      GLint componentsCount, GLenum type, GLboolean normalized, GLsizei stride,
      const VertexBufferPtr& buffer, size_t offset);

   VertexArrayBuilder& SetIndexBuffer(const VertexBufferPtr& buffer);

private:
   Context& mContext;
   const GLFunctions& mFunctions;

   std::vector<VertexBufferPtr> mAssignedBuffers;
   VertexBufferPtr mCurrentBuffer;

   GLuint mVertexArray { 0 };

   uint32_t mAttribIndex { 0 };
};

}
