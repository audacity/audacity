/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  VertexArray.cpp

  Dmitry Vedenko

**********************************************************************/
#include "VertexArray.h"

#include <algorithm>

#include "GLRenderer.h"
#include "Context.h"
#include "VertexBuffer.h"

namespace graphics::gl
{

VertexArray::~VertexArray()
{
   if (mVertexArray != 0)
   {
      mContext.GetFunctions().DeleteVertexArrays(
         1, &mVertexArray);
   }
}

void VertexArray::Bind(Context& context)
{
   context.GetFunctions().BindVertexArray(mVertexArray);
}

VertexArray::VertexArray(
   Context& context, std::vector<VertexBufferPtr> buffers, GLuint vao)
    : mContext(context)
    , mAssignedBuffers(std::move(buffers))
    , mVertexArray(vao)
    , mContextDestroyedSubscription(context.Subscribe(
         [this](ContextDestroyedMessage) { mVertexArray = 0; }))
{
}

VertexArrayBuilder::VertexArrayBuilder(Context& context)
    : mContext(context)
    , mFunctions(context.GetFunctions())
{
   context.ResetVertexArrayState();

   mFunctions.GenVertexArrays(1, &mVertexArray);
   mFunctions.BindVertexArray(mVertexArray);
}

VertexArrayBuilder::~VertexArrayBuilder()
{
   if (mVertexArray != 0)
      mFunctions.DeleteVertexArrays(1, &mVertexArray);
}

std::shared_ptr<VertexArray> VertexArrayBuilder::Build()
{
   auto vao = std::shared_ptr<VertexArray>(
      new VertexArray(mContext, std::move(mAssignedBuffers), mVertexArray));

   mVertexArray = 0;
   mCurrentBuffer = {};

   return vao;
}

VertexArrayBuilder& VertexArrayBuilder::AddPointer(
   GLint componentsCount, GLenum type, GLboolean normalized, GLsizei stride,
   const VertexBufferPtr& buffer, size_t offset)
{
   if (buffer == nullptr)
   {
      ++mAttribIndex;
      return *this;
   }

   if (mCurrentBuffer != buffer)
   {
      mCurrentBuffer = buffer;
      mContext.BindBuffer(*buffer);
   }

   mFunctions.EnableVertexAttribArray(mAttribIndex);

   mFunctions.VertexAttribPointer(
      mAttribIndex, componentsCount, type, normalized, stride,
      reinterpret_cast<const void*>(offset));

   ++mAttribIndex;

   return *this;
}
VertexArrayBuilder&
VertexArrayBuilder::SetIndexBuffer(const VertexBufferPtr& buffer)
{
   if (buffer == nullptr)
      return *this;

   mAssignedBuffers.emplace_back(buffer);
   buffer->Bind(mContext, GLenum::ELEMENT_ARRAY_BUFFER);

   return *this;
}
} // namespace graphics::gl
