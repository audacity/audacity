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
   if (mVertexArray !=0)
   {
      mRenderer.GetResourceContext().GetFunctions().DeleteVertexArrays(
         1, &mVertexArray);
   }
}

bool VertexArray::IsDirty() const noexcept
{
   return std::any_of(
      mAssignedBuffers.begin(), mAssignedBuffers.end(),
      [](const auto& buffer) { return buffer->IsDirty(); });
}

void VertexArray::PerformUpdate(Context& context)
{
   for (auto& buffer : mAssignedBuffers)
   {
      if (buffer->IsDirty())
         buffer->PerformUpdate(context);
   }
}

void VertexArray::Bind(Context& context)
{
   context.GetFunctions().BindVertexArray(mVertexArray);
}

VertexArray::VertexArray(
   GLRenderer& renderer, std::vector<VertexBufferPtr> buffers, GLuint vao)
    : mRenderer(renderer)
    , mAssignedBuffers(std::move(buffers))
    , mVertexArray(vao)
{
}

VertexArrayBuilder::VertexArrayBuilder(GLRenderer& renderer)
    : mRenderer(renderer)
    , mFunctions(renderer.GetResourceContext().GetFunctions())
{
   mRenderer.GetResourceContext().ResetVertexArrayState();

   mFunctions.GenVertexArrays(1, &mVertexArray);
   mFunctions.BindVertexArray(mVertexArray);
}

VertexArrayBuilder::~VertexArrayBuilder()
{
   if (mVertexArray != 0)
   {
      mRenderer.GetResourceContext().GetFunctions().DeleteVertexArrays(
         1, &mVertexArray);
   }
}

std::shared_ptr<VertexArray> VertexArrayBuilder::Build()
{
   auto vao = std::shared_ptr<VertexArray>(
      new VertexArray(mRenderer, std::move(mAssignedBuffers), mVertexArray));

   mVertexArray = 0;
   mCurrentBuffer = {};

   return vao;
}

VertexArrayBuilder& VertexArrayBuilder::AddPointer(
   GLint componentsCount, GLenum type, GLboolean normalized, GLsizei stride,
   const VertexBufferPtr& buffer, size_t offset)
{
   auto& context = mRenderer.GetResourceContext();
   
   if (mCurrentBuffer != buffer)
   {
      mCurrentBuffer = buffer;
      mCurrentBuffer->Bind(context, GLenum::ARRAY_BUFFER);
   }

   mFunctions.EnableVertexAttribArray(mAttribIndex);
   
   mFunctions.VertexAttribPointer(
      mAttribIndex, componentsCount, type, normalized, stride,
      (const void*)offset);

   ++mAttribIndex;
   
   return *this;
}
} // namespace graphics::gl
