/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  VertexBuffer.cpp

  Dmitry Vedenko

**********************************************************************/

#include "VertexBuffer.h"

#include <cstring>

#include "GLRenderer.h"
#include "Context.h"

namespace graphics::gl
{

VertexBuffer::VertexBuffer(GLRenderer& renderer, size_t bufferSize)
    : mRenderer(renderer)
{
   auto& functions = mRenderer.GetResourceContext().GetFunctions();
   functions.GenBuffers(1, &mBuffer);

   mBufferData.resize(bufferSize);
}

VertexBuffer::~VertexBuffer()
{
   if (mBuffer == 0)
      return;
   
   auto& functions = mRenderer.GetResourceContext().GetFunctions();
   functions.DeleteBuffers(1, &mBuffer);
}

size_t VertexBuffer::GetSize() const noexcept
{
   return mBufferData.size();
}

bool VertexBuffer::Update(size_t initialOffset, const void* data, size_t size)
{
   if (initialOffset + size > mBufferData.size())
      return false;

   mDirty = true;
   
   std::memcpy(mBufferData.data() + initialOffset, data, size);
   
   return true;
}

void VertexBuffer::Bind(Context& context, GLenum target)
{
   auto& functions = context.GetFunctions();

   functions.BindBuffer(target, mBuffer);

   mCurrentTarget = target;
}

bool VertexBuffer::IsDirty() const
{
   return mDirty;
}

void VertexBuffer::PerformUpdate(Context& context)
{
   if (!mDirty || mCurrentTarget == GLenum::INVALID)
      return ;

   mDirty = false;

   auto& functions = context.GetFunctions();

   functions.BufferData(
      mCurrentTarget, mBufferData.size(), nullptr,
      GLenum::STREAM_DRAW);

   functions.BufferData(
      mCurrentTarget, mBufferData.size(), mBufferData.data(),
      GLenum::STREAM_DRAW);
}

} // namespace graphics::gl
