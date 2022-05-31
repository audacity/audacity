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

VertexBuffer::VertexBuffer(
   GLRenderer& renderer, GLenum target, size_t bufferSize)
    : mRenderer(renderer)
    , mSize(bufferSize)
    , mTarget(target)
    , mBufferStream(*this)
{
   auto& context = mRenderer.GetResourceContext();
   auto& functions = context.GetFunctions();

   functions.GenBuffers(1, &mBuffer);

   context.BindBuffer(*this);
   functions.BufferData(GLenum::ARRAY_BUFFER, mSize, nullptr, GLenum::DYNAMIC_DRAW);

   mRendererDestroyedSubscription = mRenderer.Subscribe([this](RendererDestroyedMessage) {
      mBuffer = 0;
   });
}

VertexBuffer::~VertexBuffer()
{
   if (mBuffer == 0)
      return;

   mBufferStream.Reset(mRenderer.GetResourceContext());

   auto& functions = mRenderer.GetResourceContext().GetFunctions();
   functions.DeleteBuffers(1, &mBuffer);
}

size_t VertexBuffer::GetSize() const noexcept
{
   return mSize;
}

GLenum VertexBuffer::GetBufferTarget() const noexcept
{
   return mTarget;
}

VertexBufferStream& VertexBuffer::GetStream() noexcept
{
   return mBufferStream;
}

void VertexBuffer::Bind(Context& context) const noexcept
{
   auto& functions = context.GetFunctions();
   functions.BindBuffer(mTarget, mBuffer);
}

void VertexBuffer::Bind(Context& context, GLenum target) const noexcept
{
   auto& functions = context.GetFunctions();
   functions.BindBuffer(target, mBuffer);
}

bool VertexBufferStream::Append(Context& context, const void* data, size_t size)
{
   if (mOffset + size >= mSize)
      return false;

   if (mPointer == nullptr && !Map(context))
      return false;

   std::memcpy(mPointer + mOffset, data, size);

   mOffset += size;

   return true;
}

void VertexBufferStream::Flush(Context& context)
{
   if (mPointer == nullptr)
      return;

   auto& functions = context.GetFunctions();

   context.BindBuffer(mParent);

   functions.FlushMappedBufferRange(
      mParent.GetBufferTarget(), mFirstMappedByte, mOffset - mFirstMappedByte);

   functions.UnmapBuffer(mParent.GetBufferTarget());

   mPointer = nullptr;
}

void VertexBufferStream::Reset(Context& context)
{
   if (mPointer != nullptr)
   {
      context.BindBuffer(mParent);
      context.GetFunctions().UnmapBuffer(mParent.GetBufferTarget());
      mPointer = nullptr;
   }

   mOffset = 0;
   mFirstMappedByte = 0;
}

size_t VertexBufferStream::GetBytesLeft() const noexcept
{
   return mSize - mOffset;
}

VertexBufferStream::VertexBufferStream(VertexBuffer& parent)
    : mParent(parent)
    , mSize(parent.GetSize())
{
}

VertexBufferStream::~VertexBufferStream()
{
}

bool VertexBufferStream::Map(Context& context)
{
   context.BindBuffer(mParent);

   auto& functions = context.GetFunctions();

   const GLbitfield access =
      static_cast<GLbitfield>(GLenum::MAP_WRITE_BIT) |
      static_cast<GLbitfield>(GLenum::MAP_FLUSH_EXPLICIT_BIT);

   mPointer = static_cast<uint8_t*>(functions.MapBufferRange(
      mParent.GetBufferTarget(), 0, mSize, access));

   mFirstMappedByte = mOffset;

   return mPointer != nullptr;
}

} // namespace graphics::gl
