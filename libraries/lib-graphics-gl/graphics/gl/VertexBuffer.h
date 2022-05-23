/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  VertexBuffer.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <cstring>
#include <numeric>
#include <vector>

#include "GLFunctions.h"
#include "Observer.h"

namespace graphics::gl
{

class GLRenderer;
class Context;
class VertexBuffer;

class VertexBufferStream final
{
public:
   template<typename T>
   bool Append(Context& context, T value)
   {
      constexpr size_t size = sizeof(value);

      if (mOffset + size >= mSize)
         return false;

      if (mPointer == nullptr && !Map(context))
         return false;

      void* ptr = mPointer + mOffset;
      *static_cast<T*>(ptr) = value;

      mOffset += size;

      return true;
   }

   bool Append(Context& context, const void* data, size_t size);

   void Flush(Context& context);
   void Reset(Context& context);

   size_t GetBytesLeft() const noexcept;

private:
   explicit VertexBufferStream(VertexBuffer& parent);
   ~VertexBufferStream();

   bool Map(Context& context);

   VertexBuffer& mParent;

   uint8_t* mPointer { nullptr };

   size_t mOffset { 0 };
   size_t mSize { 0 };

   size_t mFirstMappedByte { 0 };

   friend class VertexBuffer;
};

class VertexBuffer final
{
public:
   VertexBuffer(GLRenderer& renderer, GLenum target, size_t bufferSize);
   ~VertexBuffer();

   VertexBuffer(const VertexBuffer&) = delete;
   VertexBuffer(VertexBuffer&&) = delete;
   VertexBuffer& operator=(const VertexBuffer&) = delete;
   VertexBuffer& operator=(const VertexBuffer&&);

   size_t GetSize() const noexcept;

   GLenum GetBufferTarget() const noexcept;

   VertexBufferStream& GetStream() noexcept;

private:
   void Bind(Context& context) const noexcept;
   void Bind(Context& context, GLenum target) const noexcept;

   GLRenderer& mRenderer;

   GLuint mBuffer { 0 };
   size_t mSize { 0 };

   GLenum mTarget { GLenum::ARRAY_BUFFER };

   VertexBufferStream mBufferStream;

   Observer::Subscription mRendererDestroyedSubscription;

   friend class Context;
   friend class VertexArrayBuilder;
};
   
}
