/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  PaintTarget.cpp

  Dmitry Vedenko

**********************************************************************/
#include "PaintTarget.h"

#include "Context.h"
#include "GLRenderer.h"
#include "Program.h"
#include "ProgramLibrary.h"
#include "VertexBuffer.h"
#include "VertexArray.h"
#include "Framebuffer.h"
#include "GLFontRenderer.h"
#include "GLRenderer.h"

namespace graphics::gl
{
static constexpr size_t VertexBufferSize = 64 * 1024;

static constexpr size_t MaxVertexCount = VertexBufferSize / sizeof(Vertex);
static constexpr size_t MaxIndexCount = VertexBufferSize / sizeof(IndexType);

namespace
{
struct Batch final
{
   GLenum primitiveMode { GLenum::TRIANGLES };

   GLsizei firstIndex { 0 };
   GLsizei size { 0 };
   
   Context::Snapshot snapshot;

   Batch() = default;
   Batch(const Batch&) = default;
   Batch(Batch&&) = default;
   Batch& operator=(const Batch&) = default;
   Batch& operator=(Batch&&) = default;

   Batch(
      const Context::Snapshot& _snapshot, GLenum _primitiveMode,
      GLsizei _firstIndex)
       : primitiveMode(_primitiveMode)
       , firstIndex(_firstIndex)
       , snapshot(_snapshot)
   {
   }
};
} // namespace

class PaintTarget::StreamTarget final
{
public:
   StreamTarget(GLRenderer& renderer, Context& context)
       : mRenderer(renderer)
       , mContext(context)
   {
      mVertexBuffer = std::make_shared<VertexBuffer>(
         renderer, GLenum::ARRAY_BUFFER, VertexBufferSize);

      mIndexBuffer = std::make_shared<VertexBuffer>(
         renderer, GLenum::ARRAY_BUFFER, VertexBufferSize);

      mVertexArray =
         VertexArrayBuilder(context)
            .AddPointer(
               2, GLenum::FLOAT, false, sizeof(Vertex), mVertexBuffer, 0)
            .AddPointer(
               2, GLenum::SHORT, true, sizeof(Vertex), mVertexBuffer,
               offsetof(Vertex, uv))
            .AddPointer(
               4, GLenum::UNSIGNED_BYTE, true, sizeof(Vertex), mVertexBuffer,
               offsetof(Vertex, mulColor))
            .AddPointer(
               4, GLenum::UNSIGNED_BYTE, true, sizeof(Vertex), mVertexBuffer,
               offsetof(Vertex, addColor))
            .SetIndexBuffer(mIndexBuffer)
            .Build();
   }

   bool CanFit(size_t vertexCount, size_t indexCount) const noexcept
   {
      return mWrittenVerticesCount + vertexCount < MaxVertexCount &&
             mWrittenIndicesCount + indexCount < MaxIndexCount;
   }

   bool Append(
      const VertexTransform& transform, GLenum primitiveMode, const Vertex* vertices,
      size_t vertexCount, const IndexType* indices, size_t indexCount, bool renderToTexture)
   {
      mContext.BindVertexArray(mVertexArray);

      if (!CanFit(vertexCount, indexCount))
         return false;

      auto& indexStream = mIndexBuffer->GetStream();

      if (
         mBatches.empty() || mBatches.back().primitiveMode != primitiveMode ||
         mBatches.back().snapshot != mContext.GetSnapshot())
      {
         mBatches.emplace_back(
            mContext.GetSnapshot(), primitiveMode, mWrittenIndicesCount);
      }
      else if (
         primitiveMode == GLenum::LINE_STRIP ||
         primitiveMode == GLenum::LINE_LOOP ||
         primitiveMode == GLenum::TRIANGLE_STRIP ||
         primitiveMode == GLenum::TRIANGLE_FAN)
      {
         indexStream.Append<IndexType>(mContext, PrimitiveRestartIndex);
         ++mWrittenIndicesCount;
         ++mBatches.back().size;
      }

      const float ymul = renderToTexture ? -1.0f : 1.0f;

      for (size_t i = 0; i < vertexCount; ++i)
      {
         mVertexBuffer->GetStream().Append(
            mContext, transform.TransformedVertex(vertices[i], ymul));
      }

      assert(mWrittenIndicesCount < std::numeric_limits<IndexType>::max());

      const auto indexOffset = static_cast<IndexType>(mWrittenVerticesCount);

      for (size_t i = 0; i < indexCount; ++i)
      {
         auto index = indices[i];

         if (index != PrimitiveRestartIndex)
            index += indexOffset;

         indexStream.Append<IndexType>(mContext, index);
      }

      mWrittenVerticesCount += vertexCount;
      mWrittenIndicesCount += indexCount;
      mBatches.back().size += indexCount;

      return true;
   }

   void FlushBatches()
   {
      if (mBatches.empty())
         return;

      mRenderer.GetFontRenderer().UpdateGPUCache();

      mVertexBuffer->GetStream().Flush(mContext);
      mIndexBuffer->GetStream().Flush(mContext);

      for (const auto& batch : mBatches)
      {
         mContext.SetSnaphot(batch.snapshot);

         mContext.GetFunctions().DrawElements(
            batch.primitiveMode, batch.size, GLenum::UNSIGNED_SHORT,
            reinterpret_cast<const void*>(batch.firstIndex * sizeof(IndexType)));
      }

      mBatches.clear();
   }

   void Reset()
   {
      mVertexBuffer->GetStream().Reset(mContext);
      mIndexBuffer->GetStream().Reset(mContext);

      mWrittenVerticesCount = 0;
      mWrittenIndicesCount = 0;

      mBatches.clear();
   }

private:
   GLRenderer& mRenderer;
   Context& mContext;

   std::shared_ptr<VertexBuffer> mVertexBuffer;
   std::shared_ptr<VertexBuffer> mIndexBuffer;
   
   std::shared_ptr<VertexArray> mVertexArray;

   size_t mWrittenVerticesCount { 0 };
   size_t mWrittenIndicesCount { 0 };

   std::vector<Batch> mBatches;
};

PaintTarget::~PaintTarget()
{
}

bool PaintTarget::Append(
   GLenum primitiveMode, const Vertex* vertices, size_t vertexCount,
   const IndexType* indices, size_t indexCount)
{
   if (mStreamTargets[mCurrentStreamTargetIndex]->Append(
          mCurrentTransform, primitiveMode, vertices, vertexCount, indices,
          indexCount, mFramebuffer != nullptr))
      return true;

   mStreamTargets[mCurrentStreamTargetIndex]->FlushBatches();

   ++mCurrentStreamTargetIndex;

   if (mCurrentStreamTargetIndex == mStreamTargets.size())
      mStreamTargets.emplace_back(std::make_unique<StreamTarget>(mRenderer, mContext));

   return mStreamTargets[mCurrentStreamTargetIndex]->Append(
      mCurrentTransform, primitiveMode, vertices, vertexCount, indices,
      indexCount, mFramebuffer != nullptr);
}

void PaintTarget::SetTransform(const Transform& transform)
{
   mCurrentTransform.mFastTransform = transform;
   mCurrentTransform.mIsFullTransform = false;
}

void PaintTarget::SetTransform(const FullTransform& transform)
{
   mCurrentTransform.mFullTransform = transform;
   mCurrentTransform.mIsFullTransform = true;
}

PaintTarget::PaintTarget(GLRenderer& renderer, Context& context)
    : mRenderer(renderer)
    , mContext(context)
{
   mDefaultProgram =
      renderer.GetProgramLibrary().GetProgram(ProgramID::Default);

   mStreamTargets.emplace_back(std::make_unique<StreamTarget>(renderer, context));
}

void PaintTarget::BeginRendering(
   const std::shared_ptr<Framebuffer>& framebuffer)
{
   mContext.SetPrimitiveRestartIndex(PrimitiveRestartIndex);
   mContext.BindFramebuffer(framebuffer);
   mContext.BindProgram(mDefaultProgram);

   if (framebuffer != nullptr)
   {
      mCurrentTransform.mViewportWidth =
         static_cast<float>(framebuffer->GetWidth());
      mCurrentTransform.mViewportHeight =
         static_cast<float>(framebuffer->GetHeight());
   }
   else
   {
      const auto size = mContext.GetSize();

      mCurrentTransform.mViewportWidth = size.width;
      mCurrentTransform.mViewportHeight = size.height;
   }

   mContext.GetFunctions().Viewport(
      0, 0, mCurrentTransform.mViewportWidth,
      mCurrentTransform.mViewportHeight);

   mContext.SetViewport(RectType<uint32_t> {
      {},
      { static_cast<uint32_t>(mCurrentTransform.mViewportWidth),
        static_cast<uint32_t>(mCurrentTransform.mViewportHeight) } });

   mContext.ResetClipRect();
   mContext.Clear();

   mFramebuffer = framebuffer;

   mRenderer.GetFontRenderer().SetCurrentPaintTarget(*this, mContext);
}

void PaintTarget::EndRendering()
{
   mStreamTargets[mCurrentStreamTargetIndex]->FlushBatches();

   mRenderer.GetFontRenderer().ResetPaintTarget();

   for (size_t i = 0; i <= mCurrentStreamTargetIndex; ++i)
      mStreamTargets[i]->Reset();

   mCurrentStreamTargetIndex = 0;
}

void PaintTarget::RestartRendering()
{
   BeginRendering(mFramebuffer);
}

void PaintTarget::VertexTransform::SetTransform(const Transform& transform)
{
   mFastTransform = transform;
   mIsFullTransform = false;
}

void PaintTarget::VertexTransform::SetTransform(const FullTransform& transform)
{
   mFullTransform = transform;
   mIsFullTransform = true;
}

Vertex PaintTarget::VertexTransform::TransformedVertex(Vertex input, float yMul) const noexcept
{
   yMul = 1;
   const auto transformedPoint = mIsFullTransform ? mFullTransform.Apply(input.pos) :
                                      mFastTransform.Apply(input.pos);

   input.pos = { 2.0f * transformedPoint.x / mViewportWidth - 1.0f,
                 yMul*(1.0f - 2.0f * transformedPoint.y / mViewportHeight) };

   return input;
}

} // namespace graphics::gl
