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
#include "Texture.h"

#include "GraphicsObjectCache.h"

namespace graphics::gl
{
static constexpr size_t VertexBufferSize = 64 * 1024;

static constexpr size_t MaxVertexCount = VertexBufferSize / sizeof(Vertex);
static constexpr size_t MaxIndexCount = VertexBufferSize / sizeof(IndexType);

namespace
{
struct BatchState final
{
   ProgramPtr program;
   ProgramConstantsPtr programConstants;
   size_t programConstantsVersion { 0 };

   VertexArrayPtr vao;

   TexturePtr texture;

   GLenum primitiveMode { GLenum::TRIANGLES };

   RectType<GLint> clipRect;
   bool clippingEnabled { false };
};

struct Batch final
{
   GLsizei firstIndex { 0 };
   GLsizei size { 0 };

   BatchState state;

   Batch() = default;
   Batch(const Batch&) = default;
   Batch(Batch&&) = default;
   Batch& operator=(const Batch&) = default;
   Batch& operator=(Batch&&) = default;

   Batch(const BatchState& _state, GLsizei _firstIndex)
      : firstIndex(_firstIndex)
      , state(_state)
   {}
};

struct LinearGradientBrush final
{
   std::shared_ptr<Program> program;
   std::shared_ptr<ProgramConstants> constants;
};

LinearGradientBrush GetLinearGradientBrush(GLRenderer& renderer, const Brush& brush)
{
   LinearGradientBrush result;

   result.constants = std::make_shared<ProgramConstants>();

   auto gradientData = brush.GetGradientData();
   const auto stopsCount = gradientData->stops.size();

   std::vector<Color> colors;
   std::vector<float> stops;

   colors.reserve(stopsCount);
   stops.reserve(stopsCount);

   for (size_t i = 0; i < stopsCount; ++i)
   {
      colors.push_back(gradientData->stops[i].color);
      stops.push_back(gradientData->stops[i].position);
   }

   result.constants->SetUniform("c_GradientStart", gradientData->firstPoint.x, gradientData->firstPoint.y);
   result.constants->SetUniform("c_GradientEnd", gradientData->secondPoint.x, gradientData->secondPoint.y);

   result.constants->SetUniform("c_Colors", stopsCount, colors.data());
   result.constants->SetUniform("c_Stops", ProgramConstants::UniformSize::Scalar, stopsCount, stops.data());

   result.program = renderer.GetProgramLibrary().GetLinearGradientProgram(stopsCount);

   return result;
}

size_t GetProgramConstantsVersion(const ProgramConstantsPtr& constants)
{
   return constants != nullptr ? constants->GetVersion() : 0;
}

bool IsSameTexture(const TexturePtr& lhs, const TexturePtr& rhs)
{
   if (lhs == rhs)
      return true;

   if (lhs != nullptr && rhs != nullptr)
      return lhs->UsesSameObject(*rhs);

   return false;
}
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

   void SetProgram(const ProgramPtr& program, const ProgramConstantsPtr& programConstants)
   {
      const auto& prevBatchState = GetLastBatchState();

      if (prevBatchState.program == program &&
          prevBatchState.programConstants == programConstants &&
          GetProgramConstantsVersion(prevBatchState.programConstants) ==
             GetProgramConstantsVersion(programConstants))
         return;

      auto& newBatchState = GetMutableBatchState();

      newBatchState.program = program;
      newBatchState.programConstants = programConstants;
      newBatchState.programConstantsVersion = GetProgramConstantsVersion(programConstants);
   }

   void SetVertexArray(const VertexArrayPtr& vao)
   {
      if (GetLastBatchState().vao == vao)
         return ;

      GetMutableBatchState().vao = vao;
   }

   void SetTexture(const TexturePtr& texture)
   {
      if (IsSameTexture(GetLastBatchState().texture, texture))
         return ;

      GetMutableBatchState().texture = texture;
   }

   void SetPrimitiveMode(GLenum primitiveMode)
   {
      if (GetLastBatchState().primitiveMode == primitiveMode)
         return ;

      GetMutableBatchState().primitiveMode = primitiveMode;
   }

   void SetClippingEnabled(bool enabled)
   {
      if (GetLastBatchState().clippingEnabled == enabled)
         return ;

      GetMutableBatchState().clippingEnabled = enabled;
   }

   void SetInitialBatchState(const BatchState& state)
   {
      if (mBatches.empty())
         mBatches.emplace_back(state, 0);
   }

   void SetClipRect(const RectType<GLint>& clipRect)
   {
      if (GetLastBatchState().clipRect == clipRect)
         return ;

      GetMutableBatchState().clipRect = clipRect;
   }

   bool CanFit(size_t vertexCount, size_t indexCount) const noexcept
   {
      return mWrittenVerticesCount + vertexCount < MaxVertexCount &&
             mWrittenIndicesCount + indexCount < MaxIndexCount;
   }

   bool Append(
      const VertexTransform& transform, GLenum primitiveMode, const Vertex* vertices,
      size_t vertexCount, const IndexType* indices, size_t indexCount, bool flipY)
   {
      if (!CanFit(vertexCount, indexCount))
         return false;

      auto& indexStream = mIndexBuffer->GetStream();

      SetPrimitiveMode(primitiveMode);

      if (mBatches.back().size != 0 && (
         primitiveMode == GLenum::LINE_STRIP ||
         primitiveMode == GLenum::LINE_LOOP ||
         primitiveMode == GLenum::TRIANGLE_STRIP ||
         primitiveMode == GLenum::TRIANGLE_FAN))
      {
         indexStream.Append<IndexType>(mContext, PrimitiveRestartIndex);
         ++mWrittenIndicesCount;
         ++mBatches.back().size;
      }

      const float ymul = flipY ? -1.0f : 1.0f;

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
         mContext.BindProgram(batch.state.program, batch.state.programConstants);
         mContext.BindVertexArray(batch.state.vao);
         mContext.BindTexture(batch.state.texture, 0);
         if(batch.state.clippingEnabled)
            mContext.SetClipRect(batch.state.clipRect);
         else
            mContext.ResetClipRect();

         mContext.GetFunctions().DrawElements(
            batch.state.primitiveMode, batch.size, GLenum::UNSIGNED_SHORT,
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

   const BatchState& GetLastBatchState()
   {
      if (mBatches.empty())
         GenerateFirstBatch();

      return mBatches.back().state;
   }
private:
   void GenerateFirstBatch()
   {
      mBatches.emplace_back();
      mBatches.back().state.vao = mVertexArray;
   }

   BatchState& GetMutableBatchState()
   {
      if (mBatches.empty())
         GenerateFirstBatch();
      else if (mBatches.back().size != 0)
         mBatches.emplace_back(GetLastBatchState(), mWrittenIndicesCount);

      return mBatches.back().state;
   }

   GLRenderer& mRenderer;
   Context& mContext;

   std::shared_ptr<VertexBuffer> mVertexBuffer;
   std::shared_ptr<VertexBuffer> mIndexBuffer;

   std::shared_ptr<VertexArray> mVertexArray;

   size_t mWrittenVerticesCount { 0 };
   size_t mWrittenIndicesCount { 0 };

   std::vector<Batch> mBatches;
};

class PaintTarget::GradientBrushesCache final
{
public:
   explicit GradientBrushesCache(GLRenderer& renderer)
       : mLinearGradientBrushesCache(
            [&renderer](const Brush& brush)
            { return GetLinearGradientBrush(renderer, brush); })
   {
   }

   LinearGradientBrush GetBrush(const Brush& brush)
   {
      return mLinearGradientBrushesCache.Get(brush);
   }

private:
   GraphicsObjectCache<Brush, LinearGradientBrush, 8, false> mLinearGradientBrushesCache;
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
          indexCount, mFramebuffer == nullptr && mContext.HasFlippedY()))
      return true;

   auto batchState = mStreamTargets[mCurrentStreamTargetIndex]->GetLastBatchState();

   mStreamTargets[mCurrentStreamTargetIndex]->FlushBatches();

   ++mCurrentStreamTargetIndex;

   if (mCurrentStreamTargetIndex == mStreamTargets.size())
      mStreamTargets.emplace_back(std::make_unique<StreamTarget>(mRenderer, mContext));

   mStreamTargets[mCurrentStreamTargetIndex]->SetInitialBatchState(batchState);

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

void PaintTarget::SetDefaultShader()
{
   SetProgram(mDefaultProgram, nullptr);
}

void PaintTarget::SetupShadersForBrush(const Brush& brush)
{
   if (brush.GetStyle() == BrushStyle::LinearGradient)
   {
      if (mGradientBrushesCache == nullptr)
         mGradientBrushesCache =
            std::make_unique<GradientBrushesCache>(mRenderer);

      auto gradientBrush = mGradientBrushesCache->GetBrush(brush);

      SetProgram(gradientBrush.program, gradientBrush.constants);
   }
   else
   {
      SetDefaultShader();
   }
}

Size PaintTarget::GetSize() const noexcept
{
   return { mCurrentTransform.mViewportWidth,
            mCurrentTransform.mViewportHeight };
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

   mContext.SetViewport(RectType<uint32_t> {
      {},
      { static_cast<uint32_t>(mCurrentTransform.mViewportWidth),
        static_cast<uint32_t>(mCurrentTransform.mViewportHeight) } });

   mContext.ResetClipRect();
   mContext.Clear();

   mFramebuffer = framebuffer;

   mRenderer.GetFontRenderer().SetCurrentPaintTarget(*this, mContext);
   SetProgram(mDefaultProgram, nullptr);
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

void PaintTarget::SetProgram(
   const ProgramPtr& program, const ProgramConstantsPtr& constants)
{
   mStreamTargets[mCurrentStreamTargetIndex]->SetProgram(program, constants);
}

void PaintTarget::SetVertexArray(const VertexArrayPtr& vertexArray)
{
   mStreamTargets[mCurrentStreamTargetIndex]->SetVertexArray(vertexArray);
}

void PaintTarget::SetTexture(const TexturePtr& texture)
{
   mStreamTargets[mCurrentStreamTargetIndex]->SetTexture(texture);
}

void PaintTarget::EnableClipping(const Rect& rect)
{
   mStreamTargets[mCurrentStreamTargetIndex]->SetClipRect(rect_cast<GLint>(rect));
   mStreamTargets[mCurrentStreamTargetIndex]->SetClippingEnabled(true);
}

void PaintTarget::DisableClipping()
{
   mStreamTargets[mCurrentStreamTargetIndex]->SetClippingEnabled(false);
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
   const auto transformedPoint = mIsFullTransform ? mFullTransform.Apply(input.pos) :
                                      mFastTransform.Apply(input.pos);

   input.pos = { 2.0f * transformedPoint.x / mViewportWidth - 1.0f,
                 yMul*(1.0f - 2.0f * transformedPoint.y / mViewportHeight) };

   return input;
}

} // namespace graphics::gl
