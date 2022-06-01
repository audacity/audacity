/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  PaintTarget.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <memory>
#include <vector>

#include "graphics/Point.h"
#include "graphics/Transform.h"
#include "graphics/Color.h"
#include "graphics/Brush.h"

#include "GLFunctions.h"

namespace graphics::gl
{

class GLRenderer;
class Context;
class Framebuffer;

class Texture;
using TexturePtr = std::shared_ptr<Texture>;

class Program;
using ProgramPtr = std::shared_ptr<Program>;
class ProgramConstants;
using ProgramConstantsPtr = std::shared_ptr<ProgramConstants>;

class VertexArray;
using VertexArrayPtr = std::shared_ptr<VertexArray>;

class VertexBuffer;

using IndexType = uint16_t;
static constexpr IndexType PrimitiveRestartIndex =
   std::numeric_limits<uint16_t>::max();

struct Vertex final
{
   Point pos;
   PointType<int16_t> uv;

   Color mulColor;
   Color addColor;
};

class PaintTarget final
{
public:
   ~PaintTarget();

   bool Append(
      GLenum primitiveMode, const Vertex* vertices, size_t vertexCount,
      const IndexType* indices, size_t indexCount);

   void SetTransform(const Transform& transform);
   void SetTransform(const FullTransform& transform);

   void SetDefaultShader();
   void SetupShadersForBrush(const Brush& brush);

   Size GetSize() const noexcept;

   void SetProgram(const ProgramPtr& program, const ProgramConstantsPtr& constants);
   void SetVertexArray(const VertexArrayPtr& vertexArray);
   void SetTexture(const TexturePtr& texture);
   void EnableClipping(const Rect& rect);
   void DisableClipping();

private:
   explicit PaintTarget(GLRenderer& renderer, Context& context);

   void BeginRendering(const std::shared_ptr<Framebuffer>& framebuffer);
   void EndRendering();
   void RestartRendering();

   GLRenderer& mRenderer;
   Context& mContext;

   std::shared_ptr<Program> mDefaultProgram;

   class StreamTarget;
   std::vector<std::unique_ptr<StreamTarget>> mStreamTargets;
   size_t mCurrentStreamTargetIndex { 0 };

   struct VertexTransform final
   {
      Transform mFastTransform;
      FullTransform mFullTransform;

      float mViewportWidth { 0.0f };
      float mViewportHeight { 0.0f };

      bool mIsFullTransform { false };

      void SetTransform(const Transform& transform);
      void SetTransform(const FullTransform& transform);

      Vertex TransformedVertex(Vertex input, float yMult) const noexcept;
   };

   std::shared_ptr<Framebuffer> mFramebuffer;

   VertexTransform mCurrentTransform;

   class GradientBrushesCache;
   std::unique_ptr<GradientBrushesCache> mGradientBrushesCache;

   friend class PaintTargetsStack;
};
}
