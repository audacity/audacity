/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Context.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <array>
#include <cstdint>
#include <vector>

#include "graphics/Color.h"
#include "GLFunctions.h"

#include "graphics/Rect.h"

#include "Observer.h"

namespace graphics::gl
{
class Texture;
using TexturePtr = std::shared_ptr<Texture>;

class VertexArray;
using VertexArrayPtr = std::shared_ptr<VertexArray>;

class Framebuffer;
using FramebufferPtr = std::shared_ptr<Framebuffer>;

class VertexBuffer;

class Program;
using ProgramPtr = std::shared_ptr<Program>;

class ProgramConstants;
using ProgramConstantsPtr = std::shared_ptr<ProgramConstants>;

struct ContextDestroyedMessage : Observer::Message {};

enum class ContextResourceType
{
   Framebuffer,
};

class Context /* not final */ :
    public Observer::Publisher<ContextDestroyedMessage>
{
public:
   static constexpr size_t MAX_TEXTURE_UNITS = 2;

   explicit Context(const GLFunctions& functions);
   virtual ~Context();

   virtual Size GetSize() const = 0;

   const GLFunctions& GetFunctions() const;

   virtual void Clear(const Rect& rect, Color color);
   virtual void Clear();

   TexturePtr GetCurrentTexture(uint32_t textureUnitIndex) const;
   void BindTexture(const TexturePtr& texture, uint32_t textureUnitIndex);

   void ResetVertexArrayState();
   void BindVertexArray(const VertexArrayPtr& vertexArray);

   void ReleaseContextResource(ContextResourceType framebuffer, GLuint resourceId);

   void BindBuffer(const VertexBuffer& buffer);

   void BindProgram(const ProgramPtr& program, const ProgramConstantsPtr& constants);

   void BindFramebuffer(const FramebufferPtr& framebuffer);
   FramebufferPtr GetCurrentFramebuffer() const;

   void SetClipRect(const Rect& rect);
   void SetClipRect(const RectType<GLint>& rect);
   void ResetClipRect();

   void SetClientActiveTexture(uint32_t unit);

   void SetPrimitiveRestartIndex(GLuint index);

   void SetViewport(const RectType<uint32_t> viewport);

   void SetUnpackAlignment(uint32_t alignment);
   void SetBestUnpackAlignment(uint32_t rowStride);

   class Snapshot final
   {
   public:
      Snapshot() = default;
      Snapshot(const Snapshot&) = default;
      Snapshot(Snapshot&&) = default;
      Snapshot& operator=(const Snapshot&) = default;
      Snapshot& operator=(Snapshot&&) = default;

      friend bool operator==(const Snapshot& lhs, const Snapshot& rhs);
      friend bool operator!=(const Snapshot& lhs, const Snapshot& rhs);
   private:
      void ApplySnapshot(Context& ctx) const;

      FramebufferPtr mCurrentFramebuffer;

      ProgramPtr mCurrentProgram;
      ProgramConstantsPtr mProgramConstants;
      size_t mProgramConstantsVersion { 0 };

      VertexArrayPtr mCurrentVertexArray;

      std::array<TexturePtr, 2> mCurrentTexture;

      RectType<GLint> mClipRect;
      bool mClippingEnabled { false };

      friend class Context;
   };

   const Snapshot& GetSnapshot() const;
   void SetSnaphot(const Snapshot& snapshot);

   float GetScaleFactor() const noexcept;
   uint32_t GetDPI() const noexcept;

   void CheckErrors() const;

   virtual void BindDefaultFramebuffer();
   virtual bool HasFlippedY() const noexcept;
protected:
   virtual void SetupContext();
   void DoProcessReleaseQueue();
   virtual void ProcessReleaseQueue() = 0;
   void UpdateScreenProperties(uint32_t dpi, float scaleFactor) noexcept;
private:
   void SetScreenSpaceClipRect(RectType<GLint> rect);

   const GLFunctions& mFunctions;

   Color mClearColor;

   std::vector<std::pair<GLenum, const VertexBuffer*>> mBufferMappings;
   std::vector<std::pair<ContextResourceType, GLuint>> mReleaseQueue;

   GLenum mCurrentActiveTexture { GLenum::TEXTURE0 };

   Snapshot mCurrentState;

   GLuint mSamplerStateObject { 0 };
   GLuint mPrimitiveRestartIndex { std::numeric_limits<GLuint>::max() };

   RectType<uint32_t> mViewport;

   uint32_t mUnpackAlignment { 4 };

   uint32_t mDPI { 96 };
   float mScaleFactor { 1.0f };
}; // class Context;
} // namespace graphics::gl
