/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  Context.cpp

  Dmitry Vedenko

**********************************************************************/
#include "Context.h"

#include <algorithm>

#include "Texture.h"
#include "VertexArray.h"
#include "VertexBuffer.h"
#include "Program.h"
#include "Framebuffer.h"

#define CHECK_ERRORS 0

#ifdef CHECK_ERRORS
#include "wx/log.h"
#endif

namespace graphics::gl
{

Context::Context(const GLFunctions& functions)
    : mFunctions(functions)
{

}

Context::~Context()
{
}

void Context::Clear(const Rect& rect, Color color)
{
   if (mClearColor != color)
   {
      mClearColor = color;
      mFunctions.ClearColor(
         color.GetRed() / 255.0f, color.GetGreen() / 255.0f,
         color.GetBlue() / 255.0f, color.GetAlpha() / 255.0f);
   }

   const bool clippingWasEnabled = mCurrentState.mClippingEnabled;
   auto oldRect = mCurrentState.mClipRect;

   SetClipRect(rect);

   mFunctions.Clear(static_cast<GLbitfield>(GLenum::COLOR_BUFFER_BIT));

   if (!clippingWasEnabled)
      ResetClipRect();
   else
      SetScreenSpaceClipRect(oldRect);
}

void Context::Clear()
{
   mFunctions.Clear(static_cast<GLbitfield>(GLenum::COLOR_BUFFER_BIT));
}

TexturePtr Context::GetCurrentTexture(uint32_t textureUnitIndex = 0) const
{
   if (textureUnitIndex < MAX_TEXTURE_UNITS)
      return mCurrentState.mCurrentTexture[textureUnitIndex];

   return {};
}

void Context::BindTexture(const TexturePtr& texture, uint32_t textureUnitIndex)
{
   if (textureUnitIndex >= MAX_TEXTURE_UNITS)
      return ;

   if (mCurrentState.mCurrentTexture[textureUnitIndex] == texture)
      return ;

   SetClientActiveTexture(textureUnitIndex);

   if (texture != nullptr)
      texture->Bind(*this, mCurrentState.mCurrentTexture[textureUnitIndex].get());
   else
      mFunctions.BindTexture(GLenum::TEXTURE_2D, 0);

   mCurrentState.mCurrentTexture[textureUnitIndex] = texture;

   CheckErrors();
}

const GLFunctions& Context::GetFunctions() const
{
   return mFunctions;
}

void Context::ResetVertexArrayState()
{
   mCurrentState.mCurrentVertexArray = {};
   mFunctions.BindVertexArray(0);
}

void Context::BindVertexArray(const VertexArrayPtr& vertexArray)
{
   if (mCurrentState.mCurrentVertexArray == vertexArray)
      return ;

   mCurrentState.mCurrentVertexArray = vertexArray;
   mCurrentState.mCurrentVertexArray->Bind(*this);

   CheckErrors();
}

void Context::ReleaseContextResource(
   ContextResourceType framebuffer, GLuint resourceId)
{
   const auto pair = std::make_pair(framebuffer, resourceId);

   if (
      std::find(mReleaseQueue.begin(), mReleaseQueue.end(), pair) ==
      mReleaseQueue.end())
   {
      mReleaseQueue.push_back(pair);
   }

   ProcessReleaseQueue();
}

void Context::BindBuffer(const VertexBuffer& buffer)
{
   auto it = std::find_if(
      mBufferMappings.begin(), mBufferMappings.end(),
      [target = buffer.GetBufferTarget()](const auto& pair)
      { return pair.first == target; });

   if (it != mBufferMappings.end())
   {
      if (it->second != &buffer)
      {
         it->second = &buffer;
         buffer.Bind(*this);
      }
   }
   else
   {
      mBufferMappings.emplace_back(
         std::make_pair(buffer.GetBufferTarget(), &buffer));

      buffer.Bind(*this);
   }

   CheckErrors();
}

void Context::BindProgram(const ProgramPtr& program, const ProgramConstantsPtr& constants)
{
   const size_t constantsVersion = constants != nullptr ? constants->GetVersion() : 0;

   if (
      mCurrentState.mCurrentProgram != program ||
      mCurrentState.mProgramConstants != constants ||
      mCurrentState.mProgramConstantsVersion != constantsVersion)
   {
      mCurrentState.mCurrentProgram = program;
      mCurrentState.mProgramConstants = constants;
      mCurrentState.mProgramConstantsVersion = constantsVersion;

      if (program != nullptr)
         program->Bind(*this, constants.get());
      else
         mFunctions.UseProgram(0);

      CheckErrors();
   }
}

void Context::BindFramebuffer(const FramebufferPtr& framebuffer)
{
   if (mCurrentState.mCurrentFramebuffer != framebuffer)
   {
      mCurrentState.mCurrentFramebuffer = framebuffer;

      if (framebuffer != nullptr)
         framebuffer->Bind(*this);
      else
         BindDefaultFramebuffer();

      CheckErrors();
   }
}

FramebufferPtr Context::GetCurrentFramebuffer() const
{
   return mCurrentState.mCurrentFramebuffer;
}

void Context::SetClipRect(const Rect& rect)
{
   SetClipRect(rect_cast<GLint>(rect));
}

void Context::SetClipRect(const RectType<GLint>& rect)
{
   const auto scaleFactor = GetScaleFactor();

   RectType<GLint> scaledRect { {
      static_cast<GLint>(scaleFactor * rect.origin.x),
      static_cast<GLint>(scaleFactor * rect.origin.y) }, {
      static_cast<GLint>(scaleFactor * rect.size.width),
      static_cast<GLint>(scaleFactor * rect.size.height)
   } };

   const auto fixOrigin =
      mCurrentState.mCurrentFramebuffer != nullptr || !HasFlippedY();

   if (fixOrigin)
   {
      scaledRect.origin.y =
         mViewport.size.height - scaledRect.origin.y - scaledRect.size.height;
   }

   SetScreenSpaceClipRect(scaledRect);
}

void Context::SetScreenSpaceClipRect(RectType<GLint> rect)
{
   
   if (!mCurrentState.mClippingEnabled)
   {
      mCurrentState.mClippingEnabled = true;
      mFunctions.Enable(GLenum::SCISSOR_TEST);
   }

   if (rect != mCurrentState.mClipRect)
   {
      mCurrentState.mClipRect = rect;

      mFunctions.Scissor(
         rect.origin.x,
         rect.origin.y,
         rect.size.width,
         rect.size.height);
   }
}

void Context::ResetClipRect()
{
   if (mCurrentState.mClippingEnabled)
   {
      mCurrentState.mClippingEnabled = false;
      mFunctions.Disable(GLenum::SCISSOR_TEST);
   }
}

void Context::SetClientActiveTexture(uint32_t textureUnitIndex)
{
   const GLenum targetTextureUnit = static_cast<GLenum>(
      static_cast<uint32_t>(GLenum::TEXTURE0) + textureUnitIndex);

   if (mCurrentActiveTexture != targetTextureUnit)
   {
      mFunctions.ActiveTexture(targetTextureUnit);
      CheckErrors();
      mCurrentActiveTexture = targetTextureUnit;
   }
}

void Context::SetPrimitiveRestartIndex(GLuint index)
{
   if (mPrimitiveRestartIndex != index)
   {
      mFunctions.PrimitiveRestartIndex(index);
      mPrimitiveRestartIndex = index;
   }
}

void Context::SetViewport(const RectType<uint32_t> viewport)
{
   const auto scaleFactor = GetScaleFactor();

   const RectType<GLuint> scaledRect { {
      static_cast<GLuint>(scaleFactor * viewport.origin.x),
      static_cast<GLuint>(scaleFactor * viewport.origin.y) }, {
      static_cast<GLuint>(scaleFactor * viewport.size.width),
      static_cast<GLuint>(scaleFactor * viewport.size.height)
   } };

   if (mViewport != scaledRect)
   {
      mViewport = scaledRect;

      mFunctions.Viewport(
         scaledRect.origin.x,
         scaledRect.origin.y,
         scaledRect.size.width,
         scaledRect.size.height);
   }
}

void Context::SetUnpackAlignment(uint32_t alignment)
{
   if (mUnpackAlignment != alignment)
   {
      mFunctions.PixelStorei(GLenum::UNPACK_ALIGNMENT, alignment);
      CheckErrors();
      mUnpackAlignment = alignment;
   }
}

void Context::SetBestUnpackAlignment(uint32_t rowStride)
{
   if (rowStride % 8 == 0)
      SetUnpackAlignment(8);
   else if (rowStride % 4 == 0)
      SetUnpackAlignment(4);
   else if (rowStride % 2 == 0)
      SetUnpackAlignment(2);
   else
      SetUnpackAlignment(1);
}

void Context::SetupContext()
{
   mFunctions.Enable(GLenum::PRIMITIVE_RESTART);

   mFunctions.Enable(GLenum::BLEND);

   mFunctions.BlendFunc(GLenum::SRC_ALPHA, GLenum::ONE_MINUS_SRC_ALPHA);
   mFunctions.BlendEquation(GLenum::FUNC_ADD);

   mFunctions.GenSamplers(1, &mSamplerStateObject);

   mFunctions.SamplerParameteri(mSamplerStateObject, GLenum::TEXTURE_WRAP_S, static_cast<GLint>(GLenum::CLAMP_TO_EDGE));
   mFunctions.SamplerParameteri(mSamplerStateObject, GLenum::TEXTURE_WRAP_T, static_cast<GLint>(GLenum::CLAMP_TO_EDGE));
   mFunctions.SamplerParameteri(mSamplerStateObject, GLenum::TEXTURE_MIN_FILTER, static_cast<GLint>(GLenum::LINEAR));
   mFunctions.SamplerParameteri(mSamplerStateObject, GLenum::TEXTURE_MAG_FILTER, static_cast<GLint>(GLenum::LINEAR));

   mFunctions.BindSampler(0, mSamplerStateObject);
   mFunctions.BindSampler(1, mSamplerStateObject);

   mFunctions.ActiveTexture(GLenum::TEXTURE0);

   CheckErrors();
}

void Context::DoProcessReleaseQueue()
{
   for (auto pair : mReleaseQueue)
   {
      switch (pair.first)
      {
      case ContextResourceType::Framebuffer:
         mFunctions.DeleteFramebuffers(1, &pair.second);
         break;
      default:
         assert(false);
      }
   }

   mReleaseQueue.clear();
}

float Context::GetScaleFactor() const noexcept
{
   return mCurrentState.mCurrentFramebuffer != nullptr ?
      mCurrentState.mCurrentFramebuffer->GetScaleFactor() :
      mScaleFactor;
}

uint32_t Context::GetDPI() const noexcept
{
   return mCurrentState.mCurrentFramebuffer != nullptr ?
             mCurrentState.mCurrentFramebuffer->GetDPI() :
             mDPI;
}

void Context::UpdateScreenProperties(uint32_t dpi, float scaleFactor) noexcept
{
   if (mDPI != dpi)
   {
      mDPI = dpi;
   }

   if (std::abs(mScaleFactor - scaleFactor) > std::numeric_limits<float>::epsilon())
   {
      mScaleFactor = scaleFactor;
   }
}

void Context::CheckErrors() const
{
#if CHECK_ERRORS
   const auto error = mFunctions.GetError();
   if (error != GLenum::NO_ERROR)
   {
      wxLogDebug("GL error: %d", error);
   }
#endif
}

bool Context::HasFlippedY() const noexcept
{
   return false;
}

void Context::BindDefaultFramebuffer()
{
   mFunctions.BindFramebuffer(GLenum::FRAMEBUFFER, 0);
}

const Context::Snapshot& Context::GetSnapshot() const
{
   return mCurrentState;
}

void Context::SetSnaphot(const Snapshot& snapshot)
{
   snapshot.ApplySnapshot(*this);
}

void Context::Snapshot::ApplySnapshot(Context& context) const
{
   context.BindProgram(mCurrentProgram, mProgramConstants);
   context.BindFramebuffer(mCurrentFramebuffer);
   context.BindVertexArray(mCurrentVertexArray);

   for (size_t i = 0; i < mCurrentTexture.size(); ++i)
      context.BindTexture(mCurrentTexture[i], i);

   if (!mClippingEnabled)
      context.ResetClipRect();
   else
      context.SetScreenSpaceClipRect(mClipRect);
}

bool operator==(const Context::Snapshot& lhs, const Context::Snapshot& rhs)
{
   return lhs.mCurrentFramebuffer == rhs.mCurrentFramebuffer &&
      lhs.mCurrentProgram == rhs.mCurrentProgram &&
      lhs.mProgramConstants == rhs.mProgramConstants &&
      lhs.mProgramConstantsVersion == rhs.mProgramConstantsVersion &&
      lhs.mCurrentVertexArray == rhs.mCurrentVertexArray &&
      lhs.mCurrentTexture == rhs.mCurrentTexture &&
      lhs.mClippingEnabled == rhs.mClippingEnabled &&
      lhs.mClipRect == rhs.mClipRect;
}

bool operator!=(const Context::Snapshot& lhs, const Context::Snapshot& rhs)
{
   return !(lhs == rhs);
}


} // namespace graphics::gl
