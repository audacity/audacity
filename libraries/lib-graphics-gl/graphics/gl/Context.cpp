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

namespace graphics::gl
{

Context::Context(GLFunctions& functions)
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
         color.GetRed(), color.GetGreen(), color.GetBlue(), color.GetAlpha());
   }

   const bool clippingWasEnabled = mCurrentState.mClippingEnabled;
   auto oldRect = mCurrentState.mClipRect;

   SetClipRect(rect);
   
   mFunctions.Clear(static_cast<GLbitfield>(GLenum::COLOR_BUFFER_BIT));

   if (!clippingWasEnabled)
   {
      ResetClipRect();
   }
   else
   {
      SetClipRect(oldRect);
   }
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

   const GLenum targetTextureUnit = static_cast<GLenum>(
      static_cast<uint32_t>(GLenum::TEXTURE0) + textureUnitIndex);

   if (mCurrentState.mCurrentTexture[textureUnitIndex] == texture)
      return ;

   SetClientActiveTexture(textureUnitIndex);
   
   if (texture != nullptr)
      texture->Bind(*this, mCurrentState.mCurrentTexture[textureUnitIndex].get());
   else
      mFunctions.BindTexture(GLenum::TEXTURE_2D, 0);

   mCurrentState.mCurrentTexture[textureUnitIndex] = texture;
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
}

void Context::BindProgram(const ProgramPtr& program)
{
   if (mCurrentState.mCurrentProgram != program)
   {
      mCurrentState.mCurrentProgram = program;

      if (program != nullptr)
         program->Bind(*this);
      else
         mFunctions.UseProgram(0);
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
         mFunctions.BindFramebuffer(GLenum::FRAMEBUFFER, 0);
   }
}

void Context::SetClipRect(const Rect& rect)
{
   SetClipRect(rect_cast<GLint>(rect));
}

void Context::SetClipRect(const RectType<GLint>& rect)
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
         rect.Origin.x, mViewport.Size.height - rect.Size.height - rect.Origin.y,
         rect.Size.width, rect.Size.height);
   }
}

void Context::ResetClipRect()
{
   if (mCurrentState.mClippingEnabled)
   {
      mCurrentState.mClippingEnabled = false;
      mFunctions.Disable(GLenum::SCISSOR_TEST);
      
      mCurrentState.mClipRect = { {},
                                  { static_cast<GLint>(mViewport.Size.width),
                                    static_cast<GLint>(mViewport.Size.height) } };
   }
}

void Context::SetClientActiveTexture(uint32_t textureUnitIndex)
{
   const GLenum targetTextureUnit = static_cast<GLenum>(
      static_cast<uint32_t>(GLenum::TEXTURE0) + textureUnitIndex);

   if (mCurrentActiveTexture != targetTextureUnit)
   {
      mFunctions.ActiveTexture(targetTextureUnit);
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
   if (mViewport != viewport)
   {
      mViewport = viewport;
      mFunctions.Viewport(
         viewport.Origin.x, viewport.Origin.y, viewport.Size.width,
         viewport.Size.height);

      mCurrentState.mClipRect = { {},
                                  { static_cast<GLint>(viewport.Size.width),
                                    static_cast<GLint>(viewport.Size.height) } };
   }
}

void Context::SetUnpackAlignment(uint32_t alignment)
{
   if (mUnpackAlignment != alignment)
   {
      mFunctions.PixelStorei(GLenum::UNPACK_ALIGNMENT, alignment);
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
   
   mFunctions.Enablei(GLenum::BLEND, 0);
   mFunctions.BlendFunc(GLenum::SRC_ALPHA, GLenum::ONE_MINUS_SRC_ALPHA);
   mFunctions.BlendEquation(GLenum::FUNC_ADD);

   mFunctions.GenSamplers(1, &mSamplerStateObject);

   mFunctions.SamplerParameteri(mSamplerStateObject, GLenum::TEXTURE_WRAP_S, static_cast<GLint>(GLenum::CLAMP_TO_EDGE));
   mFunctions.SamplerParameteri(mSamplerStateObject, GLenum::TEXTURE_WRAP_T, static_cast<GLint>(GLenum::CLAMP_TO_EDGE));
   mFunctions.SamplerParameteri(mSamplerStateObject, GLenum::TEXTURE_MIN_FILTER, static_cast<GLint>(GLenum::LINEAR));
   mFunctions.SamplerParameteri(mSamplerStateObject, GLenum::TEXTURE_MAG_FILTER, static_cast<GLint>(GLenum::LINEAR));
   
   mFunctions.BindSampler(0, mSamplerStateObject);
   mFunctions.BindSampler(1, mSamplerStateObject);
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
   context.BindFramebuffer(mCurrentFramebuffer);
   context.BindProgram(mCurrentProgram);
   context.BindVertexArray(mCurrentVertexArray);

   for (size_t i = 0; i < mCurrentTexture.size(); ++i)
      context.BindTexture(mCurrentTexture[i], i);

   if (!mClippingEnabled)
      context.ResetClipRect();
   else
      context.SetClipRect(mClipRect);
}

bool operator==(const Context::Snapshot& lhs, const Context::Snapshot& rhs)
{
   return lhs.mCurrentFramebuffer == rhs.mCurrentFramebuffer &&
      lhs.mCurrentProgram == rhs.mCurrentProgram &&
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
