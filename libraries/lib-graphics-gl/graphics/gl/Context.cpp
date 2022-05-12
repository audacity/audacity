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

namespace graphics::gl
{

Context::Context(GLFunctions& functions)
    : mFunctions(functions)
{
}

Context::~Context()
{
}

void Context::Clear(Color color)
{
   if (mClearColor != color)
   {
      mClearColor = color;
      mFunctions.ClearColor(
         color.GetRed(), color.GetGreen(), color.GetBlue(), color.GetAlpha());
   }

   mFunctions.Clear(static_cast<GLbitfield>(GLenum::COLOR_BUFFER_BIT));
}

TexturePtr Context::GetCurrentTexture(uint32_t textureUnitIndex = 0) const
{
   if (textureUnitIndex < MAX_TEXTURE_UNITS)
      return mCurrentTexture[textureUnitIndex];

   return {};
}

bool Context::BindTexture(const TexturePtr& texture, uint32_t textureUnitIndex)
{
   if (textureUnitIndex >= MAX_TEXTURE_UNITS)
      return false;

   const GLenum targetTextureUnit = static_cast<GLenum>(
      static_cast<uint32_t>(GLenum::TEXTURE0) + textureUnitIndex);

   if (mCurrentClientTexture != targetTextureUnit)
   {
      mCurrentClientTexture = targetTextureUnit;
      mFunctions.ActiveTexture(mCurrentClientTexture);
   }

   if (mCurrentTexture[textureUnitIndex] == texture)
      return false;

   mCurrentTexture[textureUnitIndex] = texture;

   if (texture != nullptr)
      texture->Bind(*this);
   else
      mFunctions.BindTexture(targetTextureUnit, 0);

   return true;
}

const GLFunctions& Context::GetFunctions() const
{
   return mFunctions;
}

void Context::ResetVertexArrayState()
{
   mCurrentVertexArray = {};
}

bool Context::BindVertexArray(const VertexArrayPtr& vertexArray)
{
   if (mCurrentVertexArray == vertexArray)
      return false;

   mCurrentVertexArray = vertexArray;
   mCurrentVertexArray->Bind(*this);

   return true;
}

void Context::ReleaseContextResource(
   ContextResourceType framebuffer, GLuint resourceId)
{
   const auto pair = std::make_pair(framebuffer, resourceId);

   if (
      std::find(mReleaseQueue.begin(), mReleaseQueue.end(), pair) !=
      mReleaseQueue.end())
   {
      mReleaseQueue.push_back(pair);
   }

   ProcessReleaseQueue();
}

void Context::SetupContext()
{
   mFunctions.Enable(GLenum::TEXTURE_2D);
   mFunctions.Enable(GLenum::SCISSOR_TEST);
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

} // namespace graphics::gl
