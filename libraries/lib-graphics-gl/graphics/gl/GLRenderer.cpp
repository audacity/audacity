/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  GLRenderer.cpp

  Dmitry Vedenko

**********************************************************************/
#include "GLRenderer.h"
#include "GLFunctions.h"
#include "Context.h"

#ifdef WIN32
#  include "platforms/windows/WGLRenderer.h"
#endif

namespace graphics::gl
{
namespace
{
std::unique_ptr<GLRenderer> SharedRenderer;
}

const RendererID OpenGLRendererID = RegisterRenderer("OpenGL 3.0");

GLRenderer& GetSharedRenderer()
{
   if (SharedRenderer == nullptr)
   {
   #ifdef WIN32
      SharedRenderer = std::make_unique<platforms::windows::WGLRenderer>();
   #endif
   }

   return *SharedRenderer;
}

void ShutdownSharedRenderer()
{
   SharedRenderer.reset();
}

GLRenderer::~GLRenderer()
{
}

} // namespace graphics::gl
