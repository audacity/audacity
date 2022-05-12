/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  GLRenderer.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>

#include "graphics/RendererID.h"

namespace graphics::gl
{
extern const RendererID OpenGLRendererID;

struct GLFunctions;
class Context;

class GRAPHICS_GL_API GLRenderer /* not final */
{
public:
   virtual ~GLRenderer();

   virtual bool IsSupported() const = 0;

   virtual Context& GetResourceContext() = 0;
   virtual std::unique_ptr<Context> CreateContext(void* window) = 0;

   virtual void BeginRendering(Context& context) = 0;
   virtual void EndRendering() = 0;
};

GRAPHICS_GL_API GLRenderer& GetSharedRenderer();
GRAPHICS_GL_API void ShutdownSharedRenderer();
}
