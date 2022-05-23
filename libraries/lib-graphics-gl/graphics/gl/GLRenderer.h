/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  GLRenderer.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>

#include "graphics/RendererID.h"
#include "Observer.h"

class Painter;
class FontInfo;

namespace graphics::gl
{
extern const RendererID OpenGLRendererID;

struct GLFunctions;
class Context;
class ProgramLibrary;
class GLFontRenderer;

struct RendererDestroyedMessage : Observer::Message {}; 

class GRAPHICS_GL_API GLRenderer /* not final */ :
    public Observer::Publisher<RendererDestroyedMessage>
{
public:
   GLRenderer();
   virtual ~GLRenderer();

   virtual bool IsAvailable() const = 0;

   virtual Context& GetResourceContext() = 0;
   virtual std::unique_ptr<Context> CreateContext(void* window) = 0;

   std::unique_ptr<Painter> CreateWindowPainter(void* window, const FontInfo& defaultFont);
   std::unique_ptr<Painter> CreateMeasuringPainter(const FontInfo& defaultFont);

   const ProgramLibrary& GetProgramLibrary() const;
   GLFontRenderer& GetFontRenderer() const;
   
   virtual void BeginRendering(Context& context) = 0;
   virtual void EndRendering() = 0;

   void Shutdown();

private:
   std::shared_ptr<ProgramLibrary> mProgramLibrary;
   std::unique_ptr<GLFontRenderer> mFontRenderer;
};

GRAPHICS_GL_API GLRenderer& GetSharedRenderer();
GRAPHICS_GL_API void ShutdownSharedRenderer();
}
