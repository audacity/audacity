/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  GLRenderer.cpp

  Dmitry Vedenko

**********************************************************************/
#include "GLRenderer.h"

#include "Context.h"
#include "GLPainter.h"
#include "ProgramLibrary.h"
#include "GLFontRenderer.h"

#include "graphics/fonts/FontLibrary.h"
#include "graphics/fonts/Font.h"

namespace graphics::gl
{
namespace
{
std::unique_ptr<GLRenderer> SharedRenderer;
std::vector<std::function<std::unique_ptr<GLRenderer>()>> RendererFactories;
}

bool RegisterRendererFactory(std::function<std::unique_ptr<GLRenderer>()> factory)
{
   RendererFactories.emplace_back(std::move(factory));

   return true;
}

const RendererID OpenGLRendererID = RegisterRenderer("OpenGL 3.2");

GLRenderer& GetSharedRenderer()
{
   if (SharedRenderer == nullptr)
   {
      for(const auto& factory : RendererFactories)
      {
         SharedRenderer = factory();

         if (SharedRenderer->IsAvailable())
            return *SharedRenderer;
      }
   }

   return *SharedRenderer;
}

void ShutdownSharedRenderer()
{
   if (SharedRenderer)
   {
      SharedRenderer->Shutdown();
      SharedRenderer.reset();
   }
}

GLRenderer::GLRenderer()
    : mProgramLibrary(std::make_shared<ProgramLibrary>(*this))
    , mFontRenderer(std::make_unique<GLFontRenderer>(*this))
{
}

GLRenderer::~GLRenderer()
{
}

std::unique_ptr<Painter>
GLRenderer::CreateWindowPainter(void* window, const FontInfo& defaultFont)
{
   auto context = CreateContext(window);

   if (context == nullptr)
      return {};

   return std::make_unique<GLPainter>(*this, std::move(context), defaultFont);
}

std::unique_ptr<Painter>
GLRenderer::CreateMeasuringPainter(const FontInfo& defaultFont)
{
   return std::make_unique<GLPainter>(
      *this, GetResourceContext(), defaultFont);
}

const ProgramLibrary& GLRenderer::GetProgramLibrary() const
{
   return *mProgramLibrary;
}

GLFontRenderer& GLRenderer::GetFontRenderer() const
{
   return *mFontRenderer;
}

void GLRenderer::Shutdown()
{
   mProgramLibrary = {};

   Publish(RendererDestroyedMessage {});
}

} // namespace graphics::gl
