/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  CGLRenderer.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <memory>

#include "graphics/gl/Context.h"
#include "graphics/gl/GLRenderer.h"


namespace graphics::gl::platforms::macocs
{
class CGLFunctions;
class CGLContext;

class CGLRenderer final : public GLRenderer
{
public:
   CGLRenderer();
   ~CGLRenderer();

   bool IsAvailable() const override;

   Context& GetResourceContext() override;

   std::unique_ptr<Context> CreateContext(void* window) override;
   void ContextDestroyed(Context& ctx);

   void BeginRendering(Context& context) override;
   void EndRendering() override;

private:
   std::unique_ptr<CGLFunctions> mCGLFunctions;
   std::unique_ptr<CGLContext> mCGLContext;

   CGLContext* mCurrentContext { nullptr };
}; // class EGLRenderer

} // namespace graphics::gl::platforms::maocs
