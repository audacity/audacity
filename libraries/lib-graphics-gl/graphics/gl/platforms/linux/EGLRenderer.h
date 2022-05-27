/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  EGLRenderer.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "EGLRenderer.h"

#include <cstdint>
#include <memory>

#include "graphics/gl/Context.h"
#include "graphics/gl/GLRenderer.h"

namespace graphics::gl::platforms::linux_like
{
class EGLFunctions;
class EGLContextWrapper;

class EGLRenderer final : public GLRenderer
{
public:
   EGLRenderer();
   ~EGLRenderer();

   bool IsAvailable() const override;

   Context& GetResourceContext() override;

   std::unique_ptr<Context> CreateContext(void* window) override;
   void ContextDestroyed(Context& ctx);

   void BeginRendering(Context& context) override;
   void EndRendering() override;

private:
   std::unique_ptr<EGLFunctions> mEGLFunctions;
   std::unique_ptr<EGLContextWrapper> mEGLContext;

   EGLContextWrapper* mCurrentContext { nullptr };
}; // class EGLRenderer


} // namespace graphics::gl::platforms::linux_like
