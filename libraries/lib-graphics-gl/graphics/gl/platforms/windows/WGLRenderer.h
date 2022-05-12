/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WGLRenderer.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>

#include "graphics/gl/GLRenderer.h"

namespace graphics::gl::platforms::windows
{
class WGLRenderer final : public GLRenderer
{
public:
   WGLRenderer();
   ~WGLRenderer();

   bool IsSupported() const override;

   Context& GetResourceContext() override;

   std::unique_ptr<Context> CreateContext(void* window) override;

   virtual void BeginRendering(Context& context) override;
   virtual void EndRendering() override;
private:
   class WGLContext;
   class InvisibleWindow;

   std::unique_ptr<InvisibleWindow> mInvisibleWindow;
   std::unique_ptr<WGLContext> mInvisibleWindowContext;

   WGLContext* mCurrentContext { nullptr };
};
}
