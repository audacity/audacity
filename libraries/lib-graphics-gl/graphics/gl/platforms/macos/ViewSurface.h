/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  ViewSurface.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <cstdint>
#include <memory>

namespace graphics::gl
{
struct GLFunctions;
}

namespace graphics::gl::platforms::macocs
{
class ViewSurface final
{
public:
   ViewSurface(const GLFunctions& functions, void* view);
   ~ViewSurface();

   uint32_t GetWidth() const noexcept;
   uint32_t GetHeight() const noexcept;
   float GetScaleFactor() const noexcept;

   void BeginRendering();
   void BindFramebuffer();
   void EndRendering();

private:
   class Impl;
   std::unique_ptr<Impl> mImpl;
};

} // namespace graphics::gl::platforms::macocs
