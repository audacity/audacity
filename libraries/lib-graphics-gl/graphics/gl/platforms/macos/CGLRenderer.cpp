/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  CGLRenderer.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CGLRenderer.h"

#include <OpenGL/OpenGL.h>
#include <OpenGL/CGLTypes.h>
#include <OpenGL/CGLCurrent.h>

#include <dlfcn.h>

#include "ViewSurface.h"

namespace graphics::gl::platforms::macocs
{
namespace
{
auto registered = ([](){
                      return RegisterRendererFactory([](){
                                                        return std::make_unique<CGLRenderer>();
                                                     });
                   })();
}

class CGLFunctions : public GLFunctions
{
public:
   CGLFunctions()
   {
      if (&CGLCreateContext == nullptr)
         return;

      mProcessHandle = dlopen(NULL, RTLD_LAZY | RTLD_LOCAL);

      if (mProcessHandle == nullptr)
         return;

      mIsOk = LoadFunctions();
   }

   ~CGLFunctions()
   {
      if (mProcessHandle != nullptr)
         dlclose(mProcessHandle);
   }

   bool IsOk() const noexcept
   {
      return mIsOk;
   }
private:
   void* GetFunctionPointer(const char* name) const
   {
      return dlsym(mProcessHandle, name);
   }

   void* mProcessHandle { nullptr };

   bool mIsOk { false };
};

const CGLPixelFormatAttribute attributes[4] = {
  kCGLPFAAccelerated,
  kCGLPFAOpenGLProfile,
  static_cast<CGLPixelFormatAttribute>(kCGLOGLPVersion_3_2_Core),
  static_cast<CGLPixelFormatAttribute>(0)
};

class CGLContext : public Context
{
public:
   CGLContext(CGLRenderer& renderer, const CGLFunctions& functions)
      : Context(functions)
      , mRenderer(renderer)
      , mFunctions(functions)
   {
      GLint numPixelFormats;

      CGLError error = CGLChoosePixelFormat(attributes, &mPixelFormat, &numPixelFormats);

      if (error != kCGLNoError)
         return;

      error = CGLCreateContext(mPixelFormat, nullptr, &mContext);

      if (error != kCGLNoError)
         mContext = nullptr;

      CGLSetCurrentContext(mContext);
      SetupContext();
      mInitialised = true;
   }

   CGLContext(CGLContext& baseContext, void* view)
      : Context(baseContext.mFunctions)
      , mRenderer(baseContext.mRenderer)
      , mFunctions(baseContext.mFunctions)
      , mSurface(std::make_unique<ViewSurface>(mFunctions, view))
   {
      CGLError error = CGLCreateContext(baseContext.mPixelFormat, baseContext.mContext, &mContext);

      if (error != kCGLNoError)
         mContext = nullptr;
   }

   ~CGLContext()
   {
      Publish(ContextDestroyedMessage {});
      mRenderer.ContextDestroyed(*this);

      if (mContext != nullptr)
         CGLDestroyContext(mContext);

      if (mPixelFormat != nullptr)
         CGLDestroyPixelFormat(mPixelFormat);
   }

   Size GetSize() const override
   {
      if (mSurface != nullptr)
         return { float(mSurface->GetWidth()), float(mSurface->GetHeight()) };

      return {};
   }

   void ProcessReleaseQueue() override
   {
      if (mContext == CGLGetCurrentContext())
         DoProcessReleaseQueue();
   }

   bool IsOk() const noexcept
   {
      return mContext != nullptr;
   }

   void BeginRendering()
   {
      if(mContext == nullptr)
         return;

      if (mContext != CGLGetCurrentContext())
         CGLSetCurrentContext(mContext);

      if (!mInitialised)
      {
         mInitialised = true;
         SetupContext();
      }

      ProcessReleaseQueue();

      if (mSurface != nullptr)
      {
         mSurface->BeginRendering();

         UpdateScreenProperties(
            static_cast<uint32_t>(96 * mSurface->GetScaleFactor()),
            mSurface->GetScaleFactor());
      }
   }

   void EndRendering()
   {
      if (mSurface != nullptr)
         mSurface->EndRendering();
   }

   void BindDefaultFramebuffer() override
   {
      if (mSurface != nullptr)
         mSurface->BindFramebuffer();
   }

   bool HasFlippedY() const noexcept override
   {
      return true;
   }
private:
   CGLRenderer& mRenderer;
   const CGLFunctions& mFunctions;

   CGLPixelFormatObj mPixelFormat { nullptr };
   CGLContextObj mContext { nullptr };

   std::unique_ptr<ViewSurface> mSurface;

   bool mInitialised { false };
};

CGLRenderer::CGLRenderer()
   : mCGLFunctions(std::make_unique<CGLFunctions>())
{
   if (!mCGLFunctions->IsOk())
      return;

   mCGLContext = std::make_unique<CGLContext>(*this, *mCGLFunctions);
}

CGLRenderer::~CGLRenderer()
{

}

bool CGLRenderer::IsAvailable() const
{
   return mCGLContext != nullptr && mCGLContext->IsOk();
}

Context& CGLRenderer::GetResourceContext()
{
   return *mCGLContext;
}

std::unique_ptr<Context> CGLRenderer::CreateContext(void* window)
{
   return std::make_unique<CGLContext>(*mCGLContext, window);
}

void CGLRenderer::ContextDestroyed(Context& ctx)
{
   if (mCurrentContext == &ctx)
   {
      if (mCurrentContext != mCGLContext.get())
      {
         mCurrentContext = mCGLContext.get();

         if (mCGLContext != nullptr)
            mCGLContext->BeginRendering();
      }
   }
}

void CGLRenderer::BeginRendering(Context& context)
{
   mCurrentContext = static_cast<CGLContext*>(&context);
   mCurrentContext->BeginRendering();
}

void CGLRenderer::EndRendering()
{
   if (mCurrentContext != nullptr)
      mCurrentContext->EndRendering();
}

} // namespace graphics::gl::platforms::maocs
