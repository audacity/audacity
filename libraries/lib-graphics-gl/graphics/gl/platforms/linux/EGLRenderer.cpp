/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  EGLRenderer.cpp

  Dmitry Vedenko

**********************************************************************/
#include "EGLRenderer.h"

#include <dlfcn.h>
#include <cassert>

#include <gdk/gdk.h>
#include <gtk/gtk.h>
#ifdef GDK_WINDOWING_X11
#   include <gdk/gdkx.h>
#   ifndef GDK_IS_X11_DISPLAY
#      define GDK_IS_X11_DISPLAY(display) (true)
#   endif
#   ifndef GDK_IS_X11_WINDOW
#      define GDK_IS_X11_WINDOW(window) (true)
#   endif
#endif
#ifdef GDK_WINDOWING_WAYLAND
#   include <gdk/gdkwayland.h>
#endif

namespace graphics::gl::platforms::linux_like
{
namespace
{
namespace
{
auto registered = ([](){
                      return RegisterRendererFactory([](){
                                                        return std::make_unique<EGLRenderer>();
                                                     });
                   })();
}
}

#define EGLAPIENTRYP

using EGLBoolean = int;
using EGLint = int32_t;
using EGLenum = uint32_t;
using EGLDisplay = void*;
using EGLConfig = void*;
using EGLSurface = void*;
using EGLContext = void*;
using EGLNativeDisplayType = void*;
using EGLNativePixmapType = void*;
using EGLNativeWindowType = void*;
using EGLClientBuffer = void*;

enum class EGLError : EGLint
{
   EGL_SUCCESS = 0x3000,
   EGL_NOT_INITIALIZED = 0x3001,
   EGL_BAD_ACCESS = 0x3002,
   EGL_BAD_ALLOC = 0x3003,
   EGL_BAD_ATTRIBUTE = 0x3004,
   EGL_BAD_CONFIG = 0x3005,
   EGL_BAD_CONTEXT = 0x3006,
   EGL_BAD_CURRENT_SURFACE = 0x3007,
   EGL_BAD_DISPLAY = 0x3008,
   EGL_BAD_MATCH = 0x3009,
   EGL_BAD_NATIVE_PIXMAP = 0x300A,
   EGL_BAD_NATIVE_WINDOW = 0x300B,
   EGL_BAD_PARAMETER = 0x300C,
   EGL_BAD_SURFACE = 0x300D,
   EGL_CONTEXT_LOST = 0x300E,
};

constexpr EGLint EGL_BUFFER_SIZE = 0x3020;
constexpr EGLint EGL_ALPHA_SIZE = 0x3021;
constexpr EGLint EGL_BLUE_SIZE = 0x3022;
constexpr EGLint EGL_GREEN_SIZE = 0x3023;
constexpr EGLint EGL_RED_SIZE = 0x3024;
constexpr EGLint EGL_DEPTH_SIZE = 0x3025;
constexpr EGLint EGL_STENCIL_SIZE = 0x3026;
constexpr EGLint EGL_CONFIG_CAVEAT = 0x3027;
constexpr EGLint EGL_CONFIG_ID = 0x3028;
constexpr EGLint EGL_LEVEL = 0x3029;
constexpr EGLint EGL_MAX_PBUFFER_HEIGHT = 0x302A;
constexpr EGLint EGL_MAX_PBUFFER_PIXELS = 0x302B;
constexpr EGLint EGL_MAX_PBUFFER_WIDTH = 0x302C;
constexpr EGLint EGL_NATIVE_RENDERABLE = 0x302D;
constexpr EGLint EGL_NATIVE_VISUAL_ID = 0x302E;
constexpr EGLint EGL_NATIVE_VISUAL_TYPE = 0x302F;
constexpr EGLint EGL_SAMPLES = 0x3031;
constexpr EGLint EGL_SAMPLE_BUFFERS = 0x3032;
constexpr EGLint EGL_SURFACE_TYPE = 0x3033;
constexpr EGLint EGL_TRANSPARENT_TYPE = 0x3034;
constexpr EGLint EGL_TRANSPARENT_BLUE_VALUE = 0x3035;
constexpr EGLint EGL_TRANSPARENT_GREEN_VALUE = 0x3036;
constexpr EGLint EGL_TRANSPARENT_RED_VALUE = 0x3037;
constexpr EGLint EGL_NONE = 0x3038;
constexpr EGLint EGL_BIND_TO_TEXTURE_RGB = 0x3039;
constexpr EGLint EGL_BIND_TO_TEXTURE_RGBA = 0x303A;
constexpr EGLint EGL_MIN_SWAP_INTERVAL = 0x303B;
constexpr EGLint EGL_MAX_SWAP_INTERVAL = 0x303C;
constexpr EGLint EGL_DONT_CARE = ((EGLint)-1);
constexpr EGLint EGL_SLOW_CONFIG = 0x3050;
constexpr EGLint EGL_NON_CONFORMANT_CONFIG = 0x3051;
constexpr EGLint EGL_TRANSPARENT_RGB = 0x3052;
constexpr EGLint EGL_NO_TEXTURE = 0x305C;
constexpr EGLint EGL_TEXTURE_RGB = 0x305D;
constexpr EGLint EGL_TEXTURE_RGBA = 0x305E;
constexpr EGLint EGL_TEXTURE_2D = 0x305F;
constexpr EGLint EGL_PBUFFER_BIT = 0x01;
constexpr EGLint EGL_PIXMAP_BIT = 0x02;
constexpr EGLint EGL_WINDOW_BIT = 0x04;
constexpr EGLint EGL_VENDOR = 0x3053;
constexpr EGLint EGL_VERSION = 0x3054;
constexpr EGLint EGL_EXTENSIONS = 0x3055;
constexpr EGLint EGL_HEIGHT = 0x3056;
constexpr EGLint EGL_WIDTH = 0x3057;
constexpr EGLint EGL_LARGEST_PBUFFER = 0x3058;
constexpr EGLint EGL_TEXTURE_FORMAT = 0x3080;
constexpr EGLint EGL_TEXTURE_TARGET = 0x3081;
constexpr EGLint EGL_MIPMAP_TEXTURE = 0x3082;
constexpr EGLint EGL_MIPMAP_LEVEL = 0x3083;
constexpr EGLint EGL_BACK_BUFFER = 0x3084;
constexpr EGLint EGL_DRAW = 0x3059;
constexpr EGLint EGL_READ = 0x305A;
constexpr EGLint EGL_CORE_NATIVE_ENGINE = 0x305B;
constexpr EGLint EGL_RENDERABLE_TYPE = 0x3040;
constexpr EGLint EGL_OPENGL_API = 0x30A2;
constexpr EGLint EGL_OPENGL_BIT = 0x0008;
constexpr EGLint EGL_CONTEXT_MAJOR_VERSION = 0x3098;
constexpr EGLint EGL_CONTEXT_MINOR_VERSION = 0x30FB;
constexpr EGLint EGL_CONTEXT_OPENGL_PROFILE_MASK = 0x30FD;
constexpr EGLint EGL_CONTEXT_OPENGL_RESET_NOTIFICATION_STRATEGY = 0x31BD;
constexpr EGLint EGL_NO_RESET_NOTIFICATION = 0x31BE;
constexpr EGLint EGL_LOSE_CONTEXT_ON_RESET = 0x31BF;
constexpr EGLint EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT = 0x00000001;
constexpr EGLint EGL_CONTEXT_OPENGL_COMPATIBILITY_PROFILE_BIT = 0x00000002;
constexpr EGLint EGL_CONTEXT_OPENGL_DEBUG = 0x31B0;
constexpr EGLint EGL_CONTEXT_OPENGL_FORWARD_COMPATIBLE = 0x31B1;
constexpr EGLint EGL_CONTEXT_OPENGL_ROBUST_ACCESS = 0x31B2;
constexpr EGLint EGL_DISPLAY_SCALING = 10000;
constexpr EGLint EGL_HORIZONTAL_RESOLUTION = 3090;

using PFNEGLCHOOSECONFIGPROC = EGLBoolean(EGLAPIENTRYP*)(
   EGLDisplay dpy, const EGLint* attrib_list, EGLConfig* configs,
   EGLint config_size, EGLint* num_config);
using PFNEGLCOPYBUFFERSPROC = EGLBoolean(EGLAPIENTRYP*)(
   EGLDisplay dpy, EGLSurface surface, EGLNativePixmapType target);
using PFNEGLCREATECONTEXTPROC = EGLContext(EGLAPIENTRYP*)(
   EGLDisplay dpy, EGLConfig config, EGLContext share_context,
   const EGLint* attrib_list);
using PFNEGLCREATEPBUFFERSURFACEPROC = EGLSurface(EGLAPIENTRYP*)(
   EGLDisplay dpy, EGLConfig config, const EGLint* attrib_list);
using PFNEGLCREATEPIXMAPSURFACEPROC = EGLSurface(EGLAPIENTRYP*)(
   EGLDisplay dpy, EGLConfig config, EGLNativePixmapType pixmap,
   const EGLint* attrib_list);
using PFNEGLCREATEWINDOWSURFACEPROC = EGLSurface(EGLAPIENTRYP*)(
   EGLDisplay dpy, EGLConfig config, EGLNativeWindowType win,
   const EGLint* attrib_list);
using PFNEGLDESTROYCONTEXTPROC =
   EGLBoolean(EGLAPIENTRYP*)(EGLDisplay dpy, EGLContext ctx);
using PFNEGLDESTROYSURFACEPROC =
   EGLBoolean(EGLAPIENTRYP*)(EGLDisplay dpy, EGLSurface surface);
using PFNEGLGETCONFIGATTRIBPROC = EGLBoolean(EGLAPIENTRYP*)(
   EGLDisplay dpy, EGLConfig config, EGLint attribute, EGLint* value);
using PFNEGLGETCONFIGSPROC = EGLBoolean(EGLAPIENTRYP*)(
   EGLDisplay dpy, EGLConfig* configs, EGLint config_size, EGLint* num_config);
using PFNEGLGETCURRENTDISPLAYPROC = EGLDisplay(EGLAPIENTRYP*)(void);
using PFNEGLGETCURRENTSURFACEPROC = EGLSurface(EGLAPIENTRYP*)(EGLint readdraw);
using PFNEGLGETDISPLAYPROC =
   EGLDisplay(EGLAPIENTRYP*)(EGLNativeDisplayType display_id);
using PFNEGLGETERRORPROC = EGLint(EGLAPIENTRYP*)(void);
using PFNEGLGETPROCADDRESSPROC = void*(EGLAPIENTRYP*)(const char* procname);
using PFNEGLINITIALIZEPROC =
   EGLBoolean(EGLAPIENTRYP*)(EGLDisplay dpy, EGLint* major, EGLint* minor);
using PFNEGLMAKECURRENTPROC = EGLBoolean(EGLAPIENTRYP*)(
   EGLDisplay dpy, EGLSurface draw, EGLSurface read, EGLContext ctx);
using PFNEGLQUERYCONTEXTPROC = EGLBoolean(EGLAPIENTRYP*)(
   EGLDisplay dpy, EGLContext ctx, EGLint attribute, EGLint* value);
using PFNEGLQUERYSTRINGPROC =
   const char*(EGLAPIENTRYP*)(EGLDisplay dpy, EGLint name);
using PFNEGLQUERYSURFACEPROC = EGLBoolean(EGLAPIENTRYP*)(
   EGLDisplay dpy, EGLSurface surface, EGLint attribute, EGLint* value);
using PFNEGLSWAPBUFFERSPROC =
   EGLBoolean(EGLAPIENTRYP*)(EGLDisplay dpy, EGLSurface surface);
using PFNEGLTERMINATEPROC = EGLBoolean(EGLAPIENTRYP*)(EGLDisplay dpy);
using PFNEGLSURFACEATTRIBPROC = EGLBoolean(EGLAPIENTRYP*)(
   EGLDisplay dpy, EGLSurface surface, EGLint attribute, EGLint value);
using PFNEGLSWAPINTERVALPROC =
   EGLBoolean(EGLAPIENTRYP*)(EGLDisplay dpy, EGLint interval);
using PFNEGLBINDAPIPROC = EGLBoolean(EGLAPIENTRYP*)(EGLenum api);
using PFNEGLQUERYAPIPROC = EGLenum(EGLAPIENTRYP*)(void);
using PFNEGLGETCURRENTCONTEXTPROC = EGLContext(EGLAPIENTRYP*)(void);

struct EGLFunctions final : public GLFunctions
{
   PFNEGLCHOOSECONFIGPROC ChooseConfig { nullptr };
   PFNEGLCOPYBUFFERSPROC CopyBuffers { nullptr };
   PFNEGLCREATECONTEXTPROC CreateContext { nullptr };
   PFNEGLCREATEPBUFFERSURFACEPROC CreatePbufferSurface { nullptr };
   PFNEGLCREATEPIXMAPSURFACEPROC CreatePixmapSurface { nullptr };
   PFNEGLCREATEWINDOWSURFACEPROC CreateWindowSurface { nullptr };
   PFNEGLDESTROYCONTEXTPROC DestroyContext { nullptr };
   PFNEGLDESTROYSURFACEPROC DestroySurface { nullptr };
   PFNEGLGETCONFIGATTRIBPROC GetConfigAttrib { nullptr };
   PFNEGLGETCONFIGSPROC GetConfigs { nullptr };
   PFNEGLGETCURRENTDISPLAYPROC GetCurrentDisplay { nullptr };
   PFNEGLGETCURRENTSURFACEPROC GetCurrentSurface { nullptr };
   PFNEGLGETDISPLAYPROC GetDisplay { nullptr };
   PFNEGLGETERRORPROC GetError { nullptr };
   PFNEGLGETPROCADDRESSPROC GetProcAddress { nullptr };
   PFNEGLINITIALIZEPROC Initialize { nullptr };
   PFNEGLMAKECURRENTPROC MakeCurrent { nullptr };
   PFNEGLQUERYCONTEXTPROC QueryContext { nullptr };
   PFNEGLQUERYSTRINGPROC QueryString { nullptr };
   PFNEGLQUERYSURFACEPROC QuerySurface { nullptr };
   PFNEGLSWAPBUFFERSPROC SwapBuffers { nullptr };
   PFNEGLTERMINATEPROC Terminate { nullptr };
   PFNEGLSURFACEATTRIBPROC SurfaceAttrib { nullptr };
   PFNEGLSWAPINTERVALPROC SwapInterval { nullptr };
   PFNEGLBINDAPIPROC BindAPI { nullptr };
   PFNEGLQUERYAPIPROC QueryAPI { nullptr };
   PFNEGLGETCURRENTCONTEXTPROC GetCurrentContext { nullptr };

   EGLFunctions()
   {
      mEGLLibrary = dlopen("libEGL.so", RTLD_LAZY);

      if (mEGLLibrary == nullptr)
         return;

      GetProcAddress = reinterpret_cast<PFNEGLGETPROCADDRESSPROC>(
         dlsym(mEGLLibrary, "eglGetProcAddress"));

      if (GetProcAddress == nullptr)
         return;

      mIsOk = true;

      mIsOk = GetFunction(ChooseConfig, "eglChooseConfig", true) && mIsOk;
      mIsOk = GetFunction(CopyBuffers, "eglCopyBuffers", true) && mIsOk;
      mIsOk = GetFunction(CreateContext, "eglCreateContext", true) && mIsOk;
      mIsOk =
         GetFunction(CreatePbufferSurface, "eglCreatePbufferSurface", true) &&
         mIsOk;
      mIsOk =
         GetFunction(CreatePixmapSurface, "eglCreatePixmapSurface", true) &&
         mIsOk;
      mIsOk =
         GetFunction(CreateWindowSurface, "eglCreateWindowSurface", true) &&
         mIsOk;
      mIsOk = GetFunction(DestroyContext, "eglDestroyContext", true) && mIsOk;
      mIsOk = GetFunction(DestroySurface, "eglDestroySurface", true) && mIsOk;
      mIsOk = GetFunction(GetConfigAttrib, "eglGetConfigAttrib", true) && mIsOk;
      mIsOk = GetFunction(GetConfigs, "eglGetConfigs", true) && mIsOk;
      mIsOk =
         GetFunction(GetCurrentDisplay, "eglGetCurrentDisplay", true) && mIsOk;
      mIsOk =
         GetFunction(GetCurrentSurface, "eglGetCurrentSurface", true) && mIsOk;
      mIsOk = GetFunction(GetDisplay, "eglGetDisplay", true) && mIsOk;
      mIsOk = GetFunction(GetError, "eglGetError", true) && mIsOk;
      mIsOk = GetFunction(GetProcAddress, "eglGetProcAddress", true) && mIsOk;
      mIsOk = GetFunction(Initialize, "eglInitialize", true) && mIsOk;
      mIsOk = GetFunction(MakeCurrent, "eglMakeCurrent", true) && mIsOk;
      mIsOk = GetFunction(QueryContext, "eglQueryContext", true) && mIsOk;
      mIsOk = GetFunction(QueryString, "eglQueryString", true) && mIsOk;
      mIsOk = GetFunction(QuerySurface, "eglQuerySurface", true) && mIsOk;
      mIsOk = GetFunction(SwapBuffers, "eglSwapBuffers", true) && mIsOk;
      mIsOk = GetFunction(Terminate, "eglTerminate", true) && mIsOk;
      mIsOk = GetFunction(SurfaceAttrib, "eglSurfaceAttrib", true) && mIsOk;
      mIsOk = GetFunction(SwapInterval, "eglSwapInterval", true) && mIsOk;
      mIsOk = GetFunction(BindAPI, "eglBindAPI", true) && mIsOk;
      mIsOk = GetFunction(QueryAPI, "eglQueryAPI", true) && mIsOk;
      mIsOk =
         GetFunction(GetCurrentContext, "eglGetCurrentContext", true) && mIsOk;

      mIsOk = mIsOk && LoadFunctions();
   }

   ~EGLFunctions()
   {
   }

   void* GetFunctionPointer(const char* name) const
   {
      return GetProcAddress != nullptr ? GetProcAddress(name) : nullptr;
   }

   bool IsOk() const noexcept
   {
      return mIsOk;
   }

private:
   void* mEGLLibrary { nullptr };
   bool mIsOk { false };
};

namespace
{
EGLNativeDisplayType GetNativeDisplay()
{
   auto gdkDisplay = gdk_display_get_default();

#ifdef GDK_WINDOWING_WAYLAND
   if (GDK_IS_WAYLAND_DISPLAY(gdkDisplay))
      return reinterpret_cast<EGLNativeDisplayType>(
         gdk_wayland_display_get_wl_display(gdkDisplay));
#endif

#ifdef GDK_WINDOWING_X11
   if (GDK_IS_X11_DISPLAY(gdkDisplay))
      return reinterpret_cast<EGLNativeDisplayType>(
         gdk_x11_display_get_xdisplay(gdkDisplay));
#endif

   return nullptr;
}

const EGLint configAttribs[] = {
   EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
   EGL_BLUE_SIZE, 8,
   EGL_GREEN_SIZE, 8,
   EGL_RED_SIZE, 8,
   EGL_DEPTH_SIZE, 0,
   EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT,
   EGL_NONE
};

const EGLint contextAttribs[] = {
   EGL_CONTEXT_MAJOR_VERSION, 3,
   EGL_CONTEXT_MINOR_VERSION, 2,
   EGL_CONTEXT_OPENGL_PROFILE_MASK, EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT,
   EGL_NONE
};
} // namespace

class EGLContextWrapper final : public Context
{
public:
   EGLContextWrapper(EGLRenderer& renderer, const EGLFunctions& functions)
       : Context(functions)
       , mRenderer(renderer)
       , mFunctions(functions)
   {
      mDisplay = mFunctions.GetDisplay(GetNativeDisplay());

      EGLint minor, major;

      if (!mFunctions.Initialize(mDisplay, &major, &minor))
         return;

      if (!mFunctions.BindAPI(EGL_OPENGL_API))
         return;

      EGLint numConfigs;
      if (
         !mFunctions.ChooseConfig(
            mDisplay, configAttribs, &mConfig, 1, &numConfigs) ||
         numConfigs != 1)
         return;



      mContext =
         mFunctions.CreateContext(mDisplay, mConfig, nullptr, contextAttribs);
   }

   explicit EGLContextWrapper(EGLRenderer& renderer,
      const EGLContextWrapper& parentContext, GtkWidget* widget)
       : Context(parentContext.mFunctions)
       , mRenderer(renderer)
       , mFunctions(parentContext.mFunctions)
       , mDisplay(parentContext.mDisplay)
       , mConfig(parentContext.mConfig)
       , mWidget(widget)
       , mParentContext(parentContext.mContext)
   {

   }

   ~EGLContextWrapper()
   {
      Publish(ContextDestroyedMessage {});
      mRenderer.ContextDestroyed(*this);

      if (mSurface != nullptr)
         mFunctions.DestroySurface(mDisplay, mSurface);

#ifdef GDK_WINDOWING_WAYLAND
      if (mWaylandWindow != nullptr)
         wl_egl_window_destroy(mWaylandWindow);
#endif

      if (mContext != nullptr)
         mFunctions.DestroyContext(mDisplay, mContext);
   }

   Size GetSize() const override
   {
      if (mSurface == nullptr)
         return {};

      EGLint width, height;

      mFunctions.QuerySurface(mDisplay, mSurface, EGL_WIDTH, &width);
      mFunctions.QuerySurface(mDisplay, mSurface, EGL_HEIGHT, &height);

      return { float(width), float(height) };
   }

   void ProcessReleaseQueue() override
   {
      if (mContext == mFunctions.GetCurrentContext())
         DoProcessReleaseQueue();
   }

   bool IsOk() const noexcept
   {
      return mContext != nullptr;
   }

   void BeginRendering()
   {
      if (mContext == nullptr)
         TryToCreateWindowContext();

      if (mContext != mFunctions.GetCurrentContext())
      {
         if(!mFunctions.MakeCurrent(mDisplay, mSurface, mSurface, mContext))
         {
            assert(false);
            return;
         }
      }

      if(mSurface != nullptr)
      {
         EGLint pixelsPerInch {};

         mFunctions.QuerySurface(mDisplay, mSurface, EGL_HORIZONTAL_RESOLUTION, &pixelsPerInch);

         if (pixelsPerInch == 0)
            pixelsPerInch = 96;
         else
            pixelsPerInch = static_cast<EGLint>(pixelsPerInch / (EGL_DISPLAY_SCALING * 0.0254f));

         UpdateScreenProperties(pixelsPerInch, std::max(1.0f, pixelsPerInch / 96.0f));
      }

      if (!mInitialized)
      {
         mFunctions.SwapInterval(mDisplay, 0);

         SetupContext();
         mInitialized = true;
      }

      ProcessReleaseQueue();

      auto& functions = GetFunctions();

      if (mGPUSync != nullptr)
      {
         functions.ClientWaitSync(
            mGPUSync, static_cast<GLbitfield>(GLenum::SYNC_FLUSH_COMMANDS_BIT),
            TIMEOUT_IGNORED);

         functions.DeleteSync(mGPUSync);
      }

      mGPUSync =
         GetFunctions().FenceSync(GLenum::SYNC_GPU_COMMANDS_COMPLETE, 0);
   }

   void EndRendering()
   {
      if (mSurface != nullptr)
      {
            if(!mFunctions.SwapBuffers(mDisplay, mSurface))
            {
               assert(false);
            }
      }
   }

   void TryToCreateWindowContext()
   {
      if (mContext != nullptr || mWidget == nullptr)
         return;

      auto window = gtk_widget_get_window(mWidget);

      if (window == nullptr)
         return;

#ifdef GDK_WINDOWING_WAYLAND
      if (GDK_IS_WAYLAND_WINDOW(window))
      {
         auto wlSurface = gdk_wayland_window_get_wl_surface(window);
         mWaylandWindow = wl_egl_window_get_egl_window(wlSurface);
         mSurface = mFunctions.CreateWindowSurface(
            mDisplay, mConfig, reinterpret_cast<EGLNativeDisplayType>(mWaylandWindow, nullptr));
      }
#endif
#ifdef GDK_WINDOWING_X11
      if (GDK_IS_X11_WINDOW(window))
      {
         gtk_widget_set_double_buffered(mWidget, false);

         auto xid = GDK_WINDOW_XID(window);
         mSurface = mFunctions.CreateWindowSurface(
            mDisplay, mConfig, reinterpret_cast<EGLNativeDisplayType>(xid), nullptr);
      }
#endif

      if(mSurface != nullptr)
         mContext = mFunctions.CreateContext(mDisplay, mConfig, mParentContext, contextAttribs);
   }

private:
   EGLRenderer& mRenderer;
   const EGLFunctions& mFunctions;

   EGLDisplay mDisplay { nullptr };
   EGLConfig mConfig { nullptr };
   EGLContext mContext { nullptr };

   GtkWidget* mWidget { nullptr };
   EGLContext mParentContext { nullptr };
#ifdef GDK_WINDOWING_WAYLAND
   struct wl_egl_window *mWaylandWindow { nullptr };
#endif

   EGLSurface mSurface { nullptr };

   GLsync mGPUSync { nullptr };
   bool mInitialized { false };
}; // class EGLContext

EGLRenderer::EGLRenderer()
    : mEGLFunctions(std::make_unique<EGLFunctions>())
{
   if (mEGLFunctions->IsOk())
      mEGLContext = std::make_unique<EGLContextWrapper>(*this, *mEGLFunctions);

   if (mEGLContext->IsOk())
      mEGLContext->BeginRendering();
}

EGLRenderer::~EGLRenderer()
{
}

bool EGLRenderer::IsAvailable() const
{
   return mEGLContext != nullptr && mEGLContext->IsOk();
}

Context& EGLRenderer::GetResourceContext()
{
   assert(IsAvailable());
   return *mEGLContext;
}

std::unique_ptr<Context> EGLRenderer::CreateContext(void* window)
{
   GtkWidget* widget = reinterpret_cast<GtkWidget*>(window);
   return std::make_unique<EGLContextWrapper>(*this, *mEGLContext, widget);
}

void EGLRenderer::ContextDestroyed(Context& ctx)
{
   if (mCurrentContext == &ctx)
   {
      if (mCurrentContext != mEGLContext.get())
      {
         mCurrentContext = mEGLContext.get();
         mEGLContext->BeginRendering();
      }
   }
}

void EGLRenderer::BeginRendering(Context& context)
{
   mCurrentContext = static_cast<EGLContextWrapper*>(&context);
   mCurrentContext->BeginRendering();
}

void EGLRenderer::EndRendering()
{
   if (mCurrentContext != nullptr)
      mCurrentContext->EndRendering();
}

} // namespace graphics::gl::platforms::linux_like
