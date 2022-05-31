/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  WGLRenderer.cpp

  Dmitry Vedenko

**********************************************************************/
#include "WGLRenderer.h"

#include "graphics/gl/GLFunctions.h"
#include "graphics/gl/Context.h"

#include "MemoryX.h"

#include "wx/log.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef ERROR

namespace graphics::gl::platforms::windows
{
namespace
{
namespace
{
auto registered = ([](){
                      return RegisterRendererFactory([](){
                                                        return std::make_unique<WGLRenderer>();
                                                     });
                   })();
}

LPCWSTR WindowClassName = L"AudacityWGLWindow-6381EF473EC54DCD886ACFC36775B1E0";

constexpr int GL_TRUE = 1;
constexpr int WGL_DRAW_TO_WINDOW_ARB = 0x2001;
constexpr int WGL_SUPPORT_OPENGL_ARB = 0x2010;
constexpr int WGL_DOUBLE_BUFFER_ARB = 0x2011;
constexpr int WGL_ACCELERATION_ARB = 0x2003;
constexpr int WGL_FULL_ACCELERATION_ARB = 0x2027;
constexpr int WGL_PIXEL_TYPE_ARB = 0x2013;
constexpr int WGL_TYPE_RGBA_ARB = 0x202B;
constexpr int WGL_COLOR_BITS_ARB = 0x2014;
constexpr int WGL_DEPTH_BITS_ARB = 0x2022;
constexpr int WGL_STENCIL_BITS_ARB = 0x2023;

constexpr int WGL_CONTEXT_MAJOR_VERSION_ARB = 0x2091;
constexpr int WGL_CONTEXT_MINOR_VERSION_ARB = 0x2092;
constexpr int WGL_CONTEXT_PROFILE_MASK_ARB = 0x9126;
constexpr int WGL_CONTEXT_CORE_PROFILE_BIT_ARB = 0x0001;
constexpr int WGL_CONTEXT_FLAGS_ARB = 0x2094;
constexpr int WGL_CONTEXT_DEBUG_BIT_ARB = 0x0001;

constexpr int WGL_CONTEXT_RELEASE_BEHAVIOR_ARB = 0x2097;
constexpr int WGL_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB = 0x0000;

constexpr int WGL_SAMPLE_BUFFERS_ARB = 0x2041;
constexpr int WGL_SAMPLES_ARB = 0x2042;

using pfnWGLCreateContext = HGLRC(__stdcall*)(HDC);
using pfnWGLDeleteContext = BOOL(__stdcall*)(HGLRC);
using pfnWGLMakeCurrent = BOOL(__stdcall*)(HDC, HGLRC);
using pfnWGLGetCurrentContext = HGLRC(__stdcall*)();
using pfnWGLGetProcAddress = PROC(__stdcall*)(LPCSTR);

using pfnWGLSwapIntervalEXT = BOOL(__stdcall*)(int interval);
using pfnWGLGetSwapIntervalEXT = int (__stdcall*)();

using pfnWGLCreateContextAttribsARB = HGLRC(__stdcall*)(HDC, HGLRC, const int*);
using pfnWGLChoosePixelFormatARB =
   BOOL(__stdcall*)(HDC, const int*, const FLOAT*, UINT, int*, UINT*);

#ifndef NDEBUG

using GLDEBUGPROCARB = void(__stdcall*)(
   GLDebugSource source, GLDebugType type, GLuint id, GLDebugSeverity severity,
   GLsizei length, const GLchar* message, const void* userParam);

using pfnGLDebugMessageCallbackARB =
   void(__stdcall*)(GLDEBUGPROCARB callback, void* userParam);

#endif // !NDEBUG

struct WGLFunctions final : public GLFunctions
{
   explicit WGLFunctions(HMODULE glLib)
       : mGLLibrary(glLib)
   {
      LoadWGLFunctions();
   }

   void* GetFunctionPointer(const char* name) const override
   {
      auto wglFunction = WGLGetProcAddress(name);

      if (wglFunction != nullptr)
         return wglFunction;

      return GetProcAddress(mGLLibrary, name);
   }

   bool IsOk() const
   {
      return mLoaded;
   }

   pfnWGLCreateContext WGLCreateContext { nullptr };
   pfnWGLDeleteContext WGLDeleteContext { nullptr };
   pfnWGLMakeCurrent WGLMakeCurrent { nullptr };
   pfnWGLGetProcAddress WGLGetProcAddress { nullptr };
   pfnWGLGetCurrentContext WGLGetCurrentContext { nullptr };
   pfnWGLSwapIntervalEXT WGLSwapIntervalEXT { nullptr };
   pfnWGLGetSwapIntervalEXT WLGGetSwapIntervalEXT { nullptr };

   bool LoadWGLFunctions()
   {
      if (!GetFunction(WGLCreateContext, "wglCreateContext"))
         return false;

      if (!GetFunction(WGLDeleteContext, "wglDeleteContext"))
         return false;

      if (!GetFunction(WGLMakeCurrent, "wglMakeCurrent"))
         return false;

      if (!GetFunction(WGLGetCurrentContext, "wglGetCurrentContext"))
         return false;

      if (!GetFunction(WGLGetProcAddress, "wglGetProcAddress"))
         return false;

      return true;
   }

   bool LoadContextFunctions()
   {
      mLoaded = LoadFunctions();

      if (!GetFunction(WGLSwapIntervalEXT, "wglSwapIntervalEXT"))
         return false;

      if (!GetFunction(WLGGetSwapIntervalEXT, "wglGetSwapIntervalEXT"))
         return false;
      
      return mLoaded;
   }

private:
   template <typename Fn> bool GetFunction(Fn& fn, const char* name)
   {
      fn = reinterpret_cast<Fn>(GetProcAddress(mGLLibrary, name));

      if (fn == nullptr && WGLGetProcAddress != nullptr)
         fn = reinterpret_cast<Fn>(WGLGetProcAddress(name));

      return fn != nullptr;
   }

   HMODULE mGLLibrary;
   bool mLoaded;
};
} // namespace

class WGLRenderer::WGLContext : public Context
{
public:
   WGLContext(
      WGLRenderer& renderer, WGLFunctions& functions, HDC dc, HGLRC glrc, bool ownsContext)
       : Context(functions)
       , mRenderer(renderer)
       , mWGLFunctions(functions)
       , mDC(dc)
       , mGLRC(glrc)
       , mOwnsContext(ownsContext)
   {
   }

   ~WGLContext()
   {
      Publish(ContextDestroyedMessage {});
      mRenderer.ContextDestroyed(*this);

      if (mWGLFunctions.WGLGetCurrentContext() == mGLRC)
         mWGLFunctions.WGLMakeCurrent(nullptr, nullptr);
      
      if (mOwnsContext)
      {
         mWGLFunctions.WGLDeleteContext(mGLRC);
         ReleaseDC(WindowFromDC(mDC), mDC);
      }
   }

   Size GetSize() const override
   {
      RECT clientRect;
      GetClientRect(WindowFromDC(mDC), &clientRect);

      return { static_cast<float>(clientRect.right - clientRect.left),
               static_cast<float>(clientRect.bottom - clientRect.top) };
   }

   void BeginRendering()
   {
      if (mWGLFunctions.WGLGetCurrentContext() != mGLRC)
         mWGLFunctions.WGLMakeCurrent(mDC, mGLRC);

      const auto pixelsPerInch = GetDeviceCaps(mDC, LOGPIXELSX);

      UpdateScreenProperties(
         pixelsPerInch, std::max(1.0f, pixelsPerInch / 96.0f));

      mBound = true;

      if (!mInitialized)
      {
         if (
            mWGLFunctions.WLGGetSwapIntervalEXT != nullptr &&
            mWGLFunctions.WLGGetSwapIntervalEXT() != 0)
            mWGLFunctions.WGLSwapIntervalEXT(0);

         SetupContext();
         mInitialized = true;
      }

      ProcessReleaseQueue();
   }

   void EndRendering()
   {
      SwapBuffers(mDC);
   }

   void ProcessReleaseQueue() override
   {
      if (!mBound)
         return;

      DoProcessReleaseQueue();
   }

   bool IsBound() const noexcept
   {
      return mBound;
   }

   void Unbind() noexcept
   {
      mBound = false;
   }

   void EnsureBound()
   {
      if (!mBound || mWGLFunctions.WGLGetCurrentContext() != mGLRC)
         BeginRendering();
   }

private:
   WGLRenderer& mRenderer;
   WGLFunctions& mWGLFunctions;
   HDC mDC;
   HGLRC mGLRC;

   bool mOwnsContext;
   bool mBound { false };

   bool mInitialized { false };
};

class WGLRenderer::InvisibleWindow final
{
public:
   explicit InvisibleWindow(WGLRenderer& renderer)
       : mRenderer(renderer)
   {
      WNDCLASSW windowClass = {};

      windowClass.style = CS_HREDRAW | CS_VREDRAW | CS_OWNDC;
      windowClass.lpfnWndProc = DefWindowProcA;
      windowClass.hInstance = GetModuleHandle(nullptr);
      windowClass.lpszClassName = WindowClassName;

      if (!RegisterClassW(&windowClass))
         return;

      if (!ReCreateWindow())
         return;

      if (!LoadOpenGL())
         return;

      if (!LoadWGLExtensions())
         return;

      if (!ReCreateWindow())
         return;

      mGLContext = CreateOpenGL3Context(mDC, nullptr);

      if (mGLContext == nullptr)
         return;

      LoadGLFunctions();
   }

   ~InvisibleWindow()
   {
      if (mGLContext != nullptr)
         mFunctions->WGLDeleteContext(mGLContext);

      if (mGLLibrary != nullptr)
         FreeLibrary(mGLLibrary);

      if (mDC != nullptr)
         ReleaseDC(mWindow, mDC);

      if (mWindow != nullptr)
         DestroyWindow(mWindow);

      UnregisterClassW(WindowClassName, GetModuleHandle(nullptr));
   }

   bool IsOk() const
   {
      return mFunctions != nullptr && mFunctions->IsOk();
   }

   HGLRC CreateOpenGL3Context(HDC dc, HGLRC baseContext = nullptr)
   {
      const int pixelFormatAttribs[] = {
         WGL_DRAW_TO_WINDOW_ARB, GL_TRUE,
         WGL_SUPPORT_OPENGL_ARB, GL_TRUE,
         WGL_DOUBLE_BUFFER_ARB,  GL_TRUE,
         WGL_ACCELERATION_ARB,   WGL_FULL_ACCELERATION_ARB,
         WGL_PIXEL_TYPE_ARB,     WGL_TYPE_RGBA_ARB,
         WGL_COLOR_BITS_ARB,     32,
         WGL_DEPTH_BITS_ARB,     0,
         WGL_STENCIL_BITS_ARB,   0,
      #if 0
         WGL_SAMPLE_BUFFERS_ARB, GL_TRUE,
         WGL_SAMPLES_ARB,        2,
      #endif   
         0,
      };

      int pixelFormat;
      UINT numFormats;

      WGLChoosePixelFormatARB(
         dc, pixelFormatAttribs, 0, 1, &pixelFormat, &numFormats);

      if (numFormats == 0)
         return nullptr;

      PIXELFORMATDESCRIPTOR pfd = {};

      DescribePixelFormat(dc, pixelFormat, sizeof(pfd), &pfd);

      if (!SetPixelFormat(dc, pixelFormat, &pfd))
         return nullptr;

      const int glAttribs[] = {
         WGL_CONTEXT_MAJOR_VERSION_ARB,
         3,
         WGL_CONTEXT_MINOR_VERSION_ARB,
         2,
         WGL_CONTEXT_PROFILE_MASK_ARB,
         WGL_CONTEXT_CORE_PROFILE_BIT_ARB,
         WGL_CONTEXT_RELEASE_BEHAVIOR_ARB,
         WGL_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB,
#ifndef NDEBUG
         WGL_CONTEXT_FLAGS_ARB,
         WGL_CONTEXT_DEBUG_BIT_ARB,
#endif // !NDEBUG

         0,
      };

      auto context = WGLCreateContextAttribsARB(mDC, baseContext, glAttribs);
#ifndef NDEBUG
      if (context != nullptr)
      {
         mFunctions->WGLMakeCurrent(mDC, context);
         auto contextSwitcher =
            finally([this]() { mFunctions->WGLMakeCurrent(mDC, nullptr); });

         if (
            GLDebugMessageCallbackARB != nullptr ||
            GetARBFunction(
               GLDebugMessageCallbackARB, "glDebugMessageCallbackARB"))
            GLDebugMessageCallbackARB(DebugCallback, this);

         pfnEnable GLEnable;
         GetFunction(GLEnable, "glEnable");

         GLEnable(GLenum::DEBUG_OUTPUT);
         GLEnable(GLenum::DEBUG_OUTPUT_SYNCHRONOUS);
      }
#endif // !NDEBUG

      return context;
   }

   bool LoadGLFunctions()
   {
      mFunctions->WGLMakeCurrent(mDC, mGLContext);
      auto contextSwitcher =
         finally([this]() { mFunctions->WGLMakeCurrent(mDC, nullptr); });

      mFunctions->LoadContextFunctions();

      return mFunctions->IsOk();
   }

   std::unique_ptr<WGLContext> CreateContext(HDC dc, HGLRC glrc)
   {
      return std::make_unique<WGLContext>(
         mRenderer, *mFunctions, dc,
         glrc != nullptr ? glrc : CreateOpenGL3Context(dc, mGLContext),
         glrc == nullptr);
   }

   std::unique_ptr<WGLContext> CreateContextWrapper()
   {
      return CreateContext(mDC, mGLContext);
   }

private:
   template <typename Fn> bool GetFunction(Fn& fn, const char* name)
   {
      fn = reinterpret_cast<Fn>(GetProcAddress(mGLLibrary, name));
      return fn != nullptr;
   }

   template <typename Fn> bool GetARBFunction(Fn& fn, const char* name)
   {
      fn = reinterpret_cast<Fn>(mFunctions->WGLGetProcAddress(name));
      return fn != nullptr;
   }

   bool ReCreateWindow()
   {
      if (mDC != nullptr)
         ReleaseDC(mWindow, mDC);

      if (mWindow != nullptr)
         DestroyWindow(mWindow);

      mWindow = CreateWindowExW(
         0, WindowClassName, L"AudacityGL", 0, CW_USEDEFAULT, CW_USEDEFAULT,
         CW_USEDEFAULT, CW_USEDEFAULT, nullptr, nullptr,
         GetModuleHandle(nullptr), nullptr);

      if (mWindow == nullptr)
         return false;

      mDC = GetDC(mWindow);

      return mDC != nullptr;
   }

   bool LoadOpenGL()
   {
      mGLLibrary = LoadLibraryW(L"opengl32.dll");

      if (mGLLibrary == nullptr)
         return false;

      mFunctions = std::make_unique<WGLFunctions>(mGLLibrary);

      return true;
   }

   bool LoadWGLExtensions()
   {
      PIXELFORMATDESCRIPTOR pfd = {};

      pfd.nSize = sizeof(pfd);
      pfd.nVersion = 1;
      pfd.iPixelType = PFD_TYPE_RGBA;
      pfd.dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
      pfd.cColorBits = 24;
      pfd.cAlphaBits = 8;
      pfd.iLayerType = PFD_MAIN_PLANE;
      pfd.cDepthBits = 0;
      pfd.cStencilBits = 0;

      const auto pixelFormatIndex = ChoosePixelFormat(mDC, &pfd);

      if (pixelFormatIndex == 0)
         return false;

      if (!SetPixelFormat(mDC, pixelFormatIndex, &pfd))
         return false;

      HGLRC tempContext = mFunctions->WGLCreateContext(mDC);

      if (tempContext == nullptr)
         return false;

      auto tempContextDeleter = finally(
         [tempContext, this]() { mFunctions->WGLDeleteContext(tempContext); });

      if (!mFunctions->WGLMakeCurrent(mDC, tempContext))
         return false;

      auto contextSwitcher =
         finally([this]() { mFunctions->WGLMakeCurrent(mDC, nullptr); });

      if (!GetARBFunction(
             WGLCreateContextAttribsARB, "wglCreateContextAttribsARB"))
         return false;

      if (!GetARBFunction(WGLChoosePixelFormatARB, "wglChoosePixelFormatARB"))
         return false;

      return true;
   }

private:
#ifndef NDEBUG
   static void __stdcall DebugCallback(
      GLDebugSource source, GLDebugType type, GLuint id,
      GLDebugSeverity severity, GLsizei length, const GLchar* message,
      const void* userParam)
   {
      if (severity == GLDebugSeverity::NOTIFICATION)
         return;

      wxLogDebug(
         "GL CALLBACK: %s type = 0x%x, severity = 0x%x, message = %s",
         (type == GLDebugType::ERROR ? "** GL ERROR **" : ""), type, severity,
         message);

      if (type == GLDebugType::ERROR)
         DebugBreak();
   }
#endif // !NDEBUG
   WGLRenderer& mRenderer;
   
   HWND mWindow { nullptr };
   HDC mDC { nullptr };

   HMODULE mGLLibrary { nullptr };

   pfnWGLCreateContextAttribsARB WGLCreateContextAttribsARB { nullptr };
   pfnWGLChoosePixelFormatARB WGLChoosePixelFormatARB { nullptr };

#ifndef NDEBUG
   pfnGLDebugMessageCallbackARB GLDebugMessageCallbackARB { nullptr };
#endif // !NDEBUG

   HGLRC mGLContext { nullptr };

   std::unique_ptr<WGLFunctions> mFunctions;
};

WGLRenderer::WGLRenderer()
    : mInvisibleWindow(std::make_unique<InvisibleWindow>(*this))
{
   if (!mInvisibleWindow->IsOk())
   {
      mInvisibleWindow = {};
      return;
   }

   mInvisibleWindowContext = mInvisibleWindow->CreateContextWrapper();
   mCurrentContext = mInvisibleWindowContext.get();
   mCurrentContext->BeginRendering();
}

WGLRenderer::~WGLRenderer()
{
}

bool WGLRenderer::IsAvailable() const
{
   return mInvisibleWindow != nullptr;
}

Context& WGLRenderer::GetResourceContext()
{
   if (mCurrentContext == mInvisibleWindowContext.get())
      mCurrentContext->EnsureBound();
   
   return *mCurrentContext;
}

std::unique_ptr<graphics::gl::Context> WGLRenderer::CreateContext(void* window)
{
   return mInvisibleWindow->CreateContext(
      GetDC(static_cast<HWND>(window)), nullptr);
}

void WGLRenderer::ContextDestroyed(Context& ctx)
{
   if (&ctx == mInvisibleWindowContext.get())
      return;
   
   if (mCurrentContext == &ctx)
   {
      mCurrentContext = mInvisibleWindowContext.get();
      mInvisibleWindowContext->BeginRendering();
   }
}

void WGLRenderer::BeginRendering(Context& context)
{
   auto wglContext = static_cast<WGLContext*>(&context);

   if (mCurrentContext != nullptr && mCurrentContext != wglContext)
   {
      mCurrentContext->Unbind();
      mCurrentContext = wglContext;
   }
   
   mCurrentContext->BeginRendering();
}

void WGLRenderer::EndRendering()
{
   if (mCurrentContext == nullptr)
      return;

   mCurrentContext->EndRendering();
}

} // namespace graphics::gl::platforms::windows
