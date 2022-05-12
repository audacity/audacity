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

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

namespace graphics::gl::platforms::windows
{
namespace
{
LPCWSTR WindowClassName =
   L"AudacityWGLWindow-6381EF473EC54DCD886ACFC36775B1E0";

std::string GetSysErrorMessage(unsigned long errorCode)
{
   // get error message from system
   LPTSTR messageBuffer;

   if (
      ::FormatMessage(
         FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM |
            FORMAT_MESSAGE_IGNORE_INSERTS,
         NULL, errorCode, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
         (LPTSTR)&messageBuffer, 0, NULL) == 0)
   {
      return {};
   }

   if (messageBuffer == nullptr)
      return {};

   std::string result = messageBuffer;

   LocalFree(messageBuffer);

   return result;
}

constexpr int GL_TRUE = 1;
constexpr int WGL_DRAW_TO_WINDOW_ARB = 0x2001;
constexpr int WGL_SUPPORT_OPENGL_ARB = 0x2010;
constexpr int WGL_DOUBLE_BUFFER_ARB = 0x2011;
constexpr int WGL_ACCELERATION_ARB = 0x2003;
constexpr int WGL_FULL_ACCELERATION_ARB = 0x2027;
constexpr int WGL_PIXEL_TYPE_ARB = 0x2013;
constexpr int WGL_TYPE_RGBA_ARB = 0x202B;
constexpr int WGL_COLOR_BITS_ARB = 0x2014;

constexpr int WGL_CONTEXT_MAJOR_VERSION_ARB = 0x2091;
constexpr int WGL_CONTEXT_MINOR_VERSION_ARB = 0x2092;
constexpr int WGL_CONTEXT_PROFILE_MASK_ARB = 0x9126;
constexpr int WGL_CONTEXT_CORE_PROFILE_BIT_ARB = 0x0001;
constexpr int WGL_CONTEXT_FLAGS_ARB = 0x2094;
constexpr int WGL_CONTEXT_DEBUG_BIT_ARB = 0x0001;


using pfnWGLCreateContext = HGLRC(__stdcall*)(HDC);
using pfnWGLDeleteContext = BOOL(__stdcall*)(HGLRC);
using pfnWGLMakeCurrent = BOOL(__stdcall*)(HDC, HGLRC);
using pfnWGLGetProcAddress = PROC(__stdcall*)(LPCSTR);

using pfnWGLCreateContextAttribsARB = HGLRC (__stdcall*)(HDC, HGLRC, const int*);
using pfnWGLChoosePixelFormatARB =
   BOOL(__stdcall*)(HDC, const int*, const FLOAT*, UINT, int*, UINT*);

#ifndef NDEBUG

using GLDEBUGPROCARB = void(__stdcall*)(
   GLenum source, GLenum type, GLuint id, GLenum severity, GLsizei length,
   const GLchar* message, const void* userParam);

using pfnGLDebugMessageCallbackARB = void (__stdcall*)(GLDEBUGPROCARB callback, void* userParam);

#endif // !NDEBUG

struct WGLFunctions final : public GLFunctions
{
   WGLFunctions(HMODULE glLib, pfnWGLGetProcAddress WGLGetProcAddress)
       : mGLLibrary(glLib)
       , mWGLGetProcAddress(WGLGetProcAddress)
   {
      mLoaded = LoadFunctions();
   }

   void* GetFunctionPointer(const char* name) const override
   {
      auto wglFunction = mWGLGetProcAddress(name);

      if (wglFunction != nullptr)
         return wglFunction;

      return GetProcAddress(mGLLibrary, name);
   }

   bool IsOk() const
   {
      return mLoaded;
   }

private:
   HMODULE mGLLibrary;
   pfnWGLGetProcAddress mWGLGetProcAddress;
   bool mLoaded;
};
}

class WGLRenderer::WGLContext : public Context
{
public:
   WGLContext(
      GLFunctions& functions, pfnWGLMakeCurrent makeCurrent,
      pfnWGLDeleteContext deleteContext, HDC dc, HGLRC glrc, bool ownsContext)
       : Context(functions)
       , WGLMakeCurrent(makeCurrent)
       , WGLDeleteContext(deleteContext)
       , mDC(dc)
       , mGLRC(glrc)
       , mOwnsContext(ownsContext)
   {
   }

   ~WGLContext()
   {
      Publish(ContextDestroyedMessage {});
      
      if (mOwnsContext)
      {
         WGLDeleteContext(mGLRC);
         ReleaseDC(WindowFromDC(mDC), mDC);
      }
   }

   void BeginRendering()
   {
      WGLMakeCurrent(mDC, mGLRC);

      mBound = true;

      ProcessReleaseQueue();
   }

   void EndRendering()
   {
      mBound = false;
      
      SwapBuffers(mDC);
      WGLMakeCurrent(mDC, 0);
   }

   void ProcessReleaseQueue() override
   {
      if (!mBound)
         return;

      DoProcessReleaseQueue();
   }

private:
   pfnWGLMakeCurrent WGLMakeCurrent;
   pfnWGLDeleteContext WGLDeleteContext;

   HDC mDC;
   HGLRC mGLRC;

   bool mOwnsContext;
   bool mBound { false };
};

class WGLRenderer::InvisibleWindow final
{
public:
   InvisibleWindow()
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
         WGLDeleteContext(mGLContext);

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
         WGL_DRAW_TO_WINDOW_ARB,
         GL_TRUE,
         WGL_SUPPORT_OPENGL_ARB,
         GL_TRUE,
         WGL_DOUBLE_BUFFER_ARB,
         GL_TRUE,
         WGL_ACCELERATION_ARB,
         WGL_FULL_ACCELERATION_ARB,
         WGL_PIXEL_TYPE_ARB,
         WGL_TYPE_RGBA_ARB,
         WGL_COLOR_BITS_ARB,
         32,
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
      {
         auto str = GetSysErrorMessage(GetLastError());
         return nullptr;
      }

      const int glAttribs[] = {
         WGL_CONTEXT_MAJOR_VERSION_ARB,
         3,
         WGL_CONTEXT_MINOR_VERSION_ARB,
         0,
         WGL_CONTEXT_PROFILE_MASK_ARB,
         WGL_CONTEXT_CORE_PROFILE_BIT_ARB,
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
         WGLMakeCurrent(mDC, context);
         auto contextSwitcher =
            finally([this]() { WGLMakeCurrent(mDC, nullptr); });


         if (GLDebugMessageCallbackARB != nullptr || GetARBFunction(
                GLDebugMessageCallbackARB, "glDebugMessageCallbackARB"))
            GLDebugMessageCallbackARB(DebugCallback, this);

      }
#endif // !NDEBUG

      return context;
   }

   bool LoadGLFunctions()
   {
      WGLMakeCurrent(mDC, mGLContext);
      auto contextSwitcher =
         finally([this]() { WGLMakeCurrent(mDC, nullptr); });

      mFunctions =
         std::make_unique<WGLFunctions>(mGLLibrary, WGLGetProcAddress);

      return mFunctions->IsOk();
   }

   std::unique_ptr<WGLContext> CreateContext(HDC dc, HGLRC glrc)
   {
      return std::make_unique<WGLContext>(
         *mFunctions, WGLMakeCurrent, WGLDeleteContext, dc,
         glrc != nullptr ? glrc : CreateOpenGL3Context(dc, mGLContext),
         glrc == nullptr);
   }

   std::unique_ptr<WGLContext> CreateContextWrapper()
   {
      return CreateContext(mDC, mGLContext);
   }

private:
   template <typename Fn>
   bool GetFunction(Fn& fn, const char* name)
   {
      fn = reinterpret_cast<Fn>(GetProcAddress(mGLLibrary, name));
      return fn != nullptr;
   }

   template <typename Fn>
   bool GetARBFunction(Fn& fn, const char* name)
   {
      fn = reinterpret_cast<Fn>(WGLGetProcAddress(name));
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

      if (!GetFunction(WGLCreateContext, "wglCreateContext"))
         return false;

      if (!GetFunction(WGLDeleteContext, "wglDeleteContext"))
         return false;

      if (!GetFunction(WGLMakeCurrent, "wglMakeCurrent"))
         return false;

      if (!GetFunction(WGLGetProcAddress, "wglGetProcAddress"))
         return false;

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

      HGLRC tempContext = WGLCreateContext(mDC);

      if (tempContext == nullptr)
         return false;

      auto tempContextDeleter =
         finally([tempContext, this]() { WGLDeleteContext(tempContext); });

      if (!WGLMakeCurrent(mDC, tempContext))
         return false;

      auto contextSwitcher = finally([this]() { WGLMakeCurrent(mDC, nullptr); });

      if (!GetARBFunction(WGLCreateContextAttribsARB, "wglCreateContextAttribsARB"))
         return false;

      if (!GetARBFunction(WGLChoosePixelFormatARB, "wglChoosePixelFormatARB"))
         return false;

      return true;
   }

private:
#ifndef NDEBUG
   static void __stdcall DebugCallback(GLenum source, GLenum type, GLuint id, GLenum severity,
      GLsizei length, const GLchar *message,
      const void *userParam)
   {
   }
#endif // !NDEBUG

   HWND mWindow { nullptr };
   HDC mDC { nullptr };

   HMODULE mGLLibrary { nullptr };

   pfnWGLCreateContext WGLCreateContext { nullptr };
   pfnWGLDeleteContext WGLDeleteContext { nullptr };
   pfnWGLMakeCurrent WGLMakeCurrent { nullptr };
   pfnWGLGetProcAddress WGLGetProcAddress { nullptr };

   pfnWGLCreateContextAttribsARB WGLCreateContextAttribsARB { nullptr };
   pfnWGLChoosePixelFormatARB WGLChoosePixelFormatARB { nullptr };

#ifndef NDEBUG
   pfnGLDebugMessageCallbackARB GLDebugMessageCallbackARB { nullptr };
#endif // !NDEBUG

   HGLRC mGLContext { nullptr };

   std::unique_ptr<WGLFunctions> mFunctions;
};

WGLRenderer::WGLRenderer()
    : mInvisibleWindow(std::make_unique<InvisibleWindow>())
{
   if (!mInvisibleWindow->IsOk())
   {
      mInvisibleWindow = {};
      return;
   }

   mInvisibleWindowContext = mInvisibleWindow->CreateContextWrapper();
   mInvisibleWindowContext->BeginRendering();
}

WGLRenderer::~WGLRenderer()
{
}

bool WGLRenderer::IsSupported() const
{
   return mInvisibleWindow != nullptr;
}

Context& WGLRenderer::GetResourceContext()
{
   return mCurrentContext != nullptr ? *mCurrentContext :
                                       *mInvisibleWindowContext;
}

std::unique_ptr<graphics::gl::Context> WGLRenderer::CreateContext(void* window)
{
   return mInvisibleWindow->CreateContext(
      GetDC(static_cast<HWND>(window)), nullptr);
}

void WGLRenderer::BeginRendering(Context& context)
{
   mCurrentContext = static_cast<WGLContext*>(&context);
   mCurrentContext->BeginRendering();
}

void WGLRenderer::EndRendering()
{
   if (mCurrentContext == nullptr)
      return;

   mCurrentContext->EndRendering();
   mCurrentContext = nullptr;

   mInvisibleWindowContext->BeginRendering();
}

} // namespace graphics::gl::platforms::windows
