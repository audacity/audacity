/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  ViewSurface.mm

  Dmitry Vedenko

**********************************************************************/
#include "ViewSurface.h"

#import <Cocoa/Cocoa.h>

#import <IOSurface/IOSurfaceObjC.h>
#import <QuartzCore/QuartzCore.h>

#include "graphics/gl/GLFunctions.h"

@interface CALayer (Private)
- (void)setContentsChanged;
@end

@interface IOSurfaceBackedView : NSView {
   int displayWidth;
   int displayHeight;

    IOSurfaceRef ioSurface;
    GLuint texture;
    GLuint fbo;

    const graphics::gl::GLFunctions* gl;
}

- (void) bindFramebuffer;
@end

@implementation IOSurfaceBackedView

- (void) bindFramebuffer
{
   if (ioSurface == nullptr)
        [self createSurface];

   gl->BindFramebuffer(graphics::gl::GLenum::FRAMEBUFFER, fbo);
}

- (void) initView:(const graphics::gl::GLFunctions*) _gl
{
   self.wantsLayer = true;

   auto layer = [CALayer layer];
   layer.contentsGravity = kCAGravityTopLeft;

   self.layer = layer;

   displayWidth = 0;
   displayHeight = 0;

   ioSurface = nullptr;
   texture = 0;
   fbo = 0;

   gl = _gl;
}

- (instancetype)initWithFrame:(NSRect)frameRect glFunction: (const graphics::gl::GLFunctions*) _gl
{
    self = [super initWithFrame:frameRect];
    [self initView: _gl];
    return self;
}

- (BOOL)isOpaque
{
    return YES;
}

- (BOOL)isFlipped
{
    return YES;
}

- (void)viewDidMoveToWindow
{
    if ([self window] == nullptr)
        return;

    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(handleSizeChange:)
                                                 name:NSViewFrameDidChangeNotification
                                               object:self];
    [self handleSizeChange];
}

- (void)viewWillMoveToWindow:(NSWindow *)newWindow
{
    if (newWindow != nullptr)
       return;

    [[NSNotificationCenter defaultCenter] removeObserver:self
                                                    name:NSViewFrameDidChangeNotification
                                                  object:self];
}

- (void)handleSizeChange:(NSNotification*)notification
{
    [self handleSizeChange];
}

- (void)handleSizeChange
{
    auto layerBounds = self.bounds;
    NSSize backingSize = [self convertSizeToBacking:layerBounds.size];
    displayWidth = static_cast<int>(backingSize.width);
    displayHeight = static_cast<int>(backingSize.height);
    self.layer.contentsScale = self.window.backingScaleFactor;

    [self destroySurface];
}

-(void) createSurface
{
   NSDictionary* dict = @{
      IOSurfacePropertyKeyWidth: [NSNumber numberWithInt:displayWidth],
      IOSurfacePropertyKeyHeight: [NSNumber numberWithInt:displayHeight],
      IOSurfacePropertyKeyBytesPerElement: [NSNumber numberWithInt:4],
      IOSurfacePropertyKeyPixelFormat: [NSNumber numberWithInt:'BGRA'],
      (NSString*)kIOSurfaceIsGlobal: [NSNumber numberWithBool:YES]
   };

   ioSurface = IOSurfaceCreate((CFDictionaryRef)dict);

   if (ioSurface == nullptr)
      return;

   gl->GenTextures(1, &texture);

   gl->BindTexture(graphics::gl::GLenum::TEXTURE_RECTANGLE, texture);

#if 1
   auto err = CGLTexImageIOSurface2D(
      CGLGetCurrentContext(),
      uint32_t(graphics::gl::GLenum::TEXTURE_RECTANGLE),
      uint32_t(graphics::gl::GLenum::RGBA),
      displayWidth,
      displayHeight,
      uint32_t(graphics::gl::GLenum::BGRA),
      uint32_t(graphics::gl::GLenum::UNSIGNED_INT_8_8_8_8_REV),
      ioSurface,
      0);

   if (err != kCGLNoError)
   {
      [self destroySurface];
      return;
   }
#else
   gl->TexImage2D(graphics::gl::GLenum::TEXTURE_RECTANGLE, 0, graphics::gl::GLenum::RGBA, displayWidth, displayHeight, 0, graphics::gl::GLenum::BGRA, graphics::gl::GLenum::UNSIGNED_INT_8_8_8_8_REV, nullptr);
#endif
   gl->BindTexture(graphics::gl::GLenum::TEXTURE_RECTANGLE, 0);

   gl->GenFramebuffers(1, &fbo);
   gl->BindFramebuffer(graphics::gl::GLenum::FRAMEBUFFER, fbo);
   gl->FramebufferTexture2D(
      graphics::gl::GLenum::FRAMEBUFFER,
      graphics::gl::GLenum::COLOR_ATTACHMENT0,
      graphics::gl::GLenum::TEXTURE_RECTANGLE,
      texture,
      0);

   const auto status = gl->CheckFramebufferStatus(graphics::gl::GLenum::FRAMEBUFFER);

   gl->BindFramebuffer(graphics::gl::GLenum::FRAMEBUFFER, 0);

   if (status != graphics::gl::GLenum::FRAMEBUFFER_COMPLETE)
   {
      [self destroySurface];
      return;
   }

   self.layer.contents = (id)ioSurface;
}

-(void) destroySurface
{
   if (fbo != 0)
    {
       gl->DeleteFramebuffers(1, &fbo);
       fbo = 0;
    }

    if (texture != 0)
    {
       gl->DeleteTextures(1, &texture);
       texture = 0;
    }

    if (ioSurface != nullptr)
    {
       CFRelease(ioSurface);
       ioSurface = nullptr;
    }
}

-(void)dealloc
{
   [self destroySurface];
   [super dealloc];
}

@end

namespace graphics::gl::platforms::macocs
{

class ViewSurface::Impl final
{
public:
   Impl(const GLFunctions& functions, NSView* view)
      : mFunctions(functions)
      , mView(view)
   {
      [mView retain];

      mIOSurfaceView = [[IOSurfaceBackedView alloc] initWithFrame: mView.bounds glFunction: &mFunctions];
      mIOSurfaceView.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;

      [mView addSubview: mIOSurfaceView];
   }

   ~Impl()
   {
      [mView release];
      [mIOSurfaceView release];
   }

   uint32_t GetWidth() const noexcept
   {
      return mView.frame.size.width;
   }

   uint32_t GetHeight() const noexcept
   {
      return mView.frame.size.height;
   }

   float GetScaleFactor() const noexcept
   {
      if (mView.window == nullptr)
         return 1.0f;

      return mView.window.backingScaleFactor;
   }

   void BeginRendering()
   {
      [CATransaction begin];
      BindFramebuffer();
   }

   void BindFramebuffer()
   {
      [mIOSurfaceView bindFramebuffer];
   }

   void EndRendering()
   {
      mFunctions.Flush();

      [mIOSurfaceView.layer setContentsChanged];
      [CATransaction commit];
      [CATransaction flush];
   }

private:
   const GLFunctions& mFunctions;
   NSView* mView;

   IOSurfaceBackedView* mIOSurfaceView;
};

ViewSurface::ViewSurface(const GLFunctions& functions, void* view)
   : mImpl(
      std::make_unique<Impl>(functions, reinterpret_cast<NSView*>(view)))
{
}

ViewSurface::~ViewSurface()
{

}

uint32_t ViewSurface::GetWidth() const noexcept
{
   return mImpl->GetWidth();
}

uint32_t ViewSurface::GetHeight() const noexcept
{
   return mImpl->GetHeight();
}

void ViewSurface::BeginRendering()
{
   mImpl->BeginRendering();
}

void ViewSurface::BindFramebuffer()
{
   mImpl->BindFramebuffer();
}

void ViewSurface::EndRendering()
{
   mImpl->EndRendering();
}
float ViewSurface::GetScaleFactor() const noexcept
{
   return mImpl->GetScaleFactor();
}

} // namespace graphics::gl::platforms::maocs
