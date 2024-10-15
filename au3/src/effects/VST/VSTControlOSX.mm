/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTControlOSX.mm

  Leland Lucius

  Several ideas and code snippets taken from HairerSoft's HSVSTView class:

      http://www.hairersoft.com/Downloads/HSVSTView.zip

      Created by Martin on 02/06/2007.
      Copyright 2010 by HairerSoft.
      
      You are most welcome to use this code in your own (open source or not)
      project. Use at your own risk of course, etc. Please acknowledge at an
      appropriate location (manual or about box for example).
      
      Bug reports most welcome: Martin@HairerSoft.com
      
**********************************************************************/


#include "VSTControlOSX.h"

#include <memory>

@interface VSTView : NSView
{
}
@end

@implementation VSTView
 
+ (void)initialize
{
   static BOOL initialized = NO;
   if (!initialized)
   {
      initialized = YES;
      wxOSXCocoaClassAddWXMethods(self);
   }
}
@end

VSTControlImpl::VSTControlImpl(wxWindowMac *peer, NSView *view)
:  wxWidgetCocoaImpl(peer, view)
{
}

VSTControlImpl::~VSTControlImpl()
{
}

VSTControl::VSTControl()
:  VSTControlBase()
{
   mVSTView = nil;
   mView = nil;

#if !defined(_LP64)
   mHIView = NULL;
   mWindowRef = NULL;
#endif
}

VSTControl::~VSTControl()
{
   Close();
}

void VSTControl::Close()
{
   auto &resource =
#if !defined(_LP64)
       mWindowRef
#else
       mVSTView
#endif
   ;
   if (resource)
   {
      mLink->callDispatcher(effEditClose, 0, 0, resource, 0.0);
      resource = nullptr;
   }
}

bool VSTControl::Create(wxWindow *parent, VSTLink *link)
{
   DontCreatePeer();
   
   if (!VSTControlBase::Create(parent, link))
   {
      return false;
   }

   mVSTView = [VSTView alloc];
   if (!mVSTView)
   {
      return false;
   }
   [mVSTView init];
   [mVSTView retain];

   // wxWidgets takes ownership so safenew
   SetPeer(safenew VSTControlImpl(this, mVSTView));

   CreateCocoa();

#if !defined(_LP64)
   if (!mView)
   {
      CreateCarbon();
   }
#endif

   if (!mView
#if !defined(_LP64)
       && !mHIView
#endif
       )
   {
      return false;
   }

   // Must get the size again since SetPeer() could cause it to change
   SetInitialSize(GetMinSize());

   MacPostControlCreate(wxDefaultPosition, wxDefaultSize);

   return true;
}

void VSTControl::CreateCocoa()
{
   VstRect *rect;

   // Some effects like to have us get their rect before opening them.
   mLink->callDispatcher(effEditGetRect, 0, 0, &rect, 0.0);

   // Ask the effect to add its GUI
   mLink->callDispatcher(effEditOpen, 0, 0, mVSTView, 0.0);

   // Get the subview it created
   mView = [[mVSTView subviews] objectAtIndex:0];
   if (mView == NULL)
   {
      // Doesn't seem the effect created the subview.  This can
      // happen when an effect uses the content view directly.
      // As of this time, we will not try to support those and
      // just fall back to the textual interface.
      return;
   }

   // Get the final bounds of the effect GUI
   mLink->callDispatcher(effEditGetRect, 0, 0, &rect, 0.0);

   NSRect frame = {
      { 0, 0 },
      { (CGFloat) rect->right - rect->left, (CGFloat) rect->bottom - rect->top }
   };

   [mView setFrame:frame];

   [mVSTView addSubview:mView];

   SetMinSize(wxSize(frame.size.width, frame.size.height));

   return;
}

#if !defined(_LP64)

void VSTControl::CreateCarbon()
{
   OSStatus result;

   Bind(wxEVT_SIZE, &VSTControl::OnSize, this);

   VstRect *rect;

   // Some effects like to have us get their rect before opening them.
   mLink->callDispatcher(effEditGetRect, 0, 0, &rect, 0.0);

   // Suggest a dummy size
   Rect bounds = { 0, 0, 0, 0 };

   // And create the window
   result = CreateNewWindow(kOverlayWindowClass,
                            kWindowStandardHandlerAttribute |
                            kWindowCompositingAttribute |
                            kWindowOpaqueForEventsAttribute,
                            &bounds,
                            &mWindowRef);
   if (result != noErr)
   {
      return;
   }

   // Get the root control
   ControlRef root = HIViewGetRoot(mWindowRef);

   // Find the content view within our window
   HIViewRef content;
   result = HIViewFindByID(root, kHIViewWindowContentID, &content);
   if (result != noErr)
   {
      DisposeWindow(mWindowRef);
      mWindowRef = NULL;

      return;
   }

   // Some effects (iZotope Vinyl) seem to need an existing subview
   // of the content view.  So just use a "dummy" scrollview.
   result = HIScrollViewCreate(kHIScrollViewOptionsVertScroll, &mHIView);

   // Don't want to see the scroll bars
   HIScrollViewSetScrollBarAutoHide(mHIView, true);

   // Add it as a subview of the content view
   HIViewAddSubview(content, mHIView);

   // Ask the effect to add its GUI
   mLink->callDispatcher(effEditOpen, 0, 0, mWindowRef, 0.0);

   // Get the subview it created
   HIViewRef subview = HIViewGetFirstSubview(content);
   if (subview)
   {
      // The scrollview was used, so leave it.
      if (subview == mHIView)
      {
         subview = HIViewGetFirstSubview(mHIView);
      }
      // The effect didn't use our scrollview, so dispose of it.
      else
      {
         HIViewRemoveFromSuperview(mHIView);
         CFRelease(mHIView);
         mHIView = subview;
      }
   }

   // Doesn't seem the effect created a subview.  This can
   // happen when an effect uses the content view directly.
   // As of this time, we will not try to support those and
   // just fall back to the textual interface.
   if (subview == NULL)
   {
      mLink->callDispatcher(effEditClose, 0, 0, mWindowRef, 0.0);
      DisposeWindow(mWindowRef);
      mWindowRef = NULL;
      mHIView = NULL;

      return;
   }

   // Get the final bounds of the effect GUI
   mLink->callDispatcher(effEditGetRect, 0, 0, &rect, 0.0);

   // Set the size of the scrollview to match
   HIRect r = {
      { 0, 0 },
      { (CGFloat) rect->right - rect->left, (CGFloat) rect->bottom - rect->top }
   };

   // One effect, mutagene lipredemuco, doesn't return a valid rect so
   // try to detect it and use the created view dimensions instead.
   if (rect->left < 0 || rect->top < 0 || rect->right <= 0 || rect->bottom <= 0)
   {
      HIViewGetFrame(subview, &r);
   }

   // Make sure container is the same size as the effect GUI
   HIViewSetFrame(mHIView, &r);
   HIViewPlaceInSuperviewAt(mHIView, 0, 0);

   // Establish the minimum size
   SetMinSize(wxSize(r.size.width, r.size.height));

   NSWindow *parent = [mVSTView window];
   NSWindow *host = [[[NSWindow alloc] initWithWindowRef:mWindowRef] autorelease];
   [parent addChildWindow:host ordered:NSWindowAbove];

   ShowWindow(mWindowRef);
}

void VSTControl::OnSize(wxSizeEvent & evt)
{
   evt.Skip();

   wxRect rect = GetScreenRect();

   MoveWindow(mWindowRef, rect.x, rect.y, true);
   SizeWindow(mWindowRef, rect.width, rect.height, true);
}

#endif
