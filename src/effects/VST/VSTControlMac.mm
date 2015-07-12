/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTControlMac.mm

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

#include "VSTControl.h"

@interface VSTView : NSView
{
   VSTControl *mControl;
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

- (instancetype)initWithControl:(VSTControl *)control
{
   // Make sure a parameters were provided
   NSParameterAssert(control);

   mControl = control;

   [super init];

   return self;
}

- (BOOL)autoresizesSubviews
{
   return NO;
}

- (void)cocoaViewResized:(NSNotification *)notification
{
   mControl->CocoaViewResized();
}

@end

VSTControlImpl::VSTControlImpl(wxWindowMac *peer, NSView *view)
:  wxWidgetCocoaImpl(peer, view, false, false)
{
}

VSTControlImpl::~VSTControlImpl()
{
}

BEGIN_EVENT_TABLE(VSTControl, wxControl)
   EVT_SIZE(VSTControl::OnSize)
END_EVENT_TABLE()

VSTControl::VSTControl()
{
   mLink = NULL;

   mVSTView = nil;
   mView = nil;

   mSettingSize = false;

#if !defined(_LP64)
   mHIView = NULL;
   mWindowRef = NULL;

#endif
}

VSTControl::~VSTControl()
{
   if (mWindowRef)
   {
      mLink->callDispatcher(effEditClose, 0, 0, mWindowRef, 0.0);
      mWindowRef = 0;
   }

}

bool VSTControl::Create(wxWindow *parent, VSTEffectLink *link)
{
   if (!VSTControlBase::Create(parent, link))
   {
      return false;
   }

   mVSTView = [VSTView alloc];
   if (!mVSTView)
   {
      return false;
   }
   [(VSTView *)mVSTView initWithControl:this];
   [mVSTView retain];

   CreateCocoa();

#if !defined(_LP64)
   if (!mView)
   {
      CreateCarbon();
   }
#endif
   if (!mView && !mHIView)
   {
      return false;
   }

   SetPeer(new VSTControlImpl(this, mVSTView));

   if (mHIView)
   {
      CreateCarbonOverlay();
   }

   // Must get the size again since SetPeer() could cause it to change
   SetInitialSize(GetMinSize());

   MacPostControlCreate(wxDefaultPosition, wxDefaultSize);

   return true;
}

void VSTControl::OnSize(wxSizeEvent & evt)
{
   evt.Skip();

   if (mSettingSize)
   {
      return;
   }
   mSettingSize = true;

   wxSize sz = GetSize();

   if (mView)
   {
      int mask = [mView autoresizingMask];

      NSRect viewFrame = [mVSTView frame];
      NSRect viewRect = [mView frame];
   
      if (mask & NSViewWidthSizable)
      {
         viewRect.size.width = sz.GetWidth();
      }

      if (mask & NSViewHeightSizable)
      {
         viewRect.size.height = sz.GetHeight();
      }

      viewRect.origin.x = (viewFrame.size.width - viewRect.size.width) / 2;
      viewRect.origin.y = (viewFrame.size.height - viewRect.size.height) / 2;

      [mView setFrame:viewRect];
   }

#if !defined(_LP64)
   else if (mHIView)
   {
      HIRect rect;
      HIViewGetFrame(mHIView, &rect);

      CGFloat x = (sz.x - rect.size.width) / 2;
      CGFloat y = (sz.y - rect.size.height) / 2;
      
      SizeWindow(mWindowRef, sz.x, sz.y, true);
      HIViewPlaceInSuperviewAt(mHIView, x, y);

      wxWindow *w = wxGetTopLevelParent(this);

      wxSize min = w->GetMinSize();
      min.x += (rect.size.width - mLastMin.GetWidth());
      min.y += (rect.size.height - mLastMin.GetHeight());

      w->SetSizeHints(min, min);

      mLastMin = wxSize(rect.size.width, rect.size.height);
   }
#endif

   mSettingSize = false;

   return;
}

void VSTControl::CreateCocoa()
{
   bool mIsCocoa = (mLink->callDispatcher(effCanDo, 0, 0, (void *) "hasCockosViewAsConfig", 0.0) & 0xffff0000) == 0xbeef0000;
   if (!mIsCocoa)
   {
      return;
   }

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

   NSNotificationCenter *center = [NSNotificationCenter defaultCenter];
   [center addObserver:mVSTView
              selector:@selector(cocoaViewResized:)
                  name:NSViewFrameDidChangeNotification
                object:mView];

   [mVSTView addSubview:mView];

   NSSize viewSize = [mView frame].size;

   mLastMin = wxSize(viewSize.width, viewSize.height);

   SetMinSize(mLastMin);

   [mVSTView setAutoresizingMask:[mView autoresizingMask]];

   return;
}

void VSTControl::CocoaViewResized()
{
   if (mSettingSize)
   {
      return;
   }

   NSSize viewSize = [mView frame].size;
   NSSize frameSize = [mVSTView frame].size;

   [mVSTView setFrameSize:viewSize];

   int diffW = (viewSize.width - frameSize.width);
   int diffH = (viewSize.height - frameSize.height);

   wxWindow *w = wxGetTopLevelParent(this);

   wxSize min = w->GetMinSize();
   if ([mView autoresizingMask] == NSViewNotSizable)
   {
      min.x += (viewSize.width - mLastMin.GetWidth());
      min.y += (viewSize.height - mLastMin.GetHeight());
      mLastMin = wxSize(viewSize.width, viewSize.height);;
   }
   else
   {
      min.x += diffW;
      min.y += diffH;
   }
   w->SetMinSize(min);

   wxSize size = w->GetSize();
   size.x += diffW;
   size.y += diffH;
   w->SetSize(size);
}

#if !defined(_LP64)

void VSTControl::CreateCarbon()
{
   OSStatus result;

   struct
   {
      short top, left, bottom, right;
   } *rect;

   // Some effects like to have us get their rect before opening them.
   mLink->callDispatcher(effEditGetRect, 0, 0, &rect, 0.0);

   // Suggest a dummy size
   Rect bounds = { 100, 100, 200, 200 };

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

      return nil;
   }

   SetWindowActivationScope(mWindowRef, kWindowActivationScopeIndependent);

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
      // The scrollview was used, so it leave it.
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
      return;
   }

   // Get the final bounds of the effect GUI
   mLink->callDispatcher(effEditGetRect, 0, 0, &rect, 0.0);

   // Set the size of the scrollview to match
   HIRect r = { 0, 0, rect->right - rect->left, rect->bottom - rect->top };
   HIViewSetFrame(mHIView, &r);

   // Make sure it can be seen
   HIViewSetVisible(mHIView, TRUE);

   SetMinSize(wxSize(r.size.width, r.size.height));

   mLastMin = GetMinSize();

   EventTypeSpec controlEventList[] =
   {
      {kEventClassControl, kEventControlBoundsChanged},
   };

   InstallControlEventHandler(mHIView,
                              ControlEventHandlerCallback,
                              GetEventTypeCount(controlEventList),
                              controlEventList,
                              (void *) this,
                              NULL);

   return;
}

void VSTControl::CreateCarbonOverlay()
{
   NSWindow *parent = [mVSTView window];
   WindowRef parentRef = (WindowRef)[parent windowRef];

   NSWindow *host = [[[NSWindow alloc] initWithWindowRef:mWindowRef] autorelease];
   [parent addChildWindow:host ordered:NSWindowAbove];

   WindowGroupRef group;

   CreateWindowGroup(0, &group);
   SetWindowGroupParent(group, GetWindowGroup(parentRef));
   ChangeWindowGroupAttributes(group,
                               kWindowGroupAttrLayerTogether |
                               kWindowGroupAttrSharedActivation |
                               kWindowGroupAttrHideOnCollapse,
                               0);
   SetWindowGroup(parentRef, group);
   SetWindowGroup(mWindowRef, group);

   Rect location;
   GetWindowBounds(parentRef, kWindowContentRgn, &location);
   MoveWindow(mWindowRef, location.left, location.top, true);
   ShowWindow(mWindowRef);
}

pascal OSStatus
VSTControl::ControlEventHandlerCallback(EventHandlerCallRef handler, EventRef event, void *data)
{
   ((VSTControl *) data)->CarbonViewResized();

   return eventNotHandledErr;
}

void VSTControl::CarbonViewResized()
{
   if (mSettingSize)
   {
      return;
   }
   
   // resize and move window
   HIRect rect;
   HIViewGetFrame(mHIView, &rect);

   HIViewPlaceInSuperviewAt(mHIView, 0, 0);
   SizeWindow(mWindowRef, rect.size.width, rect.size.height, true);

   NSSize frameSize = [mVSTView frame].size;

   [mVSTView setFrameSize:NSMakeSize(rect.size.width, rect.size.height)];

   wxWindow *w = wxGetTopLevelParent(this);
   wxSize size = w->GetSize();
   size.x += (rect.size.width - frameSize.width);
   size.y += (rect.size.height - frameSize.height);

   // Reset the current max/min
   w->SetSizeHints(wxDefaultSize, wxDefaultSize);

   // Set the dialog size
   w->SetSize(size);

   // And finally set the new max/min
   w->SetSizeHints(size, size);

   mLastMin = wxSize(rect.size.width, rect.size.height);
}

#endif
