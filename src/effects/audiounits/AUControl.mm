/**********************************************************************

  Audacity: A Digital Audio Editor

  AUControl.mm

  Leland Lucius

  Several ideas and code snippets taken from HairerSoft's HSAUView class:

      http://www.hairersoft.com/Downloads/HSAUView.zip

      Created by Martin on 02/06/2007.
      Copyright 2010 by HairerSoft.
      
      You are most welcome to use this code in your own (open source or not)
      project. Use at your own risk of course, etc. Please acknowledge at an
      appropriate location (manual or about box for example).
      
      Bug reports most welcome: Martin@HairerSoft.com
      
**********************************************************************/

#include <AudioUnit/AudioUnit.h>
#include <AudioUnit/AudioComponent.h>
#include <AudioUnit/AudioUnitProperties.h>
#include <AudioUnit/AUCocoaUIView.h>
#include <CoreAudioKit/CoreAudioKit.h>

#if !defined(_LP64)
#include <AudioUnit/AudioUnitCarbonView.h>
#endif

#include "AUControl.h"

@interface AUView : NSView
{
   AUControl *mControl;
}
@end

@implementation AUView
 
+ (void)initialize
{
   static BOOL initialized = NO;
   if (!initialized)
   {
      initialized = YES;
      wxOSXCocoaClassAddWXMethods(self);
   }
}

- (instancetype)initWithControl:(AUControl *)control
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

AUControlImpl::AUControlImpl(wxWindowMac *peer, NSView *view)
:  wxWidgetCocoaImpl(peer, view, false, false)
{
}

AUControlImpl::~AUControlImpl()
{
}

BEGIN_EVENT_TABLE(AUControl, wxControl)
   EVT_SIZE(AUControl::OnSize)
END_EVENT_TABLE()

AUControl::AUControl()
{
   mComponent = NULL;
   mUnit = NULL;

   mAUView = nil;
   mView = nil;

   mSettingSize = false;

#if !defined(_LP64)
   mHIView = NULL;
   mWindowRef = NULL;
#endif
}

AUControl::~AUControl()
{
#if !defined(_LP64)

   if (mInstance)
   {
      AudioComponentInstanceDispose(mInstance);
   }

#endif

   if (mView)
   {
      NSNotificationCenter *center = [NSNotificationCenter defaultCenter];
      [center removeObserver:mAUView
                        name:NSViewFrameDidChangeNotification
                      object:mView];

      [mView release];
   }

   if (mAUView)
   {
      [mAUView release];
   }
}

bool AUControl::Create(wxWindow *parent, AudioComponent comp, AudioUnit unit, bool custom)
{
   mComponent = comp;
   mUnit = unit;

   DontCreatePeer();

   if (!wxControl::Create(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, wxEmptyString))
   {
      return false;
   }

   mAUView = [AUView alloc];
   if (!mAUView)
   {
      return false;
   }
   [(AUView *)mAUView initWithControl:this];
   [mAUView retain];

   if (custom)
   {
      CreateCocoa();

#if !defined(_LP64)
      if (!mView)
      {
         CreateCarbon();
      }
#endif
   }

   if (!mView && !mHIView)
   {
      CreateGeneric();
   }

   if (!mView && !mHIView)
   {
      return false;
   }

   SetPeer(new AUControlImpl(this, mAUView));

   if (mHIView)
   {
      CreateCarbonOverlay();
   }

   // Must get the size again since SetPeer() could cause it to change
   SetInitialSize(GetMinSize());

   MacPostControlCreate(wxDefaultPosition, wxDefaultSize);

   return true;
}

void AUControl::OnSize(wxSizeEvent & evt)
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

      NSRect viewFrame = [mAUView frame];
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

void AUControl::CreateCocoa()
{
   OSStatus result;
   UInt32 dataSize;

   result = AudioUnitGetPropertyInfo(mUnit,
                                     kAudioUnitProperty_CocoaUI,
                                     kAudioUnitScope_Global, 
                                     0,
                                     &dataSize,
                                     NULL);
   if (result != noErr)
   {
      return;
   }

   int cnt = (dataSize - sizeof(CFURLRef)) / sizeof(CFStringRef);
   if (cnt == 0)
   {
      return;
   }

   AudioUnitCocoaViewInfo *viewInfo = (AudioUnitCocoaViewInfo *) malloc(dataSize);
   if (viewInfo == NULL)
   {
      return;
   }

   // Get the view info
   result = AudioUnitGetProperty(mUnit,
                                 kAudioUnitProperty_CocoaUI,
                                 kAudioUnitScope_Global,
                                 0,
                                 viewInfo,
                                 &dataSize);
   if (result == noErr)
   {
      // Looks like the AU has a Cocoa UI, so load the factory class
      NSURL *bundleLoc = (NSURL *) viewInfo->mCocoaAUViewBundleLocation;
      NSString *viewClass = (NSString *) viewInfo->mCocoaAUViewClass[0];

      if (bundleLoc != nil && viewClass != nil)
      {
         // Load the bundle
         NSBundle *bundle = [NSBundle bundleWithPath:[bundleLoc path]];
         if (bundle != nil)
         {
            // Load the class from the bundle
            Class factoryClass = [bundle classNamed:viewClass];
            if (factoryClass != nil)
            {      
               // Create an instance of the class
               id factoryInst = [[[factoryClass alloc] init] autorelease];
               if (factoryInst != nil)
               {
                  // Suggest a reasonable size
                  NSSize size = {800, 600};
      
                  // Create the view
                  mView = [factoryInst uiViewForAudioUnit:mUnit withSize:size];
               }
            }
         }
      }

      if (viewInfo->mCocoaAUViewBundleLocation != nil)
      {
         CFRelease(viewInfo->mCocoaAUViewBundleLocation);
      }

      for (int i = 0; i < cnt; i++)
      {
         CFRelease(viewInfo->mCocoaAUViewClass[i]);
      }
   }

   free(viewInfo);

   if (!mView)
   {
      return;
   }

   [mView retain];

   NSNotificationCenter *center = [NSNotificationCenter defaultCenter];
   [center addObserver:mAUView
              selector:@selector(cocoaViewResized:)
                  name:NSViewFrameDidChangeNotification
                object:mView];

   [mAUView addSubview:mView];

   NSSize viewSize = [mView frame].size;

   mLastMin = wxSize(viewSize.width, viewSize.height);

   SetMinSize(mLastMin);

   [mAUView setAutoresizingMask:[mView autoresizingMask]];

   return;
}

void AUControl::CreateGeneric()
{
   OSStatus result;
   AudioComponentDescription desc;

   result = AudioComponentGetDescription(mComponent, &desc);
   if (result == noErr && desc.componentType == kAudioUnitType_Panner)
   {
      mView = [AUPannerView AUPannerViewWithAudioUnit:mUnit];
      if (mView == nil)
      {
         return;
      }
   }
   else
   {
      // Create a generic AU view
      AUGenericView *view = [AUGenericView alloc];
      if (view == nil)
      {
         return;
      }

      int flags = AUViewPropertiesDisplayFlag |
                  AUViewParametersDisplayFlag;
   
      [view initWithAudioUnit:mUnit displayFlags:flags];
   
      [view setShowsExpertParameters:YES];  

      mView = view;
   }

   [mView retain];

   NSNotificationCenter *center = [NSNotificationCenter defaultCenter];
   [center addObserver:mAUView
              selector:@selector(cocoaViewResized:)
                  name:NSViewFrameDidChangeNotification
                object:mView];

   [mAUView addSubview:mView];

   NSSize viewSize = [mView frame].size;

   mLastMin = wxSize(viewSize.width, viewSize.height);

   SetMinSize(mLastMin);

   [mAUView setAutoresizingMask:[mView autoresizingMask]];

   return;
}

void AUControl::CocoaViewResized()
{
   if (mSettingSize)
   {
      return;
   }

   NSSize viewSize = [mView frame].size;
   NSSize frameSize = [mAUView frame].size;

   [mAUView setFrameSize:viewSize];

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

void AUControl::CreateCarbon()
{
   OSStatus result;
   UInt32 dataSize;

   result = AudioUnitGetPropertyInfo(mUnit,
                                     kAudioUnitProperty_GetUIComponentList,
                                     kAudioUnitScope_Global, 
                                     0,
                                     &dataSize,
                                     NULL);
   if (result != noErr)
   {
      return;
   }

   int cnt = (dataSize - sizeof(CFURLRef)) / sizeof(CFStringRef);
   if (cnt == 0)
   {
      return;
   }

   AudioComponentDescription *compList = (AudioComponentDescription *) malloc(dataSize);
   if (compList == NULL)
   {
      return;
   }

   // Get the view info
   result = AudioUnitGetProperty(mUnit,
                                 kAudioUnitProperty_GetUIComponentList,
                                 kAudioUnitScope_Global,
                                 0,
                                 compList,
                                 &dataSize);
   if (result != noErr)
   {
      free(compList);

      return;
   }

   // Get the component
   AudioComponent comp = AudioComponentFindNext(NULL, &compList[0]);

   // Try to create an instance
   result = AudioComponentInstanceNew(comp, &mInstance);

   // Done with the list
   free(compList);

   if (result != noErr)
   {
      return;
   }

   Rect bounds = { 100, 100, 200, 200 };

   result = CreateNewWindow(kOverlayWindowClass,
                            kWindowStandardHandlerAttribute |
                            kWindowCompositingAttribute |
                            kWindowOpaqueForEventsAttribute,
                            &bounds,
                            &mWindowRef);
   if (result != noErr)
   {
      AudioComponentInstanceDispose(mInstance);
      mInstance = NULL;

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

      AudioComponentInstanceDispose(mInstance);
      mInstance = NULL;

      return;
   }

   SetWindowActivationScope(mWindowRef, kWindowActivationScopeIndependent);

   // Suggest a reasonable size
   Float32Point loc = {0.0, 0.0};
   Float32Point size = {800.0, 600.0};

   // And create it
   result = AudioUnitCarbonViewCreate(mInstance,
                                      mUnit,
                                      mWindowRef,
                                      root,
                                      &loc,
                                      &size,
                                      &mHIView);
   if (result != noErr)
   {
      DisposeWindow(mWindowRef);
      mWindowRef = NULL;

      AudioComponentInstanceDispose(mInstance);
      mInstance = NULL;

      return;
   }

   HIViewAddSubview(root, mHIView);
   HIViewPlaceInSuperviewAt(mHIView, 0, 0);
   HIViewSetVisible(mHIView, TRUE);

   HIRect rect;
   HIViewGetFrame(mHIView, &rect);
   SetMinSize(wxSize(rect.size.width, rect.size.height));

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

void AUControl::CreateCarbonOverlay()
{
   NSWindow *parent = [mAUView window];
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
AUControl::ControlEventHandlerCallback(EventHandlerCallRef handler, EventRef event, void *data)
{
   ((AUControl *) data)->CarbonViewResized();

   return eventNotHandledErr;
}

void AUControl::CarbonViewResized()
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

   [mAUView setFrameSize:NSMakeSize(rect.size.width, rect.size.height)];

   NSSize frameSize = [mAUView frame].size;

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
