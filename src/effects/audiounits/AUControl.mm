/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AUControl.mm

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


#include "AUControl.h"
#include <AudioUnit/AudioUnitProperties.h>
#include <AudioUnit/AUCocoaUIView.h>
#include <CoreAudioKit/CoreAudioKit.h>

#if !defined(_LP64)
#include <AudioUnit/AudioUnitCarbonView.h>
#endif

#include "MemoryX.h"
#include    "AudioUnitUtils.h"

class AUControlImpl final : public wxWidgetCocoaImpl
{
public :
   AUControlImpl(wxWindowMac *peer, NSView *view);
   ~AUControlImpl();
};

@interface AUView : NSView
{
@public
   AUControl *mControl;
   NSView *mView;
   bool mRedraw;
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

- (instancetype)initWithControl:(AUControl *)control;
{
   // Make sure a parameters were provided
   NSParameterAssert(control);

   mControl = control;
   mRedraw = NO;

   [super init];

   return self;
}

- (BOOL)autoresizesSubviews
{
   return NO;
}

- (void)viewWillDraw
{
   // LLL:  Hackage alert!  I have absolutely no idea why the AudioUnitView doesn't
   //       get redrawn after the ClassInfo is updated, but this bit of malarkey
   //       gets around this issue.
   //
   //       To see the problem, comment out the setFrameSize calls below, create/save
   //       a preset for the AUDelay effect with "Invert Feedback" checked, restore
   //       factory defaults and then reselect the save preset.  The display will not
   //       update properly.  But, resize the window and it does.
   //
   //       Again, this is total hackage and I hope to find the real cause soon.
   if (mRedraw) {
      NSRect viewRect = [mView frame];
      NSRect bogusRect = viewRect;
      ++bogusRect.size.width;
      [mView setFrameSize:bogusRect.size];
      [mView setFrameSize:viewRect.size];
      mRedraw = NO;
   }

   [super viewWillDraw];
}

- (void)cocoaViewResized:(NSNotification *)notification
{
   mControl->CocoaViewResized();
}

@end

AUControlImpl::AUControlImpl(wxWindowMac *peer, NSView *view)
:  wxWidgetCocoaImpl(peer, view)
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
}

AUControl::~AUControl()
{
   Close();
}

void AUControl::Close()
{
#if !defined(_LP64)

   if (mInstance) {
      AudioComponentInstanceDispose(mInstance);
      mInstance = nullptr;
   }

#endif

   if (mView) {
      NSNotificationCenter *center = [NSNotificationCenter defaultCenter];
      [center removeObserver:mAUView
                        name:NSViewFrameDidChangeNotification
                      object:mView];

      [mView release];
      mView = nullptr;
   }

   if (mAUView) {
      [mAUView release];
      mAUView = nullptr;
   }
}

bool AUControl::Create(wxWindow *parent, AudioComponent comp, AudioUnit unit, bool custom)
{
   mComponent = comp;
   mUnit = unit;

   DontCreatePeer();

   if (!wxControl::Create(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, 0, wxDefaultValidator, wxEmptyString))
      return false;

   mAUView = [AUView alloc];
   if (!mAUView)
      return false;
   [static_cast<AUView *>(mAUView) initWithControl:this];
   [mAUView retain];

   if (custom) {
      CreateCocoa();

#if !defined(_LP64)
      if (!mView)
         CreateCarbon();
#endif
   }

   if (!mView
#if !defined(_LP64)
       && !mHIView
#endif
       )
      CreateGeneric();

   if (!mView
#if !defined(_LP64)
       && !mHIView
#endif
       )
      return false;

   static_cast<AUView *>(mAUView)->mView = mView;

   // wxWidgets takes ownership so safenew
   SetPeer(safenew AUControlImpl(this, mAUView));

#if !defined(_LP64)
   if (mHIView)
      CreateCarbonOverlay();
#endif

   // Must get the size again since SetPeer() could cause it to change
   SetInitialSize(GetMinSize());
   MacPostControlCreate(wxDefaultPosition, wxDefaultSize);
   return true;
}

void AUControl::OnSize(wxSizeEvent & evt)
{
   evt.Skip();

   if (mSettingSize)
      return;
   auto vr = valueRestorer( mSettingSize, true );

   wxSize sz = GetSize();

   if (mView) {
      int mask = [mView autoresizingMask];

      NSRect viewFrame = [mAUView frame];
      NSRect viewRect = [mView frame];

      if (mask & (NSViewWidthSizable | NSViewHeightSizable)) {
         if (mask & NSViewWidthSizable)
            viewRect.size.width = sz.GetWidth();

         if (mask & NSViewHeightSizable)
            viewRect.size.height = sz.GetHeight();

         viewRect.origin.x = 0;
         viewRect.origin.y = 0;
         [mView setFrame:viewRect];
      }
      else {
         viewRect.origin.x = abs((viewFrame.size.width - viewRect.size.width) / 2);
         viewRect.origin.y = abs((viewFrame.size.height - viewRect.size.height) / 2);
         if (viewRect.origin.x || viewRect.origin.y)
            [mAUView setFrame:viewRect];
      }
   }

#if !defined(_LP64)
   else if (mHIView) {
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
   return;
}

void AUControl::CreateCocoa()
{
   PackedArray::Ptr<AudioUnitCocoaViewInfo> viewInfo;
   if (!AudioUnitUtils::GetVariableSizeProperty(mUnit,
      kAudioUnitProperty_CocoaUI, viewInfo)) {
      // Looks like the AU has a Cocoa UI, so load the factory class
      auto bundleLoc =
         static_cast<NSURL *>(viewInfo->mCocoaAUViewBundleLocation);
      auto viewClass = static_cast<NSString *>(viewInfo->mCocoaAUViewClass[0]);
      if (bundleLoc && viewClass)
         // Load the bundle
         if (auto bundle = [NSBundle bundleWithPath:[bundleLoc path]])
            // Load the class from the bundle
            if (auto factoryClass = [bundle classNamed:viewClass])
               // Create an instance of the class
               if (id factoryInst = [[[factoryClass alloc] init] autorelease])
                  // Create the view, suggesting a reasonable size
                  if ((mView = [factoryInst uiViewForAudioUnit:mUnit
                                                 withSize:NSSize{800, 600}]))
                     [mView retain];
   }

   if (!mView)
      return;

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
   if (result == noErr && desc.componentType == kAudioUnitType_Panner) {
      mView = [AUPannerView AUPannerViewWithAudioUnit:mUnit];
      if (mView == nil)
         return;
   }
   else {
      // Create a generic AU view
      AUGenericView *view = [AUGenericView alloc];
      if (view == nil)
         return;

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
      return;
   auto vr = valueRestorer( mSettingSize, true );

   NSSize viewSize = [mView frame].size;
   NSSize frameSize = [mAUView frame].size;
   [mAUView setFrameSize:viewSize];
   SetMinSize(wxSize(viewSize.width, viewSize.height));

   int diffW = (viewSize.width - frameSize.width);
   int diffH = (viewSize.height - frameSize.height);

   wxWindow *w = wxGetTopLevelParent(this);

   wxSize min = w->GetMinSize();
   if ([mView autoresizingMask] & (NSViewWidthSizable | NSViewHeightSizable)) {
      min.x += diffW;
      min.y += diffH;
   }
   else {
      min.x += (viewSize.width - mLastMin.GetWidth());
      min.y += (viewSize.height - mLastMin.GetHeight());
      mLastMin = wxSize(viewSize.width, viewSize.height);;
   }
   w->SetMinSize(min);

   // Resize the dialog as well
   w->Fit();

   // Send a "dummy" event to have the OnSize() method recalc mView position
   GetEventHandler()->AddPendingEvent(wxSizeEvent(GetSize()));
}

void AUControl::ForceRedraw()
{
   static_cast<AUView *>(mAUView)->mRedraw = YES;
}

#if !defined(_LP64)

void AUControl::CreateCarbon()
{
   PackedArray::Ptr<AudioComponentDescription> compList;
   if (AudioUnitUtils::GetVariableSizeProperty(mUnit,
      kAudioUnitProperty_GetUIComponentList, compList))
      return;

   // Get the component
   AudioComponent comp = AudioComponentFindNext(nullptr, &compList[0]);

   // Try to create an instance
   auto result = AudioComponentInstanceNew(comp, &mInstance);

   if (result != noErr)
      return;

   Rect bounds = { 100, 100, 200, 200 };
   result = CreateNewWindow(kOverlayWindowClass,
                            kWindowStandardHandlerAttribute |
                            kWindowCompositingAttribute |
                            kWindowOpaqueForEventsAttribute,
                            &bounds,
                            &mWindowRef);
   if (result != noErr) {
      AudioComponentInstanceDispose(mInstance);
      mInstance = nullptr;
      return;
   }

   // Get the root control
   ControlRef root = HIViewGetRoot(mWindowRef);

   // Find the content view within our window
   HIViewRef content;
   result = HIViewFindByID(root, kHIViewWindowContentID, &content);
   if (result != noErr) {
      DisposeWindow(mWindowRef);
      mWindowRef = nullptr;
      AudioComponentInstanceDispose(mInstance);
      mInstance = nullptr;
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
   if (result != noErr) {
      DisposeWindow(mWindowRef);
      mWindowRef = nullptr;
      AudioComponentInstanceDispose(mInstance);
      mInstance = nullptr;
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
                              this,
                              nullptr);

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
   static_cast<AUControl *>(data)->CarbonViewResized();
   return eventNotHandledErr;
}

void AUControl::CarbonViewResized()
{
   if (mSettingSize)
      return;
   auto vr = valueRestorer( mSettingSize, true );

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

   // And finally set the NEW max/min
   w->SetSizeHints(size, size);

   mLastMin = wxSize(rect.size.width, rect.size.height);
}

#endif
