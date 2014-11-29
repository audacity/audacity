/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioUnitCocoaHelper.m

  Leland Lucius

*******************************************************************//**

\class AUScrollView
\brief An NSScrollView subclass that hosts AUs

NOTE:  I do NOT to Objective-C(++), so if you see ANYTHING wrong do
       not hesitate to let me know. -Leland

*//*******************************************************************/

#import <Carbon/Carbon.h>
#import <AudioUnit/AudioUnit.h>
#import <AudioUnit/AUCocoaUIView.h>
#import <CoreAudioKit/CoreAudioKit.h>
#import <AppKit/NSScrollView.h>

#import "AudioUnitCocoaHelper.h"

@interface AUScrollView: NSScrollView
{
   NSView *auView;
   HIViewRef hiViewRef;
}

- (id)initWithFrame:(NSRect)frameRect;

/* Might be useful for improved resizing
- (void)auResized:(NSNotification *)notification;
- (void)myResized:(NSNotification *)notification;
*/

@end

@implementation AUScrollView

- (id)initWithFrame:(NSRect)frameRect
{
   self = [super initWithFrame:frameRect];

   return self;
}

- (void)reflectScrolledClipView:(NSClipView *)aClipView
{
   [super reflectScrolledClipView:aClipView];

   // Force a full refresh as some effects seem to need it
   [self setNeedsDisplay:YES];
}

- (BOOL)autoresizesSubviews
{
   // Let NSView automatically resize our children
   return YES;
}
@end

/* Might be useful for improved resizing.  I'd worked up this
   really elaborate resizing stuff that "almost" worked.  Rather
   than spend even more time, I decided to get rid of it all and
   let resizing happen as it will.  This code should be short
   lived anyway as it will probably not be needed once we convert
   to wxWidgets 3+.

-(void) setViews:(NSView *)aView hiViewRef:(HIViewRef)hView
{
   auView = aView;
   hiViewRef = hView;

   [[NSNotificationCenter defaultCenter]
         addObserver:self
         selector:@selector(myResized:) name:NSViewFrameDidChangeNotification object:self];

   [[NSNotificationCenter defaultCenter]
         addObserver:self
         selector:@selector(auResized:) name:NSViewFrameDidChangeNotification object:auView];

}

- (void)myResized:(NSNotification *)notification
{
}

- (void)auResized:(NSNotification *)notification
{
}

*/

///////////////////////////////////////////////////////////////////////////////
// Create a Cocoa based generic AU view wrapped in a Carbon view
///////////////////////////////////////////////////////////////////////////////
HIViewRef createGeneric(AudioUnit unit)
{
   HIViewRef hiView = NULL;
   OSStatus result;

   // Create a generic AU view
   NSView *auView = [[[AUGenericView alloc] initWithAudioUnit: unit] retain];
   if (auView != nil)
   {
      // Allow expert parameters to be used
      [(AUGenericView *) auView setShowsExpertParameters:YES];

      // Get the AU view's frame for later
      NSRect viewFrame = [auView frame];

      // Create the view that will host the AU view
      AUScrollView *scrollView =
         [[[AUScrollView alloc] initWithFrame:viewFrame] autorelease];

      // Not sure if this is necessary, but crashes seemed to occur
      // without it.
      [scrollView retain];

      // Set the scroller options
      [scrollView setDrawsBackground:YES];
      [scrollView setAutohidesScrollers:YES];
      [scrollView setHasHorizontalScroller:YES];
      [scrollView setHasVerticalScroller:YES];
      [scrollView setBorderType:NSNoBorder];
      [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];

      // Let the scrollview know about the AU view
      //
      // Should the AU view be released after this???
      [scrollView setDocumentView:auView];
      // [auView release];

      // Carbonize it
      result = HICocoaViewCreate(scrollView, 0, &hiView);
      if (result == noErr)
      {
         // Resize the HIView to match the AU view
         SizeControl(hiView, viewFrame.size.width, viewFrame.size.height);
      }
   }

   return hiView;
}

///////////////////////////////////////////////////////////////////////////////
// Create a Cocoa based generic panner AU view wrapped in a Carbon view
///////////////////////////////////////////////////////////////////////////////
HIViewRef createPanner(AudioUnit unit)
{
   HIViewRef hiView = NULL;
   OSStatus result;

   // Create a generic AU view
   NSView *auView = [[AUPannerView AUPannerViewWithAudioUnit:unit] retain];
   if (auView != nil)
   {
      // Get the AU view's frame for later
      NSRect viewFrame = [auView frame];

      // Create the view that will host the AU view
      AUScrollView *scrollView =
         [[[AUScrollView alloc] initWithFrame:viewFrame] autorelease];

      // Not sure if this is necessary, but crashes seemed to occur
      // without it.
      [scrollView retain];

      // Set the scroller options
      [scrollView setDrawsBackground:YES];
      [scrollView setAutohidesScrollers:YES];
      [scrollView setHasHorizontalScroller:YES];
      [scrollView setHasVerticalScroller:YES];
      [scrollView setBorderType:NSNoBorder];
      [scrollView setAutoresizingMask:NSViewWidthSizable|NSViewHeightSizable];

      // Let the scrollview know about the AU view
      //
      // Should the AU view be released after this???
      [scrollView setDocumentView:auView];
      // [auView release];

      // Carbonize it
      result = HICocoaViewCreate(scrollView, 0, &hiView);
      if (result == noErr)
      {
         // Resize the HIView to match the AU view
         SizeControl(hiView, viewFrame.size.width, viewFrame.size.height);
      }
   }

   return hiView;
}

///////////////////////////////////////////////////////////////////////////////
// Create a Cocoa based custom AU view wrapped in a Carbon view
///////////////////////////////////////////////////////////////////////////////
HIViewRef createCocoa(AudioUnit unit)
{
   HIViewRef hiView = NULL;
   OSStatus result;

   AudioUnitCocoaViewInfo cocoaViewInfo;
   UInt32 dataSize = sizeof(AudioUnitCocoaViewInfo);

   // Get info about first Cocoa view
   result = AudioUnitGetProperty(unit,
                                 kAudioUnitProperty_CocoaUI,
                                 kAudioUnitScope_Global,
                                 0,
                                 &cocoaViewInfo,
                                 &dataSize);
   if (result != noErr)
   {
      return NULL;
   }

   // Looks like the AU has a Cocoa UI, so load the factory class
   NSURL *bundleLoc = (NSURL *) cocoaViewInfo.mCocoaAUViewBundleLocation;
   NSString *className = (NSString *) cocoaViewInfo.mCocoaAUViewClass[0];
   if (!bundleLoc || !className)
   {
      return NULL;
   }

   // Load the bundle
   NSBundle *bundle = [NSBundle bundleWithPath: [bundleLoc path]];
   if (bundle != nil)
   {
      // Load the class from the bundle
      Class factoryClass = [bundle classNamed: className];
      if (factoryClass != nil)
      {      
         // Create an instance of the class
         id factoryInst = [[[factoryClass alloc] init] autorelease];
         if (factoryInst != nil)
         {
            // Suggest a resonable size
            NSSize size = {800, 600};

            // Create the view
            NSView *auView = [[factoryInst uiViewForAudioUnit: unit withSize: size] retain];
            if (auView != nil)
            {
               // Get the AU views frame for later
               NSRect viewFrame = [auView frame];

               // Create the view that will host the AU view
               AUScrollView *scrollView =
                  [[[AUScrollView alloc] initWithFrame:viewFrame] autorelease];

               // Not sure if this is necessary, but crashes seemed to occur
               // without it.
               [scrollView retain];

               // Set the scroller options
               [scrollView setDrawsBackground:YES];
               [scrollView setAutohidesScrollers:YES];
               [scrollView setHasHorizontalScroller:YES];
               [scrollView setHasVerticalScroller:YES];
               [scrollView setBorderType:NSNoBorder];

               // Let the scrollview know about the AU view
               //
               // Should the AU view be released after this???
               [scrollView setDocumentView:auView];

               // Carbonize it
               result = HICocoaViewCreate(scrollView, 0, &hiView);
               if (result == noErr)
               {
                  // Resize the HIView to match the AU view
                  SizeControl(hiView, viewFrame.size.width, viewFrame.size.height);
               }
            }
         }
      }

      // Release the bundle???
      // [bundle release];
   }

   // Release the bundle path
   [bundleLoc release];

   return hiView;
}

///////////////////////////////////////////////////////////////////////////////
// Create a Carbon based AU view
///////////////////////////////////////////////////////////////////////////////
HIViewRef createCarbon(AudioUnit unit, WindowRef window, AudioUnitCarbonView *carbonView)
{
   HIViewRef hiView = NULL;
   OSStatus result;

   // Retrieve the view component description
   ComponentDescription compDesc;
   UInt32 dataSize = sizeof(compDesc);
   result = AudioUnitGetProperty(unit,
                                 kAudioUnitProperty_GetUIComponentList,
                                 kAudioUnitScope_Global,
                                 0,
                                 &compDesc,
                                 &dataSize);
   if (result != noErr)
   {
      return NULL;
   }

   // Try to open it
   Component comp = FindNextComponent(NULL, &compDesc);
   result = OpenAComponent(comp, carbonView);
   if (result != noErr)
   {
      return NULL;
   }

   // Get the root control
   ControlRef root = HIViewGetRoot(window);
   GetRootControl(window, &root);
   
   // Find the content view within our window
   HIViewRef content;
   result = HIViewFindByID(root, kHIViewWindowContentID, &content);
   if (result != noErr)
   {
      CloseComponent(*carbonView);
      return NULL;
   }

   // Suggest a reasonable size
   Float32Point loc = {0.0, 0.0};
   Float32Point size = {800.0, 600.0};

   // And create it
   result = AudioUnitCarbonViewCreate(*carbonView,
                                      unit,
                                      window,
                                      root,
                                      &loc,
                                      &size,
                                      &hiView);

   return hiView;
}
