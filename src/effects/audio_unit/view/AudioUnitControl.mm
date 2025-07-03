/*
 * Audacity: A Digital Audio Editor
 */

#include "audiounitcontrol.h"

#include <AudioUnit/AudioUnitProperties.h>
#include <CoreAudioKit/CoreAudioKit.h>
#include <QTimer>

#include "AudioUnitUtils.h"

@interface AUView : NSView
{
    @public
    AUControl* mControl;
    NSView* mView;
}
@end

@implementation AUView

- (instancetype)initWithControl:(AUControl*)control
{
    NSParameterAssert(control);

    mControl = control;

    [super init];

    return self;
}

- (BOOL)autoresizesSubviews
{
    return NO;
}

- (BOOL)isFlipped {
    return YES;
}

- (void)cocoaViewResized:(NSNotification*)notification
{
    mControl->cocoaViewResized();
}

@end

inline AUView* asAUView(void* obj)
{
    return static_cast<AUView*>(obj);
}

inline NSView* asNSView(void* obj)
{
    return static_cast<NSView*>(obj);
}

AUControl::AUControl(QWindow* parent)
    : QWindow(parent)
    , mAUView(nullptr)
    , mView(nullptr)
    , mComponent(nullptr)
    , mUnit(nullptr)
{
}

AUControl::~AUControl()
{
    close();
}

void AUControl::close()
{
    if (mView) {
        NSNotificationCenter* center = [NSNotificationCenter defaultCenter];
        [center removeObserver:asNSView(mAUView)
         name:NSViewFrameDidChangeNotification
         object:asNSView(mView)];

        [asNSView(mView) release];
        mView = nullptr;
    }

    if (mAUView) {
        [asAUView(mAUView) release];
        mAUView = nullptr;
    }
}

bool AUControl::create(AudioComponent comp, AudioUnit unit, bool custom)
{
    mComponent = comp;
    mUnit = unit;
    show();

    mAUView = [AUView alloc];
    if (!mAUView) {
        return false;
    }
    [asAUView(mAUView) initWithControl: this];
    [asAUView(mAUView) retain];

    if (custom) {
        createCocoa();
    }

    if (!mView) {
        createGeneric();
    }

    if (!mView) {
        return false;
    }

    asAUView(mAUView)->mView = asNSView(mView);

    // Attach mAUView to the native window
    WId nativeWindowId = winId();
    NSView* parentView = reinterpret_cast<NSView*>(nativeWindowId);
    [parentView addSubview:asNSView(mAUView)];

    // Set initial size
    NSSize viewSize = [asNSView(mView) frame].size;
    mFixedSize = QSize(viewSize.width, viewSize.height);
    setMinimumSize(mFixedSize);
    resize(mFixedSize);

    QTimer::singleShot(0, this, [this]() {
        cocoaViewResized();
    });

    return true;
}

void AUControl::resizeEvent(QResizeEvent* event)
{
    // TODO: implement when effect windows are resizable
    event->accept();
}

void AUControl::createCocoa()
{
    PackedArray::Ptr<AudioUnitCocoaViewInfo> viewInfo;
    if (!AudioUnitUtils::GetVariableSizeProperty(mUnit, kAudioUnitProperty_CocoaUI, viewInfo)) {
        // Looks like the AU has a Cocoa UI, so load the factory class
        auto bundleLoc = static_cast<NSURL*>(viewInfo->mCocoaAUViewBundleLocation);
        auto viewClass = static_cast<NSString*>(viewInfo->mCocoaAUViewClass[0]);
        if (bundleLoc && viewClass) {
            // Load the bundle
            if (auto bundle = [NSBundle bundleWithPath:[bundleLoc path]]) {
                // Load the class from the bundle
                if (auto factoryClass = [bundle classNamed:viewClass]) {
                    // Create an instance of the class
                    if (id factoryInst = [[[factoryClass alloc] init] autorelease]) {
                        // Create the view, suggesting a reasonable size
                        try {
                            if ((mView = [factoryInst uiViewForAudioUnit:mUnit
                                          withSize:NSSize{ 800, 600 }])) {
                                [asNSView(mView) retain];
                            }
                        } catch (...) {
                        }
                    }
                }
            }
        }
    }

    if (!mView) {
        return;
    }
    setupView();
}

void AUControl::createGeneric()
{
    OSStatus result;
    AudioComponentDescription desc;

    result = AudioComponentGetDescription(mComponent, &desc);
    if (result == noErr && desc.componentType == kAudioUnitType_Panner) {
        mView = [AUPannerView AUPannerViewWithAudioUnit:mUnit];
        if (mView == nil) {
            return;
        }
    } else {
        // Create a generic AU view
        AUGenericView* view = [AUGenericView alloc];
        if (view == nil) {
            return;
        }

        int flags = AUViewPropertiesDisplayFlag | AUViewParametersDisplayFlag;
        [view initWithAudioUnit:mUnit displayFlags:flags];
        [view setShowsExpertParameters:YES];
        mView = view;
    }
    [asNSView(mView) retain];
    setupView();
}

void AUControl::setupView()
{
    [asAUView(mAUView) addSubview: asNSView(mView)];

    NSSize desiredSize = [asNSView(mView) frame].size;

    [asNSView(mView) setFrameSize: desiredSize];
    [asNSView(mView) setAutoresizingMask: NSViewWidthSizable | NSViewHeightSizable];
    [asAUView(mAUView) setFrameSize: desiredSize];

    mFixedSize = QSize(desiredSize.width, desiredSize.height);
    setMinimumSize(mFixedSize);

    NSNotificationCenter* center = [NSNotificationCenter defaultCenter];
    [center addObserver:asNSView(mAUView)
     selector:@selector(cocoaViewResized:)
     name:NSViewFrameDidChangeNotification
     object:asNSView(mView)];
}

void AUControl::cocoaViewResized()
{
    NSSize viewSize = [asNSView(mView) frame].size;
    [asAUView(mAUView) setFrameSize: viewSize];

    mFixedSize = QSize(viewSize.width, viewSize.height);
    setMinimumSize(mFixedSize);
    resize(mFixedSize);

    emit sizeChanged();
}
