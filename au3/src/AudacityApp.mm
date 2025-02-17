#ifdef __WXMAC__

#include "AudacityApp.h"

#include <AppKit/NSApplication.h>

// Mac-only methods of class AudacityApp that use Objective-C are kept
// here so that AudacityApp.cpp can be just C++

// The function is not presently used.  See commit
// a05d039055909d7d1dc2d4f31e1fe0659a3207dd
// whih added this function, and later commit
// e359383d3bebd583b894c9ee9d02cce2be318b56
// which made it unused.  Maybe it will need future use.

void AudacityApp::MacActivateApp()
{
    id app = [NSApplication sharedApplication];
    if ([app respondsToSelector:@selector(activateIgnoringOtherApps:)]) {
        [app activateIgnoringOtherApps:YES];
    }
}

void AudacityApp::MacFinishLaunching()
{
    [NSApp finishLaunching];
}

void AudacityApp::OnThemeChange(ThemeChangeMessage message)
{
    if (!message.appearance) {
        return;
    }

    // This API only works 10.14+
    // Previous versions will always use Light appearance
#if defined(__MAC_10_14) && defined(__MAC_OS_X_VERSION_MAX_ALLOWED) && __MAC_OS_X_VERSION_MAX_ALLOWED >= __MAC_10_14
    if (@available(macOS 10.14, *)) {
        NSAppearanceName appearanceName;

        switch (*message.appearance) {
        case PreferredSystemAppearance::Light:
            appearanceName = NSAppearanceNameAqua;
            break;
        case PreferredSystemAppearance::Dark:
            appearanceName = NSAppearanceNameDarkAqua;
            break;
        case PreferredSystemAppearance::HighContrastDark:
            appearanceName = NSAppearanceNameAccessibilityHighContrastDarkAqua;
            break;
        }

        NSAppearance* systemAppearance = [NSAppearance appearanceNamed:appearanceName];

        if (systemAppearance != nil) {
            NSApp.appearance = systemAppearance;
        }
    }
#endif
}

#endif
