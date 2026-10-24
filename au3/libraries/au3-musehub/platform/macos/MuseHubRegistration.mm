/*
 * Audacity: A Digital Audio Editor
 */
#include "MuseHubService.h"

#import <AppKit/NSWorkspace.h>

namespace audacity::musehub {
bool IsMuseHubRegistered()
{
    @autoreleasepool {
        NSURL* appUrl = [[NSWorkspace sharedWorkspace] URLForApplicationToOpenURL:[NSURL URLWithString:@"musehub://plugins"]];
        return appUrl != nil;
    }
}
}
