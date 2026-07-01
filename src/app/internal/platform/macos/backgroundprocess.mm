/*
 * Audacity: A Digital Audio Editor
 */
#include "backgroundprocess.h"

#import <Cocoa/Cocoa.h>

#include <objc/runtime.h>

namespace {
using SetActivationPolicyFn = BOOL (*)(id, SEL, NSApplicationActivationPolicy);
SetActivationPolicyFn s_originalSetActivationPolicy = nullptr;

BOOL keepAccessoryActivationPolicy(id self, SEL cmd, NSApplicationActivationPolicy)
{
    return s_originalSetActivationPolicy
           ? s_originalSetActivationPolicy(self, cmd, NSApplicationActivationPolicyAccessory)
           : NO;
}
}

namespace au::app {
void makeProcessBackground()
{
    // Mutate info.plist to make the app UIElement
    // It is allowed to create windows, but no dock icon
    NSMutableDictionary* infoDictionary = static_cast<NSMutableDictionary*>([[NSBundle mainBundle] infoDictionary]);
    [infoDictionary setObject:@YES forKey:@"LSUIElement"];

    // Overriding setActivationPolicy, to force NSApplicationActivationPolicyAccessory
    Method method = class_getInstanceMethod([NSApplication class], @selector(setActivationPolicy:));
    if (method && !s_originalSetActivationPolicy) {
        s_originalSetActivationPolicy = reinterpret_cast<SetActivationPolicyFn>(method_getImplementation(method));
        method_setImplementation(method, reinterpret_cast<IMP>(&keepAccessoryActivationPolicy));
    }
}
}
