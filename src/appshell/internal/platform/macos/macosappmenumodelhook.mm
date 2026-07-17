/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "macosappmenumodelhook.h"

#include <Cocoa/Cocoa.h>

@interface AUMenuAutoFillFilter : NSObject <NSMenuDelegate>
@property (nonatomic, assign) id<NSMenuDelegate> chainedDelegate;
- (void)stripAutoFillFrom:(NSMenu*)menu;
@end

@implementation AUMenuAutoFillFilter

- (BOOL)isAutoFillItem:(NSMenuItem*)item
{
    // AutoFill is injected by AppKit as a submenu whose children use private
    // Apple selectors. Qt's own submenus always contain at least one item with
    // qt_itemFired:, so the absence of that selector identifies an injected
    // submenu without depending on the locale-sensitive item title.
    if (!item.hasSubmenu || item.action != @selector(submenuAction:)) {
        return NO;
    }
    SEL qtFired = NSSelectorFromString(@"qt_itemFired:");
    for (NSMenuItem* child in item.submenu.itemArray) {
        if (child.action == qtFired) {
            return NO;
        }
    }
    return item.submenu.numberOfItems > 0;
}

- (void)stripAutoFillFrom:(NSMenu*)menu
{
    for (NSInteger i = menu.numberOfItems - 1; i >= 0; i--) {
        if (![self isAutoFillItem:[menu itemAtIndex:i]]) {
            continue;
        }
        [menu removeItemAtIndex:i];
        // Remove only the separator directly adjacent to the removed item so
        // that unrelated menus with intentional leading/trailing separators are
        // left intact.
        if (i < menu.numberOfItems && [[menu itemAtIndex:i] isSeparatorItem]) {
            [menu removeItemAtIndex:i];
        } else if (i > 0 && [[menu itemAtIndex:i - 1] isSeparatorItem]) {
            [menu removeItemAtIndex:i - 1];
            i--;
        }
    }
}

- (void)menuWillOpen:(NSMenu*)menu
{
    [self stripAutoFillFrom:menu];
    if ([_chainedDelegate respondsToSelector:@selector(menuWillOpen:)]) {
        [_chainedDelegate menuWillOpen:menu];
    }
}

- (BOOL)respondsToSelector:(SEL)sel
{
    return [super respondsToSelector:sel] || [_chainedDelegate respondsToSelector:sel];
}

- (id)forwardingTargetForSelector:(SEL)sel
{
    return [_chainedDelegate respondsToSelector:sel] ? _chainedDelegate : [super forwardingTargetForSelector:sel];
}

@end

// Retained owner for per-menu delegate instances (NSMenu.delegate is weak/assign).
static NSMutableArray* s_filters = nil;

using namespace au::appshell;

void MacOSAppMenuModelHook::onAppMenuInited()
{
    [[NSUserDefaults standardUserDefaults] setBool:YES forKey:@"NSDisabledDictationMenuItem"];
    [[NSUserDefaults standardUserDefaults] setBool:YES forKey:@"NSDisabledCharacterPaletteMenuItem"];

    // Qt uses qt_itemFired: for all menu items rather than standard Cocoa selectors,
    // so the Edit menu cannot be identified by action. Install the delegate on every
    // top-level submenu instead — stripAutoFillFrom: is a no-op on menus that
    // contain no injected items.
    dispatch_async(dispatch_get_main_queue(), ^{
        if (!s_filters) {
            s_filters = [[NSMutableArray alloc] init];
        }
        for (NSMenuItem* topItem in [NSApp mainMenu].itemArray) {
            NSMenu* sub = topItem.submenu;
            if (!sub || [sub.delegate isKindOfClass:[AUMenuAutoFillFilter class]]) {
                continue;
            }
            AUMenuAutoFillFilter* f = [[AUMenuAutoFillFilter alloc] init];
            f.chainedDelegate = sub.delegate;
            sub.delegate = f;
            [s_filters addObject:f];
            [f release];
        }
    });
}
