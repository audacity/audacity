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

- (void)stripAutoFillFrom:(NSMenu*)menu
{
    for (NSInteger i = menu.numberOfItems - 1; i >= 0; i--) {
        if ([[menu itemAtIndex:i].title isEqualToString:@"AutoFill"]) {
            [menu removeItemAtIndex:i];
        }
    }
    while (menu.numberOfItems > 0 && [[menu itemAtIndex:0] isSeparatorItem]) {
        [menu removeItemAtIndex:0];
    }
    while (menu.numberOfItems > 0
           && [[menu itemAtIndex:menu.numberOfItems - 1] isSeparatorItem]) {
        [menu removeItemAtIndex:menu.numberOfItems - 1];
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

using namespace au::appshell;

void MacOSAppMenuModelHook::onAppMenuInited()
{
    [[NSUserDefaults standardUserDefaults] setBool:YES forKey:@"NSDisabledDictationMenuItem"];
    [[NSUserDefaults standardUserDefaults] setBool:YES forKey:@"NSDisabledCharacterPaletteMenuItem"];

    static AUMenuAutoFillFilter* filter = [[AUMenuAutoFillFilter alloc] init];

    // queue:nil → synchronous delivery so we catch the injection regardless of
    // runloop mode (menu tracking uses a non-default mode that NSOperationQueue
    // mainQueue doesn't service). We must NOT mutate the menu from this block
    // because it fires inside -[NSMenu insertItem:atIndex:] — doing so would
    // cause a re-entrancy assertion crash. Instead we dispatch the actual work
    // to GCD which runs on the main thread as soon as the current call unwinds.
    [[NSNotificationCenter defaultCenter]
     addObserverForName:NSMenuDidAddItemNotification
     object:nil
     queue:nil
     usingBlock:^(NSNotification* note) {
         NSMenu* menu = note.object;
         NSInteger index = [[note.userInfo objectForKey:@"NSMenuItemIndex"] integerValue];
         if (index < 0 || index >= menu.numberOfItems) {
             return;
         }
         if (![[menu itemAtIndex:index].title isEqualToString:@"AutoFill"]) {
             return;
         }
         dispatch_async(dispatch_get_main_queue(), ^{
                            // Qt may have replaced the NSMenu object since the notification fired,
                            // so look up the current Edit submenu rather than using the captured ref.
                            NSMenu* editMenu = [[[NSApp mainMenu] itemWithTitle:@"Edit"] submenu];
                            if (!editMenu) {
                                return;
                            }
                            [filter stripAutoFillFrom:editMenu];
                            if (editMenu.delegate != filter) {
                                filter.chainedDelegate = editMenu.delegate;
                                editMenu.delegate = filter;
                            }
                        });
     }];
}
