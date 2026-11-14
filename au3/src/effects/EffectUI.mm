/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectUI.mm

  Leland Lucius

  Paul Licameli split from EffectUI.cpp

  Juse one tiny piece of Objective-C++ for Mac only, isolated here

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/
#if defined(__WXMAC__)
#include <Cocoa/Cocoa.h>

void MacMakeWindowFloating(NSView* handle)
{
    // Make sure the effect window actually floats above the main window
    [ [handle window] setLevel:NSFloatingWindowLevel];
}

#endif
