/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandManagerWindowClasses.cpp

  Paul Licameli

**********************************************************************/
#include "CommandManagerWindowClasses.h"

NonKeystrokeInterceptingWindow::~NonKeystrokeInterceptingWindow() = default;

TopLevelKeystrokeHandlingWindow::~TopLevelKeystrokeHandlingWindow() = default;

bool TopLevelKeystrokeHandlingWindow::HandleCommandKeystrokes()
{
    return true;
}
