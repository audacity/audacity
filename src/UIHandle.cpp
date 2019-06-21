/**********************************************************************

Audacity: A Digital Audio Editor

UIHandle.cpp

Paul Licameli

**********************************************************************/

#include "Audacity.h"
#include "UIHandle.h"

#include "RefreshCode.h"

UIHandle::~UIHandle()
{
}

void UIHandle::Enter(bool)
{
}

bool UIHandle::HasRotation() const
{
   return false;
}

bool UIHandle::Rotate(bool)
{
   return false;
}

bool UIHandle::HasEscape() const
{
   return false;
}

bool UIHandle::Escape()
{
   return false;
}

bool UIHandle::StopsOnKeystroke()
{
   return false;
}

void UIHandle::OnProjectChange(AudacityProject *)
{
}
