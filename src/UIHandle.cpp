/**********************************************************************

Audacity: A Digital Audio Editor

UIHandle.cpp

Paul Licameli

**********************************************************************/

#include "Audacity.h"
#include "UIHandle.h"

UIHandle::~UIHandle()
{
}

void UIHandle::DrawExtras
   (DrawingPass, wxDC *, const wxRegion &, const wxRect &)
{
}

bool UIHandle::StopsOnKeystroke()
{
   return false;
}

void UIHandle::OnProjectChange(AudacityProject *)
{
}
