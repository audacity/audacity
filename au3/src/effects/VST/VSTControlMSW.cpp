/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTControlMSW.cpp

  Leland Lucius

**********************************************************************/

#include "VSTControlMSW.h"

#include <wx/dynlib.h>
#include <wx/sizer.h>

VSTControl::VSTControl()
    :  VSTControlBase()
{
}

VSTControl::~VSTControl()
{
    if (mHwnd) {
        mLink->callDispatcher(effEditClose, 0, 0, mHwnd, 0.0);
        mHwnd = 0;
    }
}

bool VSTControl::Create(wxWindow* parent, VSTLink* link)
{
    if (!VSTControlBase::Create(parent, link)) {
        return false;
    }

    VstRect* rect;

    // Some effects like to have us get their rect before opening them.
    mLink->callDispatcher(effEditGetRect, 0, 0, &rect, 0.0);

    // Get the native handle
    mHwnd = GetHWND();

    // Ask the effect to add its GUI
    mLink->callDispatcher(effEditOpen, 0, 0, mHwnd, 0.0);

    // Get the final bounds of the effect GUI
    mLink->callDispatcher(effEditGetRect, 0, 0, &rect, 0.0);

    // Add the effect host window to the layout
    SetMinSize(wxSize(rect->right - rect->left, rect->bottom - rect->top));

    // Must get the size again since SetPeer() could cause it to change
    SetInitialSize(GetMinSize());

    return true;
}
