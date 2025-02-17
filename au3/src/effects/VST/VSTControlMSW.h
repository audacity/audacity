/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTControlMSW.h

  Leland Lucius

**********************************************************************/

#ifndef AUDACITY_VSTCONTROLMSW_H
#define AUDACITY_VSTCONTROLMSW_H

#include <Windows.h>

#include "VSTControl.h"

class VSTControl final : public VSTControlBase
{
public:
    VSTControl();
    ~VSTControl();

    bool Create(wxWindow* parent, VSTLink* link);

private:
    HANDLE mHwnd;
};

#endif
