/**********************************************************************

  Sneedacity: A Digital Audio Editor

  VSTControlMSW.h

  Leland Lucius

**********************************************************************/

#ifndef SNEEDACITY_VSTCONTROLMSW_H
#define SNEEDACITY_VSTCONTROLMSW_H

#include <Windows.h>

#include "VSTControl.h"

class VSTControl final : public VSTControlBase
{
public:
   VSTControl();
   ~VSTControl();

   bool Create(wxWindow *parent, VSTEffectLink *link);

private:
   HANDLE mHwnd;
};

#endif
