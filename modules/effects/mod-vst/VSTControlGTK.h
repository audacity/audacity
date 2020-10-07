/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTControlGTK.h

  Leland Lucius

**********************************************************************/

#ifndef AUDACITY_VSTCONTROLGTK_H
#define AUDACITY_VSTCONTROLGTK_H

#include <memory>
#include "VSTControl.h"

class VSTControl final : public VSTControlBase
{
public:
   VSTControl();
   virtual ~VSTControl();

   bool Create(wxWindow *parent, VSTLink *link) override;

private:
   struct Impl;
   std::unique_ptr<Impl> mImpl;
};

#endif
