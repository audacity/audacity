/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTControl.h

  Leland Lucius

**********************************************************************/

#ifndef AUDACITY_VSTCONTROL_H
#define AUDACITY_VSTCONTROL_H

#include <wx/control.h> // to inherit

#include "aeffectx.h"

class VSTEffectLink /* not final */
{
public:
   virtual ~VSTEffectLink() {};
   virtual intptr_t callDispatcher(int opcode, int index, intptr_t value, void *ptr, float opt) = 0;
};

class VSTControlBase /* not final */ : public wxControl
{
public:
   VSTControlBase()
   {
      mParent = NULL;
      mLink = NULL;
   }

   virtual ~VSTControlBase()
   {
   }

   virtual bool Create(wxWindow *parent, VSTEffectLink *link)
   {
      mParent = parent;
      mLink = link;

      if (!wxControl::Create(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxNO_BORDER | wxTAB_TRAVERSAL, wxDefaultValidator, wxEmptyString))
      {
         return false;
      }

      return true;
   }
   
protected:
   wxWindow *mParent;
   VSTEffectLink *mLink;
};

#endif
