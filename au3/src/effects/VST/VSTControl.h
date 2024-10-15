/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTControl.h

  Leland Lucius

**********************************************************************/

#ifndef AUDACITY_VSTCONTROL_H
#define AUDACITY_VSTCONTROL_H

#include <wx/control.h> // to inherit

#include "VSTWrapper.h" // for VSTLink

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

   virtual bool Create(wxWindow *parent, VSTLink *link)
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
   VSTLink *mLink;
};

#endif
