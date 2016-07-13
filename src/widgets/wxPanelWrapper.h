//
//  wxPanelWrapper.h
//  Audacity
//
//  Created by Paul Licameli on 6/25/16.
//
//

#ifndef __AUDACITY_WXPANEL_WRAPPER__
#define __AUDACITY_WXPANEL_WRAPPER__

#include "../MemoryX.h"
#include <wx/panel.h>
#include <wx/dialog.h>

void wxTabTraversalWrapperCharHook(wxKeyEvent &event);

template <typename Base>
class wxTabTraversalWrapper : public Base
{
public:
   template <typename... Args>
   wxTabTraversalWrapper(Args&&... args)
   : Base( std::forward<Args>(args)... )
   {
      this->Bind(wxEVT_CHAR_HOOK, wxTabTraversalWrapperCharHook);
   }

   ~wxTabTraversalWrapper()
   {
      this->Unbind(wxEVT_CHAR_HOOK, wxTabTraversalWrapperCharHook);
   }
};

class wxPanel;
using wxPanelWrapper = wxTabTraversalWrapper<wxPanel>;

class wxDialog;
using wxDialogWrapper = wxTabTraversalWrapper<wxDialog>;

#endif
