/*!********************************************************************

Audacity: A Digital Audio Editor

@file wxWidgetsBasicUI.h
@brief Implementation of BasicUI::WindowPlacement using wxWidgets

split from wxWidgetsBasicUI.h

Paul Licameli

**********************************************************************/
#ifndef __WXWIDGETS_WINDOW_PLACEMENT__
#define __WXWIDGETS_WINDOW_PLACEMENT__

#include "BasicUI.h"

class wxWindow;

//! Window placement information for wxWidgetsBasicUI can be constructed from a wxWindow pointer
struct SHUTTLEGUI_API wxWidgetsWindowPlacement final
: BasicUI::WindowPlacement {
   //! Retrieve the pointer to window, if placement is of this type; else null
   static wxWindow *GetParent(const WindowPlacement &placement);

   wxWidgetsWindowPlacement() = default;

   //! Construct from a pointer to window which may be null
   explicit wxWidgetsWindowPlacement( wxWindow *pWindow )
      : pWindow{ pWindow }
   {}

   ~wxWidgetsWindowPlacement() override;

   explicit operator bool() const override;

   wxWindow *pWindow{};
};

#endif
