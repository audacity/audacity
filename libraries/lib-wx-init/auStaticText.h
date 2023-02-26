/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file auStaticText.h

  Paul Licameli split from Theme.h

**********************************************************************/

#ifndef __AUDACITY_AU_STATIC_TEXT__
#define __AUDACITY_AU_STATIC_TEXT__

#include <wx/window.h> // to inherit

class wxString;
class wxPaintEvent;

class WX_INIT_API auStaticText : public wxWindow
{
public:
   auStaticText(wxWindow* parent, wxString text);
   void OnPaint(wxPaintEvent & evt);
   bool AcceptsFocus() const override { return false; }
   void OnErase(wxEraseEvent& event) {
      static_cast<void>(event);
   };
   DECLARE_EVENT_TABLE();
};

#endif
