/**********************************************************************

  Audacity: A Digital Audio Editor

**********************************************************************/

#pragma once

#include <wx/wx.h>
#include <wx/graphics.h>
#include <wx/dcbuffer.h>

enum class ArrowDirection { Left, Right };

class WX_WRAPPERS_API ArrowButton : public wxButton {
public:
   ArrowButton(wxWindow* parent, ArrowDirection direction);

   void SetClickHandler(std::function<void()> handler);

private:
   void OnPaint(wxPaintEvent& event);
   void OnButtonClick(wxCommandEvent& event);

   ArrowDirection m_direction;
   std::function<void()> m_onClick;

   wxDECLARE_EVENT_TABLE();
};
