/**********************************************************************

   Audacity: A Digital Audio Editor

   SliderTextCtrl.cpp

   Max Maisel

*******************************************************************//**

\class SliderTextCtrl
\brief A slider with connected text box.

*//*******************************************************************/


#include "../Audacity.h"
#include "audacity/Types.h"
#include "SliderTextCtrl.h"

#include <wx/defs.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/textctrl.h>
#include <wx/valnum.h>

wxDEFINE_EVENT(cEVT_SLIDERTEXT, wxCommandEvent);

SliderTextCtrl::SliderTextCtrl(wxWindow *parent, wxWindowID winid,
   double value, double min, double max, int precision, double scale,
   const wxPoint& pos, const wxSize& size, long style, double* varValue)
   : wxPanelWrapper(parent, winid, pos, size, wxWS_EX_VALIDATE_RECURSIVELY)
{
   m_log = style & LOG;
   m_int = style & INT;
   m_value = value;
   m_min = min;
   m_max = max;
   m_zero = -std::numeric_limits<double>::infinity();

   if(m_int)
   {
      precision = 0;
      m_format = "%d";
   }
   else
      m_format = wxString::Format("%%.%df", precision);

   if(scale == 0)
      m_scale = pow(10, precision);
   else
      m_scale = scale;

   wxFloatingPointValidator<double> validator(precision, varValue);

   if(m_log)
   {
      if(min <= 0.0)
      {
         m_zero = -double(precision) - 1.0 / m_scale;
         min = m_zero;
      }
      else
         min = log10(min);

      if(value <= 0.0)
         value = m_zero;
      else
         value = log10(value);
      max = log10(max);
   }

   m_sizer = safenew wxBoxSizer(
      style & HORIZONTAL ? wxHORIZONTAL : wxVERTICAL);
   m_slider = safenew wxSlider(this, ID_SLIDER,
      round(value * m_scale), floor(min * m_scale), ceil(max * m_scale),
      wxDefaultPosition, wxDefaultSize,
      style & HORIZONTAL ? wxSL_HORIZONTAL : wxSL_VERTICAL);
   m_textbox = safenew wxTextCtrl(this, ID_TEXTBOX, wxEmptyString,
      wxDefaultPosition, wxDefaultSize, 0, validator);

   m_textbox->ChangeValue(FormatValue());
   m_textbox->Bind(wxEVT_KILL_FOCUS, &SliderTextCtrl::OnKillFocus, this);

   m_sizer->Add(m_slider, 1, wxEXPAND);
   m_sizer->Add(m_textbox, 0, wxEXPAND);

   SetSizer(m_sizer);
}

void SliderTextCtrl::SetMinTextboxWidth(int width)
{
    wxSize size = GetMinSize();
    size.SetWidth(width);
    m_textbox->SetMinSize(size);
}

double SliderTextCtrl::GetValue() const
{
   return m_value;
}

void SliderTextCtrl::SetValue(double value)
{
   m_value = value;
   m_textbox->ChangeValue(FormatValue());
}

void SliderTextCtrl::OnTextChange(wxCommandEvent& event)
{
   double value;
   m_textbox->GetValue().ToDouble(&value);
   m_value = std::min(value, m_max);
   m_value = std::max(m_value, m_min);
   if(m_log)
   {
      if(m_value == 0.0)
         value = m_zero;
      else
         value = log10(m_value);
   }
   m_slider->SetValue(round(value * m_scale));
   event.SetEventType(cEVT_SLIDERTEXT);
   event.Skip();
}

void SliderTextCtrl::OnSlider(wxCommandEvent& event)
{
   m_value = m_slider->GetValue() / m_scale;
   if(m_log)
   {
      if(m_value <= m_zero)
         m_value = 0.0;
      else
      {
         m_value = pow(10.0, m_value);
         m_value = std::max(m_min, m_value);
         m_value = std::min(m_max, m_value);
      }
   }
   m_textbox->ChangeValue(FormatValue());
   m_textbox->SetSelection(-1, -1);
   event.SetEventType(cEVT_SLIDERTEXT);
   event.Skip();
}

void SliderTextCtrl::OnKillFocus(wxFocusEvent& _)
{
   m_textbox->ChangeValue(FormatValue());
   wxCommandEvent event(cEVT_SLIDERTEXT, GetId());
   wxPostEvent(GetParent(), event);
}

wxString SliderTextCtrl::FormatValue() const
{
   int v = m_value;
   if(m_int)
       return wxString::Format(m_format, v);
   else
       return wxString::Format(m_format, m_value);
}

BEGIN_EVENT_TABLE(SliderTextCtrl, wxControl)
   EVT_TEXT(ID_TEXTBOX, SliderTextCtrl::OnTextChange)
   EVT_SLIDER(ID_SLIDER, SliderTextCtrl::OnSlider)
END_EVENT_TABLE()
