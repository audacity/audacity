/**********************************************************************

   Audacity: A Digital Audio Editor

   SliderTextCtrl.h

   Max Maisel

   This class is a custom slider.

**********************************************************************/

#ifndef __AUDACITY_SLIDERTEXTCTRL__
#define __AUDACITY_SLIDERTEXTCTRL__

#include "wxPanelWrapper.h" // to inherit

class wxSizer;
class wxSlider;
class wxTextCtrl;

wxDECLARE_EVENT(cEVT_SLIDERTEXT, wxCommandEvent);

#define EVT_SLIDERTEXT(winid, func) wx__DECLARE_EVT1( \
   cEVT_SLIDERTEXT, winid, wxCommandEventHandler(func))

class SliderTextCtrl : public wxPanelWrapper
{
   public:
      enum Styles
      {
         HORIZONTAL = 1,
         VERTICAL = 2,
         LOG = 4,
         INT = 8,
      };

      SliderTextCtrl(wxWindow *parent, wxWindowID winid,
         double value, double min, double max, int precision = 2,
         double scale = 0, double offset = 0,
         const wxPoint& pos = wxDefaultPosition,
         const wxSize& size = wxDefaultSize, long style = HORIZONTAL,
         double* varValue = NULL);

      void SetMinTextboxWidth(int width);

      double GetValue() const;
      void SetValue(double value);

   private:
      void OnTextChange(wxCommandEvent& event);
      void OnSlider(wxCommandEvent& event);
      void OnKillFocus(wxFocusEvent& event);
      wxString FormatValue() const;

      enum
      {
         ID_SLIDER = 1,
         ID_TEXTBOX
      };

      wxSizer* m_sizer;
      wxSlider* m_slider;
      wxTextCtrl* m_textbox;

      bool m_log;
      bool m_int;
      double m_value;
      double m_scale;
      double m_min;
      double m_max;
      double m_zero;
      double m_offset;
      wxString m_format;

      DECLARE_EVENT_TABLE()
};

#endif
