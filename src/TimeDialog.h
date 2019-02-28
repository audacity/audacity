/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeDialog.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TimeDialog__
#define __AUDACITY_TimeDialog__

#include <wx/defs.h>
#include <wx/event.h>
#include <wx/string.h>

#include "widgets/wxPanelWrapper.h"
#include "Internat.h"

class NumericTextCtrl;
class ShuttleGui;

class TimeDialog final : public wxDialogWrapper
{
 public:

   TimeDialog(wxWindow *parent,
              const wxString &title,
              const NumericFormatSymbol &format,
              double rate,
              double time,
              const wxString &prompt = _("Duration"));

   void SetFormatString(const NumericFormatSymbol &formatString);
   void SetSampleRate(double sampleRate);
   void SetTimeValue(double newTime);
   const double GetTimeValue();

 private:

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

   void OnUpdate(wxCommandEvent &event);

 private:

   wxString mPrompt;
   NumericFormatSymbol mFormat;
   double mRate;
   double mTime;

   NumericTextCtrl *mTimeCtrl;

   DECLARE_EVENT_TABLE()
};

#endif
