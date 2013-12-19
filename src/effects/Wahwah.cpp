/**********************************************************************

  Audacity: A Digital Audio Editor

  Wahwah.cpp

  Effect programming:
  Nasca Octavian Paul (Paul Nasca)

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

*******************************************************************//**

\class EffectWahwah
\brief An EffectSimpleMono

*//****************************************************************//**

\class WahwahDialog
\brief Dialog for EffectWahwah

*//*******************************************************************/



#include "../Audacity.h"

#include "Wahwah.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../FFT.h"

#include <math.h>

#include <wx/intl.h>
#include <wx/button.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>
#include <wx/intl.h>
#include <wx/valtext.h>

//
// EffectWahwah
//

#define lfoskipsamples 30

EffectWahwah::EffectWahwah()
{
   freq = float(1.5);
   startphase = 0;
   depth = (float)0.7;
   freqofs = (float)0.3;
   res = float(2.5);
}

wxString EffectWahwah::GetEffectDescription() { 
   // Note: This is useful only after values have been set. 
   return wxString::Format(_("Applied effect: %s frequency = %.1f Hz, start phase = %.0f deg, depth = %.0f%%, resonance = %.1f, frequency offset = %.0f%%"),
                           this->GetEffectName().c_str(), 
                           freq, 
                           (startphase * 180 / M_PI), 
                           (depth * 100), 
                           res, 
                           (freqofs * 100)); 
} 

bool EffectWahwah::PromptUser()
{
   WahwahDialog dlog(this, mParent);

   dlog.freq = freq;
   dlog.freqoff = freqofs * 100;
   dlog.startphase = startphase * 180 / M_PI;
   dlog.res = res;
   dlog.depth = depth * 100;

   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   freq = dlog.freq;
   freqofs = dlog.freqoff / 100;
   startphase = dlog.startphase * M_PI / 180;
   res = dlog.res;
   depth = dlog.depth / 100;

   return true;
}

bool EffectWahwah::TransferParameters( Shuttle & shuttle )
{  
   shuttle.TransferFloat(wxT("Freq"),freq,1.5f);
   shuttle.TransferFloat(wxT("Phase"),startphase,0.0f);
   shuttle.TransferFloat(wxT("Depth"),depth,0.7f);
   shuttle.TransferFloat(wxT("Resonance"),res,2.5f);
   shuttle.TransferFloat(wxT("Offset"),freqofs,0.3f);
   return true;
}

bool EffectWahwah::NewTrackSimpleMono()
{
   lfoskip = freq * 2 * M_PI / mCurRate;
   skipcount = 0;
   xn1 = 0;
   xn2 = 0;
   yn1 = 0;
   yn2 = 0;
   b0 = 0;
   b1 = 0;
   b2 = 0;
   a0 = 0;
   a1 = 0;
   a2 = 0;

   phase = startphase;
   if (mCurChannel == Track::RightChannel)
      phase += (float)M_PI;

   return true;
}

bool EffectWahwah::ProcessSimpleMono(float *buffer, sampleCount len)
{
   float frequency, omega, sn, cs, alpha;
   float in, out;

   for (int i = 0; i < len; i++) {
      in = buffer[i];
      
      if ((skipcount++) % lfoskipsamples == 0) {
         frequency = (1 + cos(skipcount * lfoskip + phase)) / 2;
         frequency = frequency * depth * (1 - freqofs) + freqofs;
         frequency = exp((frequency - 1) * 6);
         omega = M_PI * frequency;
         sn = sin(omega);
         cs = cos(omega);
         alpha = sn / (2 * res);
         b0 = (1 - cs) / 2;
         b1 = 1 - cs;
         b2 = (1 - cs) / 2;
         a0 = 1 + alpha;
         a1 = -2 * cs;
         a2 = 1 - alpha;
      };
      out = (b0 * in + b1 * xn1 + b2 * xn2 - a1 * yn1 - a2 * yn2) / a0;
      xn2 = xn1;
      xn1 = in;
      yn2 = yn1;
      yn1 = out;
      
      // Prevents clipping
      // Commented out, per http://bugzilla.audacityteam.org/show_bug.cgi?id=689.
      //if (out < -1.0)
      //   out = float(-1.0);
      //else if (out > 1.0)
      //   out = float(1.0);
      
      buffer[i] = (float) out;
   }

   return true;
}

// WDR: class implementations

//----------------------------------------------------------------------------
// WahwahDialog
//----------------------------------------------------------------------------

#define FREQ_MIN 1
#define FREQ_MAX 40
#define FREQOFF_MIN 0
#define FREQOFF_MAX 100
#define PHASE_MIN 0
#define PHASE_MAX 359
#define DEPTH_MIN 0
#define DEPTH_MAX 100
#define RES_MIN 0
#define RES_MAX 100

// WDR: event table for WahwahDialog

BEGIN_EVENT_TABLE(WahwahDialog, EffectDialog)
    EVT_BUTTON(ID_EFFECT_PREVIEW, WahwahDialog::OnPreview)
    EVT_TEXT(ID_FREQTEXT, WahwahDialog::OnFreqText)
    EVT_TEXT(ID_FREQOFFTEXT, WahwahDialog::OnFreqOffText)
    EVT_TEXT(ID_PHASETEXT, WahwahDialog::OnPhaseText)
    EVT_TEXT(ID_DEPTHTEXT, WahwahDialog::OnDepthText)
    EVT_TEXT(ID_RESONANCETEXT, WahwahDialog::OnResonanceText)
    EVT_SLIDER(ID_FREQSLIDER, WahwahDialog::OnFreqSlider)
    EVT_SLIDER(ID_FREQOFFSLIDER, WahwahDialog::OnFreqOffSlider)
    EVT_SLIDER(ID_PHASESLIDER, WahwahDialog::OnPhaseSlider)
    EVT_SLIDER(ID_DEPTHSLIDER, WahwahDialog::OnDepthSlider)
    EVT_SLIDER(ID_RESONANCESLIDER, WahwahDialog::OnResonanceSlider)
END_EVENT_TABLE()

WahwahDialog::WahwahDialog(EffectWahwah * effect, wxWindow * parent)
:  EffectDialog(parent, _("Wahwah"), PROCESS_EFFECT),
   mEffect(effect)
{
   Init();
}

void WahwahDialog::PopulateOrExchange(ShuttleGui & S)
{
   wxTextValidator vld(wxFILTER_NUMERIC);

   S.SetBorder(5);
   S.AddSpace(0, 5);

   S.StartMultiColumn(3, wxCENTER);
   {
      wxSlider *s;
      wxTextCtrl * tempTC;
      tempTC = S.Id(ID_FREQTEXT).AddTextBox(_("LFO Frequency (Hz):"), wxT(""), 12);
      S.SetStyle(wxSL_HORIZONTAL);
      tempTC->SetValidator(vld);
      s = S.Id(ID_FREQSLIDER).AddSlider(wxT(""), 100, FREQ_MAX, FREQ_MIN);
      s->SetName(_("LFO frequency in hertz"));
#if defined(__WXGTK__)
      s->SetMinSize(wxSize(100, -1));
#endif

      tempTC = S.Id(ID_PHASETEXT).AddTextBox(_("LFO Start Phase (deg.):"), wxT(""), 12);
      S.SetStyle(wxSL_HORIZONTAL);
      tempTC->SetValidator(vld);
      s = S.Id(ID_PHASESLIDER).AddSlider(wxT(""), 0, PHASE_MAX, PHASE_MIN);
      s->SetName(_("LFO start phase in degrees"));
      s->SetLineSize(10);
#if defined(__WXGTK__)
      s->SetMinSize(wxSize(100, -1));
#endif

      tempTC = S.Id(ID_DEPTHTEXT).AddTextBox(_("Depth (%):"), wxT(""), 12);
      S.SetStyle(wxSL_HORIZONTAL);
      tempTC->SetValidator(vld);
      s = S.Id(ID_DEPTHSLIDER).AddSlider(wxT(""), 0, DEPTH_MAX, DEPTH_MIN);
      s->SetName(_("Depth in percent"));
#if defined(__WXGTK__)
      s->SetMinSize(wxSize(100, -1));
#endif

      tempTC = S.Id(ID_RESONANCETEXT).AddTextBox(_("Resonance:"), wxT(""), 12);
      S.SetStyle(wxSL_HORIZONTAL);
      tempTC->SetValidator(vld);
      s = S.Id(ID_RESONANCESLIDER).AddSlider(wxT(""), 0, RES_MAX, RES_MIN);
      s->SetName(_("Resonance"));
#if defined(__WXGTK__)
      s->SetMinSize(wxSize(100, -1));
#endif

      tempTC = S.Id(ID_FREQOFFTEXT).AddTextBox(_("Wah Frequency Offset (%):"), wxT(""), 12);
      S.SetStyle(wxSL_HORIZONTAL);
      tempTC->SetValidator(vld);
      s  =S.Id(ID_FREQOFFSLIDER).AddSlider(wxT(""), 0, FREQOFF_MAX, FREQOFF_MIN);
      s->SetName(_("Wah frequency offset in percent"));
#if defined(__WXGTK__)
      s->SetMinSize(wxSize(100, -1));
#endif
   }
   S.EndMultiColumn();
}

bool WahwahDialog::TransferDataToWindow()
{
   wxSlider *slider;

   slider = GetFreqSlider();
   if (slider)
      slider->SetValue((int)(freq * 10));

   slider = GetFreqOffSlider();
   if (slider)
      slider->SetValue((int)freqoff);

   slider = GetDepthSlider();
   if (slider)
      slider->SetValue((int)depth);

   slider = GetPhaseSlider();
   if (slider)
      slider->SetValue((int)startphase);

   slider = GetResonanceSlider();
   if (slider)
      slider->SetValue((int)(res * 10));

   wxTextCtrl *text = GetFreqText();
   if (text) {
      wxString str;
      str.Printf(wxT("%.1f"), freq);
      text->SetValue(str);
   }

   text = GetFreqOffText();
   if (text) {
      wxString str;
      str.Printf(wxT("%d"), (int) freqoff);
      text->SetValue(str);
   }

   text = GetPhaseText();
   if (text) {
      wxString str;
      str.Printf(wxT("%d"), (int) startphase);
      text->SetValue(str);
   }

   text = GetDepthText();
   if (text) {
      wxString str;
      str.Printf(wxT("%d"), (int) depth);
      text->SetValue(str);
   }

   text = GetResonanceText();
   if (text) {
      wxString str;
      str.Printf(wxT("%.1f"), res);
      text->SetValue(str);
   }

   return TRUE;
}

bool WahwahDialog::TransferDataFromWindow()
{
   wxTextCtrl *c;
   long x;

   c = GetFreqText();
   if (c) {
      double d;
      c->GetValue().ToDouble(&d);
      freq = TrapDouble(d * 10, FREQ_MIN, FREQ_MAX) / 10;
   }

   c = GetFreqOffText();
   if (c) {
      double d;
      c->GetValue().ToDouble(&d);
      freqoff = TrapDouble(d, FREQOFF_MIN, FREQOFF_MAX);
   }

   c = GetPhaseText();
   if (c) {
      c->GetValue().ToLong(&x);
      startphase = TrapLong(x, PHASE_MIN, PHASE_MAX);
   }

   c = GetDepthText();
   if (c) {
      c->GetValue().ToLong(&x);
      depth = TrapLong(x, DEPTH_MIN, DEPTH_MAX);
   }

   c = GetResonanceText();
   if (c) {
      double d;
      c->GetValue().ToDouble(&d);
      res = TrapDouble(d * 10, RES_MIN, RES_MAX) / 10;
   }

   return TRUE;
}

// WDR: handler implementations for WahwahDialog

void WahwahDialog::OnResonanceSlider(wxCommandEvent & WXUNUSED(event))
{
   wxString str;
   long res = GetResonanceSlider()->GetValue();
   str.Printf(wxT("%.1f"), res / 10.0);
   GetResonanceText()->SetValue(str);
}

void WahwahDialog::OnDepthSlider(wxCommandEvent & WXUNUSED(event))
{
   wxString str;
   long depth = GetDepthSlider()->GetValue();
   str.Printf(wxT("%ld"), depth);
   GetDepthText()->SetValue(str);
}

void WahwahDialog::OnPhaseSlider(wxCommandEvent & WXUNUSED(event))
{
   wxString str;
   long phase = GetPhaseSlider()->GetValue();
   phase = ((phase + 5) / 10) * 10;     // round to nearest multiple of 10
   str.Printf(wxT("%ld"), phase);
   GetPhaseText()->SetValue(str);
}

void WahwahDialog::OnFreqSlider(wxCommandEvent & WXUNUSED(event))
{
   wxString str;
   long freql = GetFreqSlider()->GetValue();
   str.Printf(wxT("%.1f"), freql / 10.0);
   GetFreqText()->SetValue(str);
}

void WahwahDialog::OnFreqOffSlider(wxCommandEvent & WXUNUSED(event))
{
   wxString str;
   long freqoff = GetFreqOffSlider()->GetValue();
   str.Printf(wxT("%d"), (int) freqoff);
   GetFreqOffText()->SetValue(str);
}

void WahwahDialog::OnResonanceText(wxCommandEvent & WXUNUSED(event))
{
   wxTextCtrl *c = GetResonanceText();
   if (c) {
      double resd;
      c->GetValue().ToDouble(&resd);

      res = resd;
      res = TrapDouble(resd * 10, RES_MIN, RES_MAX) / 10.0;

      wxSlider *slider = GetResonanceSlider();
      if (slider)
         slider->SetValue((int)floor(res * 10 + .5));
   }
}

void WahwahDialog::OnDepthText(wxCommandEvent & WXUNUSED(event))
{
   wxTextCtrl *c = GetDepthText();
   if (c) {
      long depth;

      c->GetValue().ToLong(&depth);
      depth = TrapLong(depth, DEPTH_MIN, DEPTH_MAX);

      wxSlider *slider = GetDepthSlider();
      if (slider)
         slider->SetValue(depth);
   }
}

void WahwahDialog::OnPhaseText(wxCommandEvent & WXUNUSED(event))
{
   wxTextCtrl *c = GetPhaseText();
   if (c) {
      long phase;

      c->GetValue().ToLong(&phase);
      phase = TrapLong(phase, PHASE_MIN, PHASE_MAX);

      wxSlider *slider = GetPhaseSlider();
      if (slider)
         slider->SetValue(phase);
   }
}

void WahwahDialog::OnFreqText(wxCommandEvent & WXUNUSED(event))
{
   wxTextCtrl *c = GetFreqText();
   if (c) {
      long freql;
      double freqd;
      c->GetValue().ToDouble(&freqd);

      freq = freqd;
      freql = TrapLong(((long)floor(freq * 10 + .5)), FREQ_MIN, FREQ_MAX);

      wxSlider *slider = GetFreqSlider();
      if (slider)
         slider->SetValue(freql);
   }
}

void WahwahDialog::OnFreqOffText(wxCommandEvent & WXUNUSED(event))
{
   wxTextCtrl *c = GetFreqOffText();
   if (c) {
      double freqoff;
      c->GetValue().ToDouble(&freqoff);
      freqoff = TrapDouble(freqoff, FREQOFF_MIN, FREQOFF_MAX);

      wxSlider *slider = GetFreqOffSlider();
      if (slider)
         slider->SetValue((int)freqoff);
   }
}

void WahwahDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   TransferDataFromWindow();

   // Save & restore parameters around Preview, because we didn't do OK.
   float old_freq = mEffect->freq;
   float old_freqofs = mEffect->freqofs;
   float old_startphase = mEffect->startphase;
   float old_res = mEffect->res;
   float old_depth = mEffect->depth;
   
   mEffect->freq = freq;
   mEffect->freqofs = freqoff / 100;
   mEffect->startphase = startphase * M_PI / 180;
   mEffect->res = res;
   mEffect->depth = depth / 100;

   mEffect->Preview();

   mEffect->freq = old_freq;
   mEffect->freqofs = old_freqofs;
   mEffect->startphase = old_startphase;
   mEffect->res = old_res;
   mEffect->depth = old_depth;
}


