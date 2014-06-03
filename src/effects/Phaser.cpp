/**********************************************************************

  Audacity: A Digital Audio Editor

  Phaser.cpp

  Effect programming:
  Nasca Octavian Paul (Paul Nasca)

  UI programming:
  Dominic Mazzoni (with the help of wxDesigner)
  Vaughan Johnson (Preview)

*******************************************************************//**

\class EffectPhaser
\brief An EffectSimpleMono

*//****************************************************************//**

\class PhaserDialog
\brief Dialog for EffectPhaser

*//*******************************************************************/


#include "../Audacity.h"

#include "Phaser.h"

#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../FFT.h"

#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>
#include <wx/spinctrl.h>
#include <wx/stattext.h>
#include <wx/valtext.h>

#include <math.h>

//
// EffectPhaser
//


#define phaserlfoshape 4.0

// How many samples are processed before compute the lfo value again
#define lfoskipsamples 20

EffectPhaser::EffectPhaser()
{
   freq = (float)0.4;
   depth = 100;
   startphase = float(0.0);
   stages = 2;
   drywet = 128;
   fb = float(0.0);
}

wxString EffectPhaser::GetEffectDescription() {
   // Note: This is useful only after values have been set.
   return wxString::Format(_("Applied effect: %s %d stages, %.0f%% wet, frequency = %.1f Hz, start phase = %.0f deg, depth = %d, feedback = %.0f%%"),
                           this->GetEffectName().c_str(),
                           stages,
                           float(drywet*100/255),
                           freq,
                           (startphase * 180 / M_PI),
                           depth,
                           fb);
}

bool EffectPhaser::PromptUser()
{
   PhaserDialog dlog(this, mParent);

   dlog.freq = freq;
   dlog.startphase = startphase * 180 / M_PI;
   dlog.fb = fb;
   dlog.depth = depth;
   dlog.stages = stages;
   dlog.drywet = drywet;

   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

  if (dlog.GetReturnCode() == wxID_CANCEL)
      return false;

   freq = dlog.freq;
   startphase = dlog.startphase * M_PI / 180;
   fb = dlog.fb;
   depth = dlog.depth;
   stages = dlog.stages;
   drywet = dlog.drywet;

   return true;
}

bool EffectPhaser::TransferParameters( Shuttle & shuttle )
{
   shuttle.TransferInt(wxT("Stages"),stages,2);
   shuttle.TransferInt(wxT("Wet"),drywet,128);
   shuttle.TransferFloat(wxT("Freq"),freq,0.4f);
   shuttle.TransferInt(wxT("Depth"),depth,100);
   shuttle.TransferFloat(wxT("Feedback"),fb,0.0f);
   return true;
}

bool EffectPhaser::NewTrackSimpleMono()
{
   for (int j = 0; j < stages; j++)
      old[j] = 0;

   skipcount = 0;
   gain = 0;
   fbout = 0;
   lfoskip = freq * 2 * M_PI / mCurRate;

   phase = startphase;
   if (mCurChannel == Track::RightChannel)
      phase += (float)M_PI;

   return true;
}

bool EffectPhaser::ProcessSimpleMono(float *buffer, sampleCount len)
{
   float m, tmp, in, out;
   int i, j;

   for (i = 0; i < len; i++) {
      in = buffer[i];

      m = in + fbout * fb / 100;
      if (((skipcount++) % lfoskipsamples) == 0) {
         //compute sine between 0 and 1
         gain = (1 + cos(skipcount * lfoskip + phase)) / 2;

         // change lfo shape
         gain =
            (exp(gain * phaserlfoshape) - 1) / (exp(phaserlfoshape)-1);

         gain = 1 - gain / 255 * depth;      // attenuate the lfo
      }
      // phasing routine
      for (j = 0; j < stages; j++) {
         tmp = old[j];
         old[j] = gain * tmp + m;
         m = tmp - gain * old[j];
      }
      fbout = m;
      out = (m * drywet + in * (255 - drywet)) / 255;

      // Prevents clipping
      // Commented out, per http://bugzilla.audacityteam.org/show_bug.cgi?id=690.
      //if (out < -1.0)
      //   out = float(-1.0);
      //else if (out > 1.0)
      //   out = float(1.0);

      buffer[i] = out;
   }

   return true;
}

// WDR: class implementations

//----------------------------------------------------------------------------
// PhaserDialog
//----------------------------------------------------------------------------

#define FREQ_MIN 1
#define FREQ_MAX 40
#define PHASE_MIN 0
#define PHASE_MAX 359
#define DEPTH_MIN 0
#define DEPTH_MAX 255
#define STAGES_MIN 2
#define STAGES_MAX 24
#define DRYWET_MIN 0
#define DRYWET_MAX 255
#define FB_MIN -100
#define FB_MAX 100

// WDR: event table for PhaserDialog

BEGIN_EVENT_TABLE(PhaserDialog, EffectDialog)
    EVT_TEXT(ID_PHASER_STAGESTEXT, PhaserDialog::OnStagesText)
    EVT_TEXT(ID_PHASER_DRYWETTEXT, PhaserDialog::OnDryWetText)
    EVT_TEXT(ID_PHASER_FREQTEXT, PhaserDialog::OnFreqText)
    EVT_TEXT(ID_PHASER_PHASETEXT, PhaserDialog::OnPhaseText)
    EVT_TEXT(ID_PHASER_DEPTHTEXT, PhaserDialog::OnDepthText)
    EVT_TEXT(ID_PHASER_FEEDBACKTEXT, PhaserDialog::OnFeedbackText)
    EVT_SLIDER(ID_PHASER_STAGESSLIDER, PhaserDialog::OnStagesSlider)
    EVT_SLIDER(ID_PHASER_DRYWETSLIDER, PhaserDialog::OnDryWetSlider)
    EVT_SLIDER(ID_PHASER_FREQSLIDER, PhaserDialog::OnFreqSlider)
    EVT_SLIDER(ID_PHASER_PHASESLIDER, PhaserDialog::OnPhaseSlider)
    EVT_SLIDER(ID_PHASER_DEPTHSLIDER, PhaserDialog::OnDepthSlider)
    EVT_SLIDER(ID_PHASER_FEEDBACKSLIDER, PhaserDialog::OnFeedbackSlider)
    EVT_BUTTON(ID_EFFECT_PREVIEW, PhaserDialog::OnPreview)
END_EVENT_TABLE()

PhaserDialog::PhaserDialog(EffectPhaser * effect, wxWindow * parent)
:  EffectDialog(parent, _("Phaser"), PROCESS_EFFECT),
   mEffect(effect)
{
   Init();
}

void PhaserDialog::PopulateOrExchange(ShuttleGui & S)
{
   wxTextValidator vld(wxFILTER_NUMERIC);

   S.SetBorder(5);

   S.StartMultiColumn(3, wxEXPAND);
   {
      wxSlider *s;
      wxTextCtrl * tempTC;
      S.SetStretchyCol(1);
      tempTC = S.Id(ID_PHASER_STAGESTEXT).AddTextBox(_("Stages:"), wxT(""), 12);
      S.SetStyle(wxSL_HORIZONTAL);
      tempTC->SetValidator(vld);
      s = S.Id(ID_PHASER_STAGESSLIDER).AddSlider(wxT(""), 2, STAGES_MAX, STAGES_MIN);
      s->SetName(_("Stages"));
#if defined(__WXGTK__)
      s->SetMinSize(wxSize(100, -1));
#endif

      tempTC = S.Id(ID_PHASER_DRYWETTEXT).AddTextBox(_("Dry/Wet:"), wxT(""), 12);
      S.SetStyle(wxSL_HORIZONTAL);
      tempTC->SetValidator(vld);
      s = S.Id(ID_PHASER_DRYWETSLIDER).AddSlider(wxT(""), 0, DRYWET_MAX, DRYWET_MIN);
      s->SetName(_("Dry Wet"));
#if defined(__WXGTK__)
      s->SetMinSize(wxSize(100, -1));
#endif

      tempTC = S.Id(ID_PHASER_FREQTEXT).AddTextBox(_("LFO Frequency (Hz):"), wxT(""), 12);
      S.SetStyle(wxSL_HORIZONTAL);
      tempTC->SetValidator(vld);
      s = S.Id(ID_PHASER_FREQSLIDER).AddSlider(wxT(""), 100, FREQ_MAX, FREQ_MIN);
      s->SetName(_("LFO frequency in hertz"));
#if defined(__WXGTK__)
      s->SetMinSize(wxSize(100, -1));
#endif

      tempTC = S.Id(ID_PHASER_PHASETEXT).AddTextBox(_("LFO Start Phase (deg.):"), wxT(""), 12);
      S.SetStyle(wxSL_HORIZONTAL);
      tempTC->SetValidator(vld);
      s = S.Id(ID_PHASER_PHASESLIDER).AddSlider(wxT(""), 0, PHASE_MAX, PHASE_MIN);
      s->SetName(_("LFO start phase in degrees"));
      s->SetLineSize(10);
#if defined(__WXGTK__)
      s->SetMinSize(wxSize(100, -1));
#endif

      tempTC = S.Id(ID_PHASER_DEPTHTEXT).AddTextBox(_("Depth:"), wxT(""), 12);
      S.SetStyle(wxSL_HORIZONTAL);
      tempTC->SetValidator(vld);
      s = S.Id(ID_PHASER_DEPTHSLIDER).AddSlider(wxT(""), 0, DEPTH_MAX, DEPTH_MIN);
      s->SetName(_("Depth in percent"));
#if defined(__WXGTK__)
      s->SetMinSize(wxSize(100, -1));
#endif

      tempTC = S.Id(ID_PHASER_FEEDBACKTEXT).AddTextBox(_("Feedback (%):"), wxT(""), 12);
      S.SetStyle(wxSL_HORIZONTAL);
      tempTC->SetValidator(vld);
      s = S.Id(ID_PHASER_FEEDBACKSLIDER).AddSlider(wxT(""), 0, FB_MAX, FB_MIN);
      s->SetName(_("Feedback in percent"));
      s->SetLineSize(10);
#if defined(__WXGTK__)
      s->SetMinSize(wxSize(100, -1));
#endif
   }
   S.EndMultiColumn();
}

bool PhaserDialog::TransferDataToWindow()
{
   wxSlider *slider;

   slider = GetFreqSlider();
   if (slider)
      slider->SetValue((int)(freq * 10));

   slider = GetPhaseSlider();
   if (slider)
      slider->SetValue((int)startphase);

   slider = GetDepthSlider();
   if (slider)
      slider->SetValue((int)depth);

   slider = GetFeedbackSlider();
   if (slider)
      slider->SetValue((int)fb);

   slider = GetDryWetSlider();
   if (slider)
      slider->SetValue((int)drywet);

   slider = GetStagesSlider();
   if (slider)
      slider->SetValue((int)stages);

   wxTextCtrl *text;

   text = GetStagesText();
   if (text) {
      wxString str;
      str.Printf(wxT("%d"), stages);
      text->SetValue(str);
   }

   text = GetDryWetText();
   if (text) {
      wxString str;
      str.Printf(wxT("%d"), drywet);
      text->SetValue(str);
   }

   text = GetFreqText();
   if (text) {
      wxString str;
      str.Printf(wxT("%.1f"), freq);
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

   text = GetFeedbackText();
   if (text) {
      wxString str;
      str.Printf(wxT("%d"), (int) fb);
      text->SetValue(str);
   }

   return TRUE;
}

bool PhaserDialog::TransferDataFromWindow()
{
   wxTextCtrl *c;
   long x;

   c = GetFreqText();
   if (c) {
      double d;
      c->GetValue().ToDouble(&d);
      freq = TrapDouble(d * 10, FREQ_MIN, FREQ_MAX) / 10;
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

   c = GetFeedbackText();
   if (c) {
      c->GetValue().ToLong(&x);
      fb = TrapLong(x, FB_MIN, FB_MAX);
   }

   c = GetStagesText();
   if (c) {
      c->GetValue().ToLong(&x);
      stages = TrapLong(x, STAGES_MIN, STAGES_MAX);
      if ((stages % 2) == 1)    // must be even
         stages = TrapLong(stages - 1, STAGES_MIN, STAGES_MAX);
   }

   c = GetDryWetText();
   if (c) {
      c->GetValue().ToLong(&x);
      drywet = TrapLong(x, DRYWET_MIN, DRYWET_MAX);
   }

   return TRUE;
}

// WDR: handler implementations for PhaserDialog

void PhaserDialog::OnStagesSlider(wxCommandEvent & WXUNUSED(event))
{
   wxString str;
   long stage = GetStagesSlider()->GetValue();
   str.Printf(wxT("%ld"), stage);
   GetStagesText()->SetValue(str);
}

void PhaserDialog::OnDryWetSlider(wxCommandEvent & WXUNUSED(event))
{
   wxString str;
   long drywet = GetDryWetSlider()->GetValue();
   str.Printf(wxT("%ld"), drywet);
   GetDryWetText()->SetValue(str);
}

void PhaserDialog::OnFeedbackSlider(wxCommandEvent & WXUNUSED(event))
{
   wxString str;
   long fb = GetFeedbackSlider()->GetValue();
   if (fb > 0)                  // round to nearest multiple of 10
      fb = ((fb + 5) / 10) * 10;
   else
      fb = ((fb - 5) / 10) * 10;
   str.Printf(wxT("%ld"), fb);
   GetFeedbackText()->SetValue(str);
}

void PhaserDialog::OnDepthSlider(wxCommandEvent & WXUNUSED(event))
{
   wxString str;
   long depth = GetDepthSlider()->GetValue();
   str.Printf(wxT("%ld"), depth);
   GetDepthText()->SetValue(str);
}

void PhaserDialog::OnPhaseSlider(wxCommandEvent & WXUNUSED(event))
{
   wxString str;
   long phase = GetPhaseSlider()->GetValue();
   phase = ((phase + 5) / 10) * 10;     // round to nearest multiple of 10
   str.Printf(wxT("%ld"), phase);
   GetPhaseText()->SetValue(str);
}

void PhaserDialog::OnFreqSlider(wxCommandEvent & WXUNUSED(event))
{
   wxString str;
   long freq = GetFreqSlider()->GetValue();
   str.Printf(wxT("%.1f"), freq / 10.0);
   GetFreqText()->SetValue(str);
}

void PhaserDialog::OnStagesText(wxCommandEvent & WXUNUSED(event))
{
   wxTextCtrl *c = GetStagesText();
   if (c) {
      long stage;

      c->GetValue().ToLong(&stage);
      stage = TrapLong(stage, STAGES_MIN, STAGES_MAX);

      wxSlider *slider = GetStagesSlider();
      if (slider)
         slider->SetValue(stage);
   }
}

void PhaserDialog::OnDryWetText(wxCommandEvent & WXUNUSED(event))
{
   wxTextCtrl *c = GetDryWetText();
   if (c) {
      long drywet;

      c->GetValue().ToLong(&drywet);
      drywet = TrapLong(drywet, DRYWET_MIN, DRYWET_MAX);

      wxSlider *slider = GetDryWetSlider();
      if (slider)
         slider->SetValue(drywet);
   }
}

void PhaserDialog::OnFeedbackText(wxCommandEvent & WXUNUSED(event))
{
   wxTextCtrl *c = GetFeedbackText();
   if (c) {
      long fb;

      c->GetValue().ToLong(&fb);
      fb = TrapLong(fb, FB_MIN, FB_MAX);

      wxSlider *slider = GetFeedbackSlider();
      if (slider)
         slider->SetValue(fb);
   }
}

void PhaserDialog::OnDepthText(wxCommandEvent & WXUNUSED(event))
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

void PhaserDialog::OnPhaseText(wxCommandEvent & WXUNUSED(event))
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

void PhaserDialog::OnFreqText(wxCommandEvent & WXUNUSED(event))
{
   wxTextCtrl *c = GetFreqText();
   if (c) {
      double freq;

      c->GetValue().ToDouble(&freq);
      freq = TrapDouble(freq * 10, FREQ_MIN, FREQ_MAX);

      wxSlider *slider = GetFreqSlider();
      if (slider)
         slider->SetValue((int)freq);
   }
}

void PhaserDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   TransferDataFromWindow();

   // Save & restore parameters around Preview, because we didn't do OK.
   float old_freq = mEffect->freq;
   float old_startphase = mEffect->startphase;
   float old_fb = mEffect->fb;
   int old_depth = mEffect->depth;
   int old_stages = mEffect->stages;
   int old_drywet = mEffect->drywet;

   mEffect->freq = freq;
   mEffect->startphase = startphase * M_PI / 180;
   mEffect->fb = fb;
   mEffect->depth = depth;
   mEffect->stages = stages;
   mEffect->drywet = drywet;

   mEffect->Preview();

   mEffect->freq = old_freq;
   mEffect->startphase = old_startphase;
   mEffect->fb = old_fb;
   mEffect->depth = old_depth;
   mEffect->stages = old_stages;
   mEffect->drywet = old_drywet;
}
