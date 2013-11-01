/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffect.cpp

  Dominic Mazzoni
  
  This class implements a Ladspa Plug-in effect.

*******************************************************************//**

\class LadspaEffect
\brief An Effect that calls up a LADSPA plug in, i.e. many possible 
effects from this one class.

*//****************************************************************//**

\class LadspaEffectDialog
\brief Dialog used with Effect

*//*******************************************************************/


#include "../../Audacity.h"

#include "ladspa.h"

#include <wx/wxprec.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/tokenzr.h>
#include <wx/intl.h>
#include <wx/scrolwin.h>

#include "../Effect.h"          // Audacity Effect base class
#include "LadspaEffect.h"       // This class's header file
#include "../../Internat.h"

LadspaEffect::LadspaEffect(const LADSPA_Descriptor *data,
                           const std::set<wxString>& categories)
   : mCategories(categories) {
   
   mData = data;
   pluginName = LAT1CTOWX(mData->Name);

   fInBuffer = NULL;
   fOutBuffer = NULL;

   inputs = 0;
   outputs = 0;
   numInputControls = 0;
   mLength = 0;

   unsigned long p;

   inputPorts = new unsigned long [mData->PortCount];
   outputPorts = new unsigned long [mData->PortCount];
   inputControls = new float [mData->PortCount];
   outputControls = new float [mData->PortCount];

   for(p=0; p<mData->PortCount; p++) {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];
      if (LADSPA_IS_PORT_AUDIO(d)) {
         if (LADSPA_IS_PORT_INPUT(d)) {
            inputPorts[inputs] = p;
            inputs++;
         }
         else if (LADSPA_IS_PORT_OUTPUT(d)) {
            outputPorts[outputs] = p;
            outputs++;
         }
      }
      if (LADSPA_IS_PORT_CONTROL(d) &&
          LADSPA_IS_PORT_INPUT(d)) {
         numInputControls++;

         float val = float(1.0);
         LADSPA_PortRangeHint hint = mData->PortRangeHints[p];

         if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor) &&
             val < hint.LowerBound)
            val = hint.LowerBound;

         if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor) &&
             val > hint.UpperBound)
            val = hint.UpperBound;

         if (LADSPA_IS_HINT_DEFAULT_MINIMUM(hint.HintDescriptor))
            val = hint.LowerBound;

         if (LADSPA_IS_HINT_DEFAULT_LOW(hint.HintDescriptor))
            val = hint.LowerBound * 0.75f + hint.UpperBound * 0.25f;

         if (LADSPA_IS_HINT_DEFAULT_MIDDLE(hint.HintDescriptor))
            val = hint.LowerBound * 0.5f + hint.UpperBound * 0.5f;

         if (LADSPA_IS_HINT_DEFAULT_HIGH(hint.HintDescriptor))
            val = hint.LowerBound * 0.25f + hint.UpperBound * 0.75f;

         if (LADSPA_IS_HINT_DEFAULT_MAXIMUM(hint.HintDescriptor))
            val = hint.UpperBound;

         if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor))
            val *= mProjectRate;

         if (LADSPA_IS_HINT_DEFAULT_0(hint.HintDescriptor))
            val = 0.0f;

         if (LADSPA_IS_HINT_DEFAULT_1(hint.HintDescriptor))
            val = 1.0f;

         if (LADSPA_IS_HINT_DEFAULT_100(hint.HintDescriptor))
            val = 100.0f;

         if (LADSPA_IS_HINT_DEFAULT_440(hint.HintDescriptor))
            val = 440.0f;

         inputControls[p] = val;
      }
   }

   int flags = PLUGIN_EFFECT;
   if (inputs == 0)
      flags |= INSERT_EFFECT;
   else if (outputs == 0)
      flags |= ANALYZE_EFFECT;
   else
      flags |= PROCESS_EFFECT;   

   SetEffectFlags(flags);
}

LadspaEffect::~LadspaEffect()
{
   delete[] inputPorts;
   delete[] outputPorts;
   delete[] inputControls;
   delete[] outputControls;
}

wxString LadspaEffect::GetEffectName()
{
   if (numInputControls > 0)
      return pluginName + wxT("...");
   else
      return pluginName;
}

std::set<wxString> LadspaEffect::GetEffectCategories()
{
   return mCategories;
}

wxString LadspaEffect::GetEffectIdentifier()
{
   wxStringTokenizer st(pluginName, wxT(" "));
   wxString id;

   // CamelCase the name
   while (st.HasMoreTokens()) {
      wxString tok = st.GetNextToken();

      id += tok.Left(1).MakeUpper() + tok.Mid(1);
   }

   return id;
}

wxString LadspaEffect::GetEffectAction()
{
   return wxString::Format(_("Performing Effect: %s"), 
                           pluginName.c_str());
}

bool LadspaEffect::Init()
{
   mBlockSize = 0;
   mainRate = 0;

   TrackListOfKindIterator iter(Track::Wave, mTracks);
   Track *left = iter.First();
   while(left) {
      if (mainRate == 0)
         mainRate = (int)(((WaveTrack *)left)->GetRate() + 0.5);
      
      if (left->GetLinked()) {
         Track *right = iter.Next();
         
         if (((WaveTrack *)left)->GetRate() !=
             ((WaveTrack *)right)->GetRate()) {
            wxMessageBox(_("Sorry, Plug-in Effects cannot be performed on stereo tracks where the individual channels of the track do not match."));
            return false;
         }
      }
      
      left = iter.Next();
   }

   if (mainRate<=0)
      mainRate = (int)(mProjectRate + 0.5);

   return true;
}

bool LadspaEffect::PromptUser()
{
   if (numInputControls > 0) {
      double length = mT1 > mT0 ? mT1 - mT0 : sDefaultGenerateLen;

      LadspaEffectDialog dlog(this, mParent, mData, inputControls, mainRate, length);
      dlog.CentreOnParent();
      dlog.ShowModal();
      
      if (!dlog.GetReturnCode())
         return false;

      mLength = dlog.GetLength();
   }
   return true;
}

bool LadspaEffect::Process()
{
   this->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;

   TrackListIterator iter(mOutputTracks);
   int count = 0;
   Track *left = iter.First();
   Track *right;
   while(left) {
      sampleCount lstart = 0, rstart = 0;
      sampleCount len;
      GetSamples((WaveTrack *)left, &lstart, &len);
      
      right = NULL;
      if (left->GetLinked() && inputs>1) {
         right = iter.Next();         
         GetSamples((WaveTrack *)right, &rstart, &len);
      }

      if (inputs < 2 && right) {
         // If the effect is mono, apply to each channel separately

         bGoodResult = ProcessStereo(count, (WaveTrack *)left, NULL,
                                 lstart, 0, len) && 
                        ProcessStereo(count, (WaveTrack *)right, NULL,
                                    rstart, 0, len);
      }
      else bGoodResult = ProcessStereo(count,
                                   (WaveTrack *)left, (WaveTrack *)right,
                                   lstart, rstart, len);
      if (!bGoodResult)
         break;
   
      left = iter.Next();
      count++;
   }

   this->ReplaceProcessedTracks(bGoodResult); 
   return bGoodResult;
}

bool LadspaEffect::ProcessStereo(int count, WaveTrack *left, WaveTrack *right,
                                 sampleCount lstart, 
                                 sampleCount rstart,
                                 sampleCount len)
{
   /* Allocate buffers */
   if (mBlockSize == 0) {
      mBlockSize = left->GetMaxBlockSize() * 2;

      fInBuffer = new float *[inputs];
      unsigned long i;
      for (i = 0; i < inputs; i++)
         fInBuffer[i] = new float[mBlockSize];
      fOutBuffer = new float *[outputs];
      for (i = 0; i < outputs; i++)
         fOutBuffer[i] = new float[mBlockSize];
   }

   /* Instantiate the plugin */

   unsigned long rate = (unsigned long)(left->GetRate() + 0.5);
   LADSPA_Handle handle = mData->instantiate(mData, rate);

   unsigned long p;
   for(p=0; p<inputs; p++) {
      mData->connect_port(handle, inputPorts[p], fInBuffer[p]);
   }
   for(p=0; p<outputs; p++) {
      mData->connect_port(handle, outputPorts[p], fOutBuffer[p]);
   }

   for(p=0; p<mData->PortCount; p++) {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];
      if (LADSPA_IS_PORT_CONTROL(d)) {
         if (LADSPA_IS_PORT_INPUT(d)) {
            mData->connect_port(handle, p, &inputControls[p]);
         }
         else
            mData->connect_port(handle, p, &outputControls[p]);
      }
   }
   
   if (mData->activate)
      mData->activate(handle);

   // Actually perform the effect here

   sampleCount originalLen = len;
   sampleCount ls = lstart;
   sampleCount rs = rstart;
   while (len) {
      int block = mBlockSize;
      if (block > len)
         block = len;

      if (left && inputs > 0) {
         left->Get((samplePtr)fInBuffer[0], floatSample, ls, block);
      }
      if (right && inputs > 1) {
         right->Get((samplePtr)fInBuffer[1], floatSample, rs, block);
      }

      mData->run(handle, block);

      if (left && outputs > 0) {
         left->Set((samplePtr)fOutBuffer[0], floatSample, ls, block);
      }      
      
      if (right && outputs > 1) {
         right->Set((samplePtr)fOutBuffer[1], floatSample, rs, block);
      }      

      len -= block;
      ls += block;
      rs += block;
      
      if (inputs > 1) {
         if (TrackGroupProgress(count, (ls-lstart)/(double)originalLen))
            return false;
      }
      else {
         if (TrackProgress(count, (ls-lstart)/(double)originalLen))
            return false;
      }
   }

   if (mData->deactivate)
      mData->deactivate(handle);

   if (mData->cleanup)
      mData->cleanup(handle);

   return true;
}

void LadspaEffect::End()
{
   unsigned long i;

   if (fInBuffer) {
      for (i = 0; i < inputs; i++) {
         if (fInBuffer[i]) {
            delete [] fInBuffer[i];
         }
      }
      delete [] fInBuffer;
      fInBuffer = NULL;
   }

   if (fOutBuffer) {
      for (i = 0; i < outputs; i++) {
         if (fOutBuffer[i]) {
            delete [] fOutBuffer[i];
         }
      }
      delete [] fOutBuffer;
      fOutBuffer = NULL;
   }
}

class Slider:public wxSlider
{
 public:
   Slider(wxWindow *parent, wxWindowID id,
          int value, int minValue, int maxValue,
          const wxPoint& pos = wxDefaultPosition,
          const wxSize& size = wxDefaultSize,
          long style = wxSL_HORIZONTAL,
          const wxValidator& validator = wxDefaultValidator,
          const wxString& name = wxSliderNameStr)
   : wxSlider(parent, id, value, minValue, maxValue,
              pos, size, style, validator, name)
   {
   };

   void OnSetFocus(wxFocusEvent &event)
   {
      wxScrolledWindow *p = (wxScrolledWindow *) GetParent();
      wxRect r = GetRect();
      wxRect rv = p->GetRect();
      rv.y = 0;

      event.Skip();

      int y;
      int yppu;
      p->GetScrollPixelsPerUnit(NULL, &yppu);

      if (r.y >= rv.y && r.GetBottom() <= rv.GetBottom()) {
         return;
      }

      if (r.y < rv.y) {
         p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
         y = r.y / yppu;
      }
      else {
         p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
         y = (r.GetBottom() - rv.GetBottom() + yppu) / yppu;
      }

      p->Scroll(-1, y);
   };

   DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(Slider, wxSlider)
    EVT_SET_FOCUS(Slider::OnSetFocus)
END_EVENT_TABLE()

class TextCtrl:public wxTextCtrl
{
 public:
   TextCtrl(wxWindow *parent, wxWindowID id,
            const wxString& value = wxEmptyString,
            const wxPoint& pos = wxDefaultPosition,
            const wxSize& size = wxDefaultSize, long style = 0,
            const wxValidator& validator = wxDefaultValidator,
            const wxString& name = wxTextCtrlNameStr)
   : wxTextCtrl(parent, id, value,
                pos, size, style, validator, name)
   {
   };

   void OnSetFocus(wxFocusEvent &event)
   {
      wxScrolledWindow *p = (wxScrolledWindow *) GetParent();
      wxRect r = GetRect();
      wxRect rv = p->GetRect();
      rv.y = 0;

      event.Skip();

      int y;
      int yppu;
      p->GetScrollPixelsPerUnit(NULL, &yppu);

      if (r.y >= rv.y && r.GetBottom() <= rv.GetBottom()) {
         return;
      }

      if (r.y < rv.y) {
         p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
         y = r.y / yppu;
      }
      else {
         p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
         y = (r.GetBottom() - rv.GetBottom() + yppu) / yppu;
      }

      p->Scroll(-1, y);
   };

   DECLARE_EVENT_TABLE();
};

BEGIN_EVENT_TABLE(TextCtrl, wxTextCtrl)
    EVT_SET_FOCUS(TextCtrl::OnSetFocus)
END_EVENT_TABLE()

const int LADSPA_SECONDS_ID = 13101;

BEGIN_EVENT_TABLE(LadspaEffectDialog, wxDialog)
    EVT_BUTTON(wxID_OK, LadspaEffectDialog::OnOK)
    EVT_BUTTON(wxID_CANCEL, LadspaEffectDialog::OnCancel)
    EVT_BUTTON(ID_EFFECT_PREVIEW, LadspaEffectDialog::OnPreview)
    EVT_SLIDER(wxID_ANY, LadspaEffectDialog::OnSlider)
    EVT_TEXT(wxID_ANY, LadspaEffectDialog::OnTextCtrl)
    EVT_CHECKBOX(wxID_ANY, LadspaEffectDialog::OnCheckBox)
END_EVENT_TABLE()

IMPLEMENT_CLASS(LadspaEffectDialog, wxDialog)

LadspaEffectDialog::LadspaEffectDialog(LadspaEffect *eff,
                                       wxWindow * parent,
                                       const LADSPA_Descriptor *data,
                                       float *inputControls,
                                       int sampleRate,
                                       double length)
   :wxDialog(parent, -1, LAT1CTOWX(data->Name),
             wxDefaultPosition, wxDefaultSize,
             wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
    effect(eff)
{
   mLength = length;
   numParams = 0;
   this->mData = data;
   this->inputControls = inputControls;
   this->sampleRate = sampleRate;
   #ifdef __WXMSW__
      // On Windows, for some reason, wxWidgets calls OnTextCtrl during creation
      // of the text control, and LadspaEffectDialog::OnTextCtrl calls HandleText,
      // which assumes all the fields have been initialized.
      // This can give us a bad pointer crash, so manipulate inSlider to
      // no-op HandleText during creation.
      inSlider = true;
   #else
      inSlider = false;
   #endif
   inText = false;

   toggles = new wxCheckBox*[mData->PortCount];
   sliders = new wxSlider*[mData->PortCount];
   fields = new wxTextCtrl*[mData->PortCount];
   labels = new wxStaticText*[mData->PortCount];
   ports = new unsigned long [mData->PortCount];

   unsigned long p;
   for(p=0; p<mData->PortCount; p++) {
      LADSPA_PortDescriptor d = mData->PortDescriptors[p];
      if (LADSPA_IS_PORT_CONTROL(d) &&
          LADSPA_IS_PORT_INPUT(d)) {
         ports[numParams] = p;
         numParams++;
      }
   }

   wxControl *item;

   wxBoxSizer *vSizer = new wxBoxSizer(wxVERTICAL);

   if (mData->Maker &&
       mData->Maker[0] && 
       LAT1CTOWX(mData->Maker) != wxString(_("None"))) {       
      item = new wxStaticText(this, 0,
                              wxString(_("Author: "))+LAT1CTOWX(mData->Maker));
      vSizer->Add(item, 0, wxALL, 5);
   }
   
   if (mData->Copyright &&
       mData->Copyright[0] && 
       LAT1CTOWX(mData->Copyright) != wxString(_("None"))) {
      
      item = new wxStaticText(this, 0,
                              LAT1CTOWX(mData->Copyright));
      vSizer->Add(item, 0, wxALL, 5);
   }

   wxScrolledWindow *w = new wxScrolledWindow(this,
                                              wxID_ANY,
                                              wxDefaultPosition,
                                              wxDefaultSize,
                                              wxVSCROLL | wxTAB_TRAVERSAL);

   // Try to give the window a sensible default/minimum size
   w->SetMinSize(wxSize(
      wxMax(600, parent->GetSize().GetWidth() * 2/3),
      parent->GetSize().GetHeight() / 2));
                                              
   w->SetScrollRate(0, 20);
   vSizer->Add(w, 1, wxEXPAND|wxALL, 5);

   // Preview, OK, & Cancel buttons
   vSizer->Add(CreateStdButtonSizer(this, ePreviewButton|eCancelButton|eOkButton), 0, wxEXPAND);

   SetSizer(vSizer);

   wxSizer *paramSizer =
      new wxStaticBoxSizer(wxVERTICAL, w, _("Effect Settings"));

   wxFlexGridSizer *gridSizer =
      new wxFlexGridSizer(5, 0, 0);
   gridSizer->AddGrowableCol(3);

   for (p = 0; p < numParams; p++) {
      wxString labelText = LAT1CTOWX(mData->PortNames[ports[p]]);
      item = new wxStaticText(w, 0, labelText + wxT(":"));
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

      wxString fieldText;
      LADSPA_PortRangeHint hint = mData->PortRangeHints[ports[p]];

      if (LADSPA_IS_HINT_TOGGLED(hint.HintDescriptor)) {
         toggles[p] = new wxCheckBox(w, p, wxT(""));
         toggles[p]->SetName(labelText);
         toggles[p]->SetValue(inputControls[ports[p]] > 0);
         gridSizer->Add(toggles[p], 0, wxALL, 5);
         ConnectFocus(toggles[p]);

         gridSizer->Add(1, 1, 0);
         gridSizer->Add(1, 1, 0);
         gridSizer->Add(1, 1, 0);
      }
      else {
         if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor))
            fieldText.Printf(wxT("%d"), (int)(inputControls[ports[p]] + 0.5));
         else
            fieldText = Internat::ToDisplayString(inputControls[ports[p]]);

         fields[p] = new wxTextCtrl(w, p, fieldText);
         fields[p]->SetName(labelText);
         gridSizer->Add(fields[p], 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
         ConnectFocus(fields[p]);

         wxString bound;
         double lower = 0.0;
         double upper = 0.0;
         bool haslo = false;
         bool hashi = false;
         bool forceint = false;

         if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor)) {
            lower = hint.LowerBound;
            haslo = true;
         }
         if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor)) {
            upper = hint.UpperBound;
            hashi = true;
         }
         if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor)) {
            lower *= sampleRate * 1000;
            upper *= sampleRate;
            forceint = true;
         }

         wxString str;
         if (haslo) {
            if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint)
               str.Printf(wxT("%d"), (int)(lower + 0.5));
            else
               str = Internat::ToDisplayString(lower);
            item = new wxStaticText(w, 0, str);
            gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);
         }
         else {
            gridSizer->Add(1, 1, 0);
         }

         sliders[p] =
             new wxSlider(w, p,
                          0, 0, 1000,
                          wxDefaultPosition,
                          wxSize(200, -1));
         sliders[p]->SetName(labelText);
         gridSizer->Add(sliders[p], 0, wxALIGN_CENTER_VERTICAL | wxEXPAND | wxALL, 5);
         ConnectFocus(sliders[p]);

         if (hashi) {
            if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint)
               str.Printf(wxT("%d"), (int)(upper + 0.5));
            else
               str = Internat::ToDisplayString(upper);
            item = new wxStaticText(w, 0, str);
            gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);
         }
         else {
            gridSizer->Add(1, 1, 0);
         }
      }
   }

   // Now add the length control
   if (effect->GetEffectFlags() & INSERT_EFFECT) {
      item = new wxStaticText(w, 0, _("Length (seconds)"));
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
      mSeconds = new wxTextCtrl(w, LADSPA_SECONDS_ID, Internat::ToDisplayString(length));
      mSeconds->SetName(_("Length (seconds)"));
      gridSizer->Add(mSeconds, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);
      ConnectFocus(mSeconds);
   }

   // Set all of the sliders based on the value in the
   // text fields
   inSlider = false; // Now we're ready for HandleText to actually do something.
   HandleText();
   
   paramSizer->Add(gridSizer, 1, wxEXPAND | wxALL, 5);
   w->SetSizer(paramSizer);

   Layout();
   Fit();
   SetSizeHints(GetSize());
}

LadspaEffectDialog::~LadspaEffectDialog()
{
   delete[]toggles;
   delete[]sliders;
   delete[]fields;
   delete[]labels;
   delete[]ports;
}

void LadspaEffectDialog::OnCheckBox(wxCommandEvent &event)
{
   int p = event.GetId();

   inputControls[ports[p]] = toggles[p]->GetValue();
}

void LadspaEffectDialog::OnSlider(wxCommandEvent &event)
{
   int p = event.GetId();

   // if we don't add the following three lines, changing
   // the value of the slider will change the text, which
   // will change the slider, and so on.  This gets rid of
   // the implicit loop.
   if (inText)
      return;
   inSlider = true;

   float val;
   float lower = float(0.0);
   float upper = float(10.0);
   float range;
   bool forceint = false;

   LADSPA_PortRangeHint hint = mData->PortRangeHints[ports[p]];
   if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor))
      lower = hint.LowerBound;
   if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor))
      upper = hint.UpperBound;
   if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor)) {
      lower *= sampleRate;
      upper *= sampleRate;
      forceint = true;
   }

   range = upper - lower;

   val = (sliders[p]->GetValue() / 1000.0) * range + lower;

   wxString str;
   if (LADSPA_IS_HINT_INTEGER(hint.HintDescriptor) || forceint)
      str.Printf(wxT("%d"), (int)(val + 0.5));
   else
      str = Internat::ToDisplayString(val);

   fields[p]->SetValue(str);

   inputControls[ports[p]] = val;

   inSlider = false;
}

void LadspaEffectDialog::OnTextCtrl(wxCommandEvent & WXUNUSED(event))
{
   HandleText();
}

void LadspaEffectDialog::HandleText()
{
   // if we don't add the following three lines, changing
   // the value of the slider will change the text, which
   // will change the slider, and so on.  This gets rid of
   // the implicit loop.

   if (inSlider)
      return;
   inText = true;
   for (unsigned long p = 0; p < numParams; p++) {
      double dval;
      float val;
      float lower = float(0.0);
      float upper = float(10.0);
      float range;

      LADSPA_PortRangeHint hint = mData->PortRangeHints[ports[p]];
      if (LADSPA_IS_HINT_TOGGLED(hint.HintDescriptor)) {
         continue;
      }

      dval = Internat::CompatibleToDouble(fields[p]->GetValue());
      val = dval;

      if (LADSPA_IS_HINT_BOUNDED_BELOW(hint.HintDescriptor))
         lower = hint.LowerBound;
      if (LADSPA_IS_HINT_BOUNDED_ABOVE(hint.HintDescriptor))
         upper = hint.UpperBound;      
      if (LADSPA_IS_HINT_SAMPLE_RATE(hint.HintDescriptor)) {
         lower *= sampleRate;
         upper *= sampleRate;
      }         
      range = upper - lower;

      if (val < lower)
         val = lower;
      if (val > upper)
         val = upper;

      inputControls[ports[p]] = val;

      sliders[p]->SetValue((int)(((val-lower)/range) * 1000.0 + 0.5));      
   }

   inText = false;
}

void LadspaEffectDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   EndModal(TRUE);
}

void LadspaEffectDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   EndModal(FALSE);
}

void LadspaEffectDialog::OnPreview(wxCommandEvent & WXUNUSED(event))
{
   effect->Preview();
}

void LadspaEffectDialog::ConnectFocus(wxControl *c)
{
   c->GetEventHandler()->Connect(wxEVT_SET_FOCUS,
                                 wxFocusEventHandler(LadspaEffectDialog::ControlSetFocus));
}

void LadspaEffectDialog::DisconnectFocus(wxControl *c)
{
   c->GetEventHandler()->Disconnect(wxEVT_SET_FOCUS,
                                 wxFocusEventHandler(LadspaEffectDialog::ControlSetFocus));
}

void LadspaEffectDialog::ControlSetFocus(wxFocusEvent &event)
{
   wxControl *c = (wxControl *) event.GetEventObject();
   wxScrolledWindow *p = (wxScrolledWindow *) c->GetParent();
   wxRect r = c->GetRect();
   wxRect rv = p->GetRect();
   rv.y = 0;

   event.Skip();

   int y;
   int yppu;
   p->GetScrollPixelsPerUnit(NULL, &yppu);

   if (r.y >= rv.y && r.GetBottom() <= rv.GetBottom()) {
      return;
   }

   if (r.y < rv.y) {
      p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
      y = r.y / yppu;
   }
   else {
      p->CalcUnscrolledPosition(0, r.y, NULL, &r.y);
      y = (r.GetBottom() - rv.GetBottom() + yppu) / yppu;
   }

   p->Scroll(-1, y);
};

double LadspaEffectDialog::GetLength()
{
   if (effect->GetEffectFlags() & INSERT_EFFECT) {
      mLength = Internat::CompatibleToDouble(mSeconds->GetValue());
   }

   return mLength;
}
