/**********************************************************************

  Audacity: A Digital Audio Editor

  VampEffect.cpp

  Chris Cannam, with heavy borrowing from LadspaEffect.cpp

  Vamp is an audio analysis and feature extraction plugin API.
  http://www.vamp-plugins.org/

**********************************************************************/

#include "VampEffect.h"

#include <vamp-hostsdk/Plugin.h>
#include <vamp-hostsdk/PluginChannelAdapter.h>
#include <vamp-hostsdk/PluginInputDomainAdapter.h>

#include <wx/wxprec.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/combobox.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/tokenzr.h>
#include <wx/intl.h>
#include <wx/scrolwin.h>

VampEffect::VampEffect(Vamp::HostExt::PluginLoader::PluginKey key,
                       int output,
                       bool hasParameters,
                       wxString name,
                       wxString category) :
   mKey(key),
   mOutput(output),
   mHasParameters(hasParameters),
   mName(name),
   mRate(0),
   mCategory(category),
   mPlugin(NULL)
{
   SetEffectFlags(PLUGIN_EFFECT | ANALYZE_EFFECT);
}

VampEffect::~VampEffect()
{
   delete mPlugin;
   mPlugin = NULL;
}

wxString VampEffect::GetEffectName()
{
   if (mHasParameters) {
      return mName + LAT1CTOWX("...");
   } else {
      return mName;
   }
}

std::set<wxString> VampEffect::GetEffectCategories()
{
   std::set<wxString> result;
   if (mCategory != wxT(""))
      result.insert(mCategory);
   return result;
}


wxString VampEffect::GetEffectIdentifier()
{
   return LAT1CTOWX(mKey.c_str());
}

wxString VampEffect::GetEffectAction()
{
   return wxString::Format(_("Extracting features: %s"),
                           GetEffectName().c_str());
}

bool VampEffect::Init()
{
   Vamp::HostExt::PluginLoader *loader =
      Vamp::HostExt::PluginLoader::getInstance();

   delete mPlugin;
   mPlugin = 0;

   TrackListOfKindIterator iter(Track::Wave, mTracks);
   WaveTrack *left = (WaveTrack *)iter.First();

   mRate = 0.0;

   while (left) {

      if (mRate == 0.0) mRate = left->GetRate();

      if (left->GetLinked()) {

         WaveTrack *right = (WaveTrack *)iter.Next();

         if (left->GetRate() != right->GetRate()) {
            wxMessageBox(_("Sorry, Vamp Plug-ins cannot be run on stereo tracks where the individual channels of the track do not match."));
            return false;
         }
      }

      left = (WaveTrack *)iter.Next();
   }

   if (mRate <= 0.0) mRate = mProjectRate;

   mPlugin = loader->loadPlugin
      (mKey, mRate, Vamp::HostExt::PluginLoader::ADAPT_ALL);

   if (!mPlugin) {
      wxMessageBox(_("Sorry, failed to load Vamp Plug-in."));
      return false;
   }

   return true;
}

bool VampEffect::PromptUser()
{
   if (!mPlugin) return false;
   if (!mHasParameters) return true;

   VampEffectDialog dlog(this, mParent, mPlugin);
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode()) return false;

   return true;
}

bool VampEffect::Process()
{
   if (!mPlugin) return false;

   TrackListOfKindIterator iter(Track::Wave, mTracks);

   int count = 0;

   WaveTrack *left = (WaveTrack *)iter.First();

   bool multiple = false;
   int prevTrackChannels = 0;

   if (GetNumWaveGroups() > 1) {
      // if there is another track beyond this one and any linked one,
      // then we're processing more than one track.  That means we
      // should use the originating track name in each new label
      // track's name, to make clear which is which
      multiple = true;
   }

   while (left) {

      sampleCount lstart, rstart = 0;
      sampleCount len;
      GetSamples(left, &lstart, &len);

      WaveTrack *right = NULL;
      int channels = 1;

      if (left->GetLinked()) {
         right = (WaveTrack *)iter.Next();
         channels = 2;
         GetSamples(right, &rstart, &len);
      }

      size_t step = mPlugin->getPreferredStepSize();
      size_t block = mPlugin->getPreferredBlockSize();

      bool initialiseRequired = true;

      if (block == 0) {
         if (step != 0) block = step;
         else block = 1024;
      }
      if (step == 0) {
         step = block;
      }

      if (prevTrackChannels > 0) {
         // Plugin has already been initialised, so if the number of
         // channels remains the same, we only need to do a reset.
         // Otherwise we need to re-construct the whole plugin,
         // because a Vamp plugin can't be re-initialised.
         if (prevTrackChannels == channels) {
            mPlugin->reset();
            initialiseRequired = false;
         } else {
            //!!! todo: retain parameters previously set
            Init();
         }
      }

      if (initialiseRequired) {
         if (!mPlugin->initialise(channels, step, block)) {
            wxMessageBox(_("Sorry, Vamp Plug-in failed to initialize."));
            return false;
         }
      }

      LabelTrack *ltrack = mFactory->NewLabelTrack();

      if (!multiple) {
         ltrack->SetName(GetEffectName());
      } else {
         ltrack->SetName(wxString::Format(wxT("%s: %s"),
                                          left->GetName().c_str(),
                                          GetEffectName().c_str()));
      }

      mTracks->Add(ltrack);

      float **data = new float*[channels]; // ANSWER-ME: Vigilant Sentry marks this as memory leak, var "data" not deleted.
      for (int c = 0; c < channels; ++c) data[c] = new float[block];

      sampleCount originalLen = len;
      sampleCount ls = lstart;
      sampleCount rs = rstart;

      while (len) {

         int request = block;
         if (request > len) request = len;

         if (left) left->Get((samplePtr)data[0], floatSample, ls, request);
         if (right) right->Get((samplePtr)data[1], floatSample, rs, request);

         if (request < (int)block) {
            for (int c = 0; c < channels; ++c) {
               for (int i = request; i < (int)block; ++i) {
                  data[c][i] = 0.f;
               }
            }
         }

         Vamp::RealTime timestamp = Vamp::RealTime::frame2RealTime
            (ls, (int)(mRate + 0.5));

         Vamp::Plugin::FeatureSet features = mPlugin->process(data, timestamp);
         AddFeatures(ltrack, features);

         if (len > (int)step) len -= step;
         else len = 0;

         ls += step;
         rs += step;

         if (channels > 1) {
            if (TrackGroupProgress(count, (ls - lstart) / double(originalLen)))
               return false;
         } else {
            if (TrackProgress(count, (ls - lstart) / double(originalLen)))
               return false;
         }
      }

      Vamp::Plugin::FeatureSet features = mPlugin->getRemainingFeatures();
      AddFeatures(ltrack, features);

      prevTrackChannels = channels;

      left = (WaveTrack *)iter.Next();
   }

   return true;
}

void VampEffect::AddFeatures(LabelTrack *ltrack,
                             Vamp::Plugin::FeatureSet &features)
{
   for (Vamp::Plugin::FeatureList::iterator fli = features[mOutput].begin();
        fli != features[mOutput].end(); ++fli) {

      Vamp::RealTime ftime0 = fli->timestamp;
      double ltime0 = ftime0.sec + (double(ftime0.nsec) / 1000000000.0);

      Vamp::RealTime ftime1 = ftime0;
      if (fli->hasDuration) ftime1 = ftime0 + fli->duration;
      double ltime1 = ftime1.sec + (double(ftime1.nsec) / 1000000000.0);

      wxString label = LAT1CTOWX(fli->label.c_str());
      if (label == wxString()) {
         if (fli->values.empty()) {
            label = wxString::Format(LAT1CTOWX("%.3f"), ltime0);
         } else {
            label = wxString::Format(LAT1CTOWX("%.3f"), *fli->values.begin());
         }
      }

      ltrack->AddLabel(ltime0, ltime1, label);
   }
}

void VampEffect::End()
{
   delete mPlugin;
   mPlugin = 0;
}


BEGIN_EVENT_TABLE(VampEffectDialog, wxDialog)
    EVT_BUTTON(wxID_OK, VampEffectDialog::OnOK)
    EVT_BUTTON(wxID_CANCEL, VampEffectDialog::OnCancel)
    EVT_SLIDER(wxID_ANY, VampEffectDialog::OnSlider)
    EVT_TEXT(wxID_ANY, VampEffectDialog::OnTextCtrl)
    EVT_CHECKBOX(wxID_ANY, VampEffectDialog::OnCheckBox)
    EVT_COMBOBOX(wxID_ANY, VampEffectDialog::OnComboBox)
END_EVENT_TABLE()

IMPLEMENT_CLASS(VampEffectDialog, wxDialog)

VampEffectDialog::VampEffectDialog(VampEffect *effect,
                                   wxWindow *parent,
                                   Vamp::Plugin *plugin) :
   wxDialog(parent, -1, effect->GetEffectName(),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
   mEffect(effect),
   mPlugin(plugin)
{
   Vamp::Plugin::ProgramList programs = plugin->getPrograms();

   mParameters = plugin->getParameterDescriptors();

#ifdef __WXMSW__
   // On Windows, for some reason, wxWidgets calls OnTextCtrl during creation
   // of the text control, and VampEffectDialog::OnTextCtrl calls HandleText,
   // which assumes all the fields have been initialized.
   // This can give us a bad pointer crash, so manipulate inSlider to
   // no-op HandleText during creation.
   inSlider = true;
#else
   inSlider = false;
#endif

   inText = false;

   int count = mParameters.size();

   toggles = new wxCheckBox*[count];
   sliders = new wxSlider*[count];
   fields = new wxTextCtrl*[count];
   labels = new wxStaticText*[count];
   combos = new wxComboBox*[count];

   wxControl *item;

   wxBoxSizer *vSizer = new wxBoxSizer(wxVERTICAL);

   item = new wxStaticText(this, 0,
                           LAT1CTOWX(plugin->getName().c_str()) +
                           wxString(_(" - Vamp audio analysis plugin")));
   vSizer->Add(item, 0, wxALL, 5);

   item = new wxStaticText(this, 0,
                           LAT1CTOWX(plugin->getDescription().c_str()));
   vSizer->Add(item, 0, wxALL, 5);

   item = new wxStaticText(this, 0,
                           wxString(_("Author: "))
                           + LAT1CTOWX(plugin->getMaker().c_str()));

   vSizer->Add(item, 0, wxALL, 5);

   item = new wxStaticText(this, 0,
                           LAT1CTOWX(plugin->getCopyright().c_str()));
   vSizer->Add(item, 0, wxALL, 5);

   wxScrolledWindow *w = new wxScrolledWindow(this,
                                              wxID_ANY,
                                              wxDefaultPosition,
                                              wxDefaultSize,
                                              wxVSCROLL | wxTAB_TRAVERSAL);

   // Try to give the window a sensible default/minimum size
   w->SetMinSize(wxSize(
      wxMax(400, parent->GetSize().GetWidth() / 2),
      parent->GetSize().GetHeight() / 2));

   w->SetScrollRate(0, 20);
   vSizer->Add(w, 1, wxEXPAND|wxALL, 5);

   vSizer->Add(CreateStdButtonSizer(this, eCancelButton|eOkButton), 0, wxEXPAND);

   SetSizer(vSizer);

   wxSizer *paramSizer =
      new wxStaticBoxSizer(wxVERTICAL, w, _("Plugin Settings"));

   wxFlexGridSizer *gridSizer = new wxFlexGridSizer(5, 0, 0);
   gridSizer->AddGrowableCol(3);

   programCombo = 0;

   if (!programs.empty()) {

      wxArrayString choices;
      wxString currentProgram =
         wxString(mPlugin->getCurrentProgram().c_str(), wxConvISO8859_1);

      for (size_t i = 0; i < programs.size(); ++i) {

         wxString choice = wxString(programs[i].c_str(), wxConvISO8859_1);
         choices.Add(choice);
      }

      gridSizer->Add(new wxStaticText(w, 0, _("Program")),
                     0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

      programCombo = new wxComboBox(w, 9999, currentProgram,
                                    wxDefaultPosition, wxDefaultSize,
                                    choices, wxCB_READONLY);
      programCombo->SetName(_("Program"));

      gridSizer->Add(1, 1, 0);
      gridSizer->Add(1, 1, 0);

      gridSizer->Add(programCombo, 0, wxALIGN_CENTER_VERTICAL | wxEXPAND | wxALL, 5);
      ConnectFocus(programCombo);

      gridSizer->Add(1, 1, 0);
   }

   for (int p = 0; p < count; p++) {

      wxString labelText = LAT1CTOWX(mParameters[p].name.c_str());
      item = new wxStaticText(w, 0, labelText + wxT(":"));
      item->SetName(labelText);
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

      wxString fieldText;

      float value = mPlugin->getParameter(mParameters[p].identifier);

      toggles[p] = 0;
      combos[p] = 0;
      sliders[p] = 0;
      fields[p] = 0;

      if (mParameters[p].isQuantized &&
          mParameters[p].quantizeStep == 1.0 &&
          mParameters[p].minValue == 0.0 &&
          mParameters[p].maxValue == 1.0) {

         toggles[p] = new wxCheckBox(w, p, wxT(""));
         toggles[p]->SetName(labelText);
         toggles[p]->SetValue(value > 0.5);
         gridSizer->Add(toggles[p], 0, wxALL, 5);
         ConnectFocus(toggles[p]);

         gridSizer->Add(1, 1, 0);
         gridSizer->Add(1, 1, 0);
         gridSizer->Add(1, 1, 0);

      } else if (mParameters[p].isQuantized &&
                 mParameters[p].quantizeStep == 1.0 &&
                 !mParameters[p].valueNames.empty()) {

         wxArrayString choices;
         wxString selected;

         for (size_t i = 0; i < mParameters[p].valueNames.size(); ++i) {
            wxString choice = wxString
               (mParameters[p].valueNames[i].c_str(), wxConvISO8859_1);
            if (size_t(value - mParameters[p].minValue + 0.5) == i) {
               selected = choice;
            }
            choices.Add(choice);
         }

         combos[p] = new wxComboBox(w, p, selected,
                                    wxDefaultPosition, wxDefaultSize,
                                    choices, wxCB_READONLY);
         combos[p]->SetName(labelText);

         gridSizer->Add(1, 1, 0);
         gridSizer->Add(1, 1, 0);

         gridSizer->Add(combos[p], 0, wxALIGN_CENTER_VERTICAL | wxEXPAND | wxALL, 5);
         ConnectFocus(combos[p]);

         gridSizer->Add(1, 1, 0);

      } else {

         fieldText = Internat::ToDisplayString(value);

         fields[p] = new wxTextCtrl(w, p, fieldText);
         fields[p]->SetName(labelText);
         gridSizer->Add(fields[p], 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);
         ConnectFocus(fields[p]);

         wxString str = Internat::ToDisplayString(mParameters[p].minValue);
         item = new wxStaticText(w, 0, str);
         gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

         sliders[p] =
             new wxSlider(w, p,
                          0, 0, 1000,
                          wxDefaultPosition,
                          wxSize(100, -1));
         sliders[p]->SetName(labelText);
         gridSizer->Add(sliders[p], 0, wxALIGN_CENTER_VERTICAL | wxEXPAND | wxALL, 5);
         ConnectFocus(sliders[p]);

         str = Internat::ToDisplayString(mParameters[p].maxValue);
         item = new wxStaticText(w, 0, str);
         gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);
      }
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

VampEffectDialog::~VampEffectDialog()
{
   delete[] sliders;
   delete[] fields;
   delete[] labels;
   delete[] toggles;
   delete[] combos;
}

void VampEffectDialog::OnCheckBox(wxCommandEvent &event)
{
   int p = event.GetId();

   mPlugin->setParameter(mParameters[p].identifier, toggles[p]->GetValue());
}

void VampEffectDialog::OnComboBox(wxCommandEvent &event)
{
   int p = event.GetId();

   if (p == 9999) {
      // special value for programs
      Vamp::Plugin::ProgramList programs = mPlugin->getPrograms();
      for (size_t i = 0; i < programs.size(); ++i) {
         if (wxString(programs[i].c_str(), wxConvISO8859_1) ==
             programCombo->GetValue()) {
            mPlugin->selectProgram(programs[i]);
            break;
         }
      }
      UpdateFromPlugin();
      return;
   }

   int value = -1;
   for (size_t i = 0; i < mParameters[p].valueNames.size(); ++i) {
      if (wxString(mParameters[p].valueNames[i].c_str(), wxConvISO8859_1) ==
          combos[p]->GetValue()) {
         value = i;
         break;
      }
   }
   if (value >= 0) {
      mPlugin->setParameter(mParameters[p].identifier, value);
   }
}

void VampEffectDialog::OnSlider(wxCommandEvent &event)
{
   int p = event.GetId();

   // if we don't add the following three lines, changing
   // the value of the slider will change the text, which
   // will change the slider, and so on.  This gets rid of
   // the implicit loop.
   if (inText) return;
   inSlider = true;

   float val;
   float lower = mParameters[p].minValue;
   float upper = mParameters[p].maxValue;
   float range;

   range = upper - lower;

   val = (sliders[p]->GetValue() / 1000.0) * range;

   if (mParameters[p].isQuantized) {

      float qs = mParameters[p].quantizeStep;

      if (qs != 0.0) {
         val = int(val / qs + 0.5) * qs;
      }
   }

   val += lower;

   wxString str = Internat::ToDisplayString(val);

   fields[p]->SetValue(str);

   mPlugin->setParameter(mParameters[p].identifier, val);

   inSlider = false;
}

void VampEffectDialog::OnTextCtrl(wxCommandEvent & WXUNUSED(event))
{
   HandleText();
}

void VampEffectDialog::HandleText()
{
   // if we don't add the following three lines, changing
   // the value of the slider will change the text, which
   // will change the slider, and so on.  This gets rid of
   // the implicit loop.

   if (inSlider) return;
   inText = true;

   for (unsigned long p = 0; p < mParameters.size(); p++) {

      if (mParameters[p].isQuantized &&
          mParameters[p].quantizeStep == 1.0) {

         if (mParameters[p].minValue == 0.0 &&
             mParameters[p].maxValue == 1.0) {
            // toggle, not slider
            continue;
         }

         if (!mParameters[p].valueNames.empty()) {
            // combo, not slider
            continue;
         }
      }

      double dval;
      float val;
      float lower = mParameters[p].minValue;
      float upper = mParameters[p].maxValue;
      float range;

      dval = Internat::CompatibleToDouble(fields[p]->GetValue());
      val = dval;

      range = upper - lower;

      if (val < lower) val = lower;
      if (val > upper) val = upper;

      if (mParameters[p].isQuantized) {

         float qs = mParameters[p].quantizeStep;

         if (qs != 0.0) {
            val = int((val - lower) / qs + 0.5) * qs + lower;
         }
      }

      mPlugin->setParameter(mParameters[p].identifier, val);

      sliders[p]->SetValue((int)(((val-lower)/range) * 1000.0 + 0.5));
   }

   inText = false;
}

void VampEffectDialog::UpdateFromPlugin()
{
   inSlider = true;

   for (unsigned long p = 0; p < mParameters.size(); p++) {

      float value = mPlugin->getParameter(mParameters[p].identifier);

      if (mParameters[p].isQuantized &&
          mParameters[p].quantizeStep == 1.0 &&
          mParameters[p].minValue == 0.0 &&
          mParameters[p].maxValue == 1.0) {

         toggles[p]->SetValue(value > 0.5);

      } else if (mParameters[p].isQuantized &&
                 mParameters[p].quantizeStep == 1.0 &&
                 !mParameters[p].valueNames.empty()) {

         wxString selected;

         for (size_t i = 0; i < mParameters[p].valueNames.size(); ++i) {
            wxString choice = wxString
               (mParameters[p].valueNames[i].c_str(), wxConvISO8859_1);
            if (size_t(value - mParameters[p].minValue + 0.5) == i) {
               selected = choice;
               break;
            }
         }

         combos[p]->SetValue(selected);

      } else {

         fields[p]->SetValue(Internat::ToDisplayString(value));
      }
   }

   inSlider = false;
   HandleText();
}

void VampEffectDialog::OnOK(wxCommandEvent & WXUNUSED(event))
{
   EndModal(TRUE);
}

void VampEffectDialog::OnCancel(wxCommandEvent & WXUNUSED(event))
{
   EndModal(FALSE);
}

void VampEffectDialog::ConnectFocus(wxControl *c)
{
   c->GetEventHandler()->Connect(wxEVT_SET_FOCUS,
                                 wxFocusEventHandler(VampEffectDialog::ControlSetFocus));
}

void VampEffectDialog::DisconnectFocus(wxControl *c)
{
   c->GetEventHandler()->Disconnect(wxEVT_SET_FOCUS,
                                 wxFocusEventHandler(VampEffectDialog::ControlSetFocus));
}

void VampEffectDialog::ControlSetFocus(wxFocusEvent &event)
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
