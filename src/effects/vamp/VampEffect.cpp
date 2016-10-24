/**********************************************************************

  Audacity: A Digital Audio Editor

  VampEffect.cpp

  Chris Cannam, with heavy borrowing from LadspaEffect.cpp

  Vamp is an audio analysis and feature extraction plugin API.
  http://www.vamp-plugins.org/

**********************************************************************/

#include "../../Audacity.h"

#if defined(USE_VAMP)

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
#include <wx/version.h>


#include "../../ShuttleGui.h"
#include "../../widgets/valnum.h"

#include "../../LabelTrack.h"
#include "../../WaveTrack.h"

#ifdef __AUDACITY_OLD_STD__
#include <list>
#endif

enum
{
   ID_Program  =  10000,
   ID_Sliders  =  11000,
   ID_Choices  =  12000,
   ID_Texts    =  13000,
   ID_Toggles  =  14000,
};

///////////////////////////////////////////////////////////////////////////////
//
// VampEffect
//
///////////////////////////////////////////////////////////////////////////////

BEGIN_EVENT_TABLE(VampEffect, wxEvtHandler)
    EVT_SLIDER(wxID_ANY, VampEffect::OnSlider)
    EVT_TEXT(wxID_ANY, VampEffect::OnTextCtrl)
    EVT_CHECKBOX(wxID_ANY, VampEffect::OnCheckBox)
    EVT_CHOICE(wxID_ANY, VampEffect::OnChoice)
END_EVENT_TABLE()

VampEffect::VampEffect(std::unique_ptr<Vamp::Plugin> &&plugin,
                       const wxString & path,
                       int output,
                       bool hasParameters)
:  mPlugin(std::move(plugin)),
   mPath(path),
   mOutput(output),
   mHasParameters(hasParameters),
   mRate(0)
{
   mKey = mPath.BeforeLast(wxT('/')).ToUTF8();
   mName = mPath.AfterLast(wxT('/'));

   mSliders = NULL;
   mFields = NULL;
   mLabels = NULL;
   mToggles = NULL;
   mChoices = NULL;
   mValues = NULL;
}

VampEffect::~VampEffect()
{
   if (mValues)
   {
      delete [] mValues;
   }

   if (mSliders)
   {
      delete [] mSliders;
   }

   if (mFields)
   {
      delete [] mFields;
   }

   if (mLabels)
   {
      delete [] mLabels;
   }

   if (mToggles)
   {
      delete [] mToggles;
   }

   if (mChoices)
   {
      delete [] mChoices;
   }
}

// ============================================================================
// IdentInterface implementation
// ============================================================================

wxString VampEffect::GetPath()
{
   return mPath;
}

wxString VampEffect::GetSymbol()
{
   return mName;
}

wxString VampEffect::GetName()
{
   return GetSymbol();
}

wxString VampEffect::GetVendor()
{
   return wxString::FromUTF8(mPlugin->getMaker().c_str());
}

wxString VampEffect::GetVersion()
{
   return wxString::Format(wxT("%d"), mPlugin->getPluginVersion());
}

wxString VampEffect::GetDescription()
{
   return wxString::FromUTF8(mPlugin->getCopyright().c_str());
}

// ============================================================================
// EffectIdentInterface implementation
// ============================================================================

EffectType VampEffect::GetType()
{
   return EffectTypeAnalyze;
}

wxString VampEffect::GetFamily()
{
   return VAMPEFFECTS_FAMILY;
}

bool VampEffect::IsInteractive()
{
   return mHasParameters;
}

bool VampEffect::IsDefault()
{
   return false;
}


// EffectClientInterface implementation

unsigned VampEffect::GetAudioInCount()
{
   return mPlugin->getMaxChannelCount();
}

bool VampEffect::GetAutomationParameters(EffectAutomationParameters & parms)
{
   for (size_t p = 0, cnt = mParameters.size(); p < cnt; p++)
   {
      wxString key = wxString::FromUTF8(mParameters[p].identifier.c_str());
      float value = mPlugin->getParameter(mParameters[p].identifier);
      float lower = mParameters[p].minValue;
      float upper = mParameters[p].maxValue;

      if (mParameters[p].isQuantized &&
          mParameters[p].quantizeStep == 1.0 &&
          lower == 0.0 &&
          upper == 1.0)
      {
         bool val = value > 0.5;

         parms.Write(key, val);
      }
      else if (mParameters[p].isQuantized &&
               mParameters[p].quantizeStep == 1.0 &&
               !mParameters[p].valueNames.empty())
      {
         wxArrayString choices;
         int val = 0;

         for (size_t i = 0, cnt = mParameters[p].valueNames.size(); i < cnt; i++)
         {
            wxString choice = wxString::FromUTF8(mParameters[p].valueNames[i].c_str());
            if (size_t(value - mParameters[p].minValue + 0.5) == i)
            {
               val = i;
            }
            choices.Add(choice);
         }

         parms.WriteEnum(key, val, choices);
      }
      else
      {
         parms.Write(key, value);
      }
   }

   return true;
}

bool VampEffect::SetAutomationParameters(EffectAutomationParameters & parms)
{
   // First pass verifies values
   for (size_t p = 0, cnt = mParameters.size(); p < cnt; p++)
   {
      wxString key = wxString::FromUTF8(mParameters[p].identifier.c_str());
      float lower = mParameters[p].minValue;
      float upper = mParameters[p].maxValue;
      bool good = false;

      if (mParameters[p].isQuantized &&
          mParameters[p].quantizeStep == 1.0 &&
          lower == 0.0 &&
          upper == 1.0)
      {
         bool val;

         good = parms.Read(key, &val);
      }
      else if (mParameters[p].isQuantized &&
               mParameters[p].quantizeStep == 1.0 &&
               !mParameters[p].valueNames.empty())
      {
         wxArrayString choices;
         int val;

         for (size_t i = 0, cnt = mParameters[p].valueNames.size(); i < cnt; i++)
         {
            wxString choice = wxString::FromUTF8(mParameters[p].valueNames[i].c_str());
            choices.Add(choice);
         }

         good = parms.ReadEnum(key, &val, choices) && val != wxNOT_FOUND;
      }
      else
      {
         double val;

         good = parms.Read(key, &val) && val >= lower && val <= upper;
      }

      if (!good)
      {
         return false;
      }
   }

   // Second pass sets the variables
   for (size_t p = 0, cnt = mParameters.size(); p < cnt; p++)
   {
      wxString key = wxString::FromUTF8(mParameters[p].identifier.c_str());
      float lower = mParameters[p].minValue;
      float upper = mParameters[p].maxValue;

      if (mParameters[p].isQuantized &&
          mParameters[p].quantizeStep == 1.0 &&
          lower == 0.0 &&
          upper == 1.0)
      {
         bool val;

         parms.Read(key, &val);

         mPlugin->setParameter(mParameters[p].identifier, val ? upper : lower);
      }
      else if (mParameters[p].isQuantized &&
               mParameters[p].quantizeStep == 1.0 &&
               !mParameters[p].valueNames.empty())
      {
         wxArrayString choices;
         int val;

         for (size_t i = 0, cnt = mParameters[p].valueNames.size(); i < cnt; i++)
         {
            wxString choice = wxString::FromUTF8(mParameters[p].valueNames[i].c_str());
            choices.Add(choice);
         }

         parms.ReadEnum(key, &val, choices);

         mPlugin->setParameter(mParameters[p].identifier, (float) val);
      }
      else
      {
         double val;

         parms.Read(key, &val);

         if (mParameters[p].isQuantized)
         {
            float qs = mParameters[p].quantizeStep;

            if (qs != 0.0)
            {
               val = (int)((val - lower) / qs + 0.5) * qs + lower;
            }
         }

         mPlugin->setParameter(mParameters[p].identifier, val);
      }
   }

   return true;
}

bool VampEffect::Init()
{
   TrackListOfKindIterator iter(Track::Wave, mTracks);
   WaveTrack *left = (WaveTrack *)iter.First();

   mRate = 0.0;

   while (left)
   {
      if (mRate == 0.0)
      {
         mRate = left->GetRate();
      }

      if (left->GetLinked())
      {
         WaveTrack *right = (WaveTrack *)iter.Next();

         if (left->GetRate() != right->GetRate())
         {
            wxMessageBox(_("Sorry, Vamp Plug-ins cannot be run on stereo tracks where the individual channels of the track do not match."));
            return false;
         }
      }

      left = (WaveTrack *)iter.Next();
   }

   if (mRate <= 0.0)
   {
      mRate = mProjectRate;
   }

   // The plugin must be reloaded to allow changing parameters

   Vamp::HostExt::PluginLoader *loader = Vamp::HostExt::PluginLoader::getInstance();
   mPlugin.reset(loader->loadPlugin(mKey, mRate, Vamp::HostExt::PluginLoader::ADAPT_ALL));
   if (!mPlugin)
   {
      wxMessageBox(_("Sorry, failed to load Vamp Plug-in."));
      return false;
   }

   return true;
}

bool VampEffect::Process()
{
   if (!mPlugin)
   {
      return false;
   }

   TrackListOfKindIterator iter(Track::Wave, mTracks);

   int count = 0;

   WaveTrack *left = (WaveTrack *)iter.First();

   bool multiple = false;
   unsigned prevTrackChannels = 0;

   if (GetNumWaveGroups() > 1)
   {
      // if there is another track beyond this one and any linked one,
      // then we're processing more than one track.  That means we
      // should use the originating track name in each NEW label
      // track's name, to make clear which is which
      multiple = true;
   }

   std::vector<std::shared_ptr<Effect::AddedAnalysisTrack>> addedTracks;

   while (left)
   {
      sampleCount lstart, rstart = 0;
      sampleCount len;
      GetSamples(left, &lstart, &len);

      WaveTrack *right = NULL;
      unsigned channels = 1;

      if (left->GetLinked())
      {
         right = (WaveTrack *)iter.Next();
         channels = 2;
         GetSamples(right, &rstart, &len);
      }

      size_t step = mPlugin->getPreferredStepSize();
      size_t block = mPlugin->getPreferredBlockSize();

      bool initialiseRequired = true;

      if (block == 0)
      {
         if (step != 0)
         {
            block = step;
         }
         else
         {
            block = 1024;
         }
      }

      if (step == 0)
      {
         step = block;
      }

      if (prevTrackChannels > 0)
      {
         // Plugin has already been initialised, so if the number of
         // channels remains the same, we only need to do a reset.
         // Otherwise we need to re-construct the whole plugin,
         // because a Vamp plugin can't be re-initialised.
         if (prevTrackChannels == channels)
         {
            mPlugin->reset();
            initialiseRequired = false;
         }
         else
         {
            //!!! todo: retain parameters previously set
            Init();
         }
      }

      if (initialiseRequired)
      {
         if (!mPlugin->initialise(channels, step, block))
         {
            wxMessageBox(_("Sorry, Vamp Plug-in failed to initialize."));
            return false;
         }
      }

      addedTracks.push_back(AddAnalysisTrack(
         multiple
         ? wxString::Format(wxT("%s: %s"),
            left->GetName().c_str(), GetName().c_str())
         : GetName()
      ));
      LabelTrack *ltrack = addedTracks.back()->get();

      float **data = new float *[channels]; // ANSWER-ME: Vigilant Sentry marks this as memory leak, var "data" not deleted.
      for (int c = 0; c < channels; ++c)
      {
         data[c] = new float[block];
      }

      auto originalLen = len;
      auto ls = lstart;
      auto rs = rstart;

      while (len != 0)
      {
         const auto request = limitSampleBufferSize( block, len );

         if (left)
         {
            left->Get((samplePtr)data[0], floatSample, ls, request);
         }

         if (right)
         {
            right->Get((samplePtr)data[1], floatSample, rs, request);
         }

         if (request < block)
         {
            for (int c = 0; c < channels; ++c)
            {
               for (decltype(block) i = request; i < block; ++i)
               {
                  data[c][i] = 0.f;
               }
            }
         }

         // UNSAFE_SAMPLE_COUNT_TRUNCATION
         // Truncation in case of very long tracks!
         Vamp::RealTime timestamp = Vamp::RealTime::frame2RealTime(
            long( ls.as_long_long() ),
            (int)(mRate + 0.5)
         );

         Vamp::Plugin::FeatureSet features = mPlugin->process(data, timestamp);
         AddFeatures(ltrack, features);

         if (len > (int)step)
         {
            len -= step;
         }
         else
         {
            len = 0;
         }

         ls += step;
         rs += step;

         if (channels > 1)
         {
            if (TrackGroupProgress(count,
                  (ls - lstart).as_double() /
                  originalLen.as_double() ))
            {
               return false;
            }
         }
         else
         {
            if (TrackProgress(count,
                  (ls - lstart).as_double() /
                  originalLen.as_double() ))
            {
               return false;
            }
         }
      }

      Vamp::Plugin::FeatureSet features = mPlugin->getRemainingFeatures();
      AddFeatures(ltrack, features);

      prevTrackChannels = channels;

      left = (WaveTrack *)iter.Next();
   }

   // All completed without cancellation, so commit the addition of tracks now
   for (auto &addedTrack : addedTracks)
      addedTrack->Commit();

   return true;
}

void VampEffect::End()
{
   mPlugin.reset();
}

void VampEffect::PopulateOrExchange(ShuttleGui & S)
{
   Vamp::Plugin::ProgramList programs = mPlugin->getPrograms();

   mParameters = mPlugin->getParameterDescriptors();

   int count = mParameters.size();

   mToggles = new wxCheckBox *[count];
   mSliders = new wxSlider *[count];
   mFields = new wxTextCtrl *[count];
   mLabels = new wxStaticText *[count];
   mChoices = new wxChoice *[count];
   mValues = new float[count];

   S.SetStyle(wxVSCROLL | wxTAB_TRAVERSAL);
   wxScrolledWindow *scroller = S.StartScroller(2);
   {
      S.StartStatic(_("Plugin Settings"));
      {
         S.StartMultiColumn(5, wxEXPAND);
         {
            S.SetStretchyCol(3);

            if (!programs.empty())
            {
               wxString currentProgram =  wxString::FromUTF8(mPlugin->getCurrentProgram().c_str());

               wxArrayString choices;
               for (size_t i = 0, cnt = programs.size(); i < cnt; i++)
               {
                  choices.Add(wxString::FromUTF8(programs[i].c_str()));
               }

               S.AddPrompt(_("Program"));

               S.Id(ID_Program);
               mProgram = S.AddChoice(wxT(""), currentProgram, &choices);
               mProgram->SetName(_("Program"));
               mProgram->SetSizeHints(-1, -1);
               wxSizer *s = mProgram->GetContainingSizer();
               s->GetItem(mProgram)->SetFlag(wxEXPAND | wxALIGN_CENTER_VERTICAL | wxALL);

               S.AddSpace(1, 1);
               S.AddSpace(1, 1);
               S.AddSpace(1, 1);
            }

            for (int p = 0; p < count; p++)
            {
	            wxString tip = wxString::FromUTF8(mParameters[p].description.c_str());
	            wxString unit = wxString::FromUTF8(mParameters[p].unit.c_str());

               float value = mPlugin->getParameter(mParameters[p].identifier);

               mToggles[p] = NULL;
               mChoices[p] = NULL;
               mSliders[p] = NULL;
               mFields[p] = NULL;
               mValues[p] = 0.0;

               wxString labelText = wxString::FromUTF8(mParameters[p].name.c_str());
               if (!unit.IsEmpty())
               {
                  labelText += wxT(" (") + unit + wxT(")");
               }
               S.AddPrompt(labelText + wxT(":"));

               if (mParameters[p].isQuantized &&
                   mParameters[p].quantizeStep == 1.0 &&
                   mParameters[p].minValue == 0.0 &&
                   mParameters[p].maxValue == 1.0)
               {
                  S.Id(ID_Toggles + p);
                  mToggles[p] = S.AddCheckBox(wxT(""),
                                              value > 0.5 ? wxT("true") : wxT("false"));
                  mToggles[p]->SetName(labelText);
                  if (!tip.IsEmpty())
                  {
                     mToggles[p]->SetToolTip(tip);
                  }
                  wxSizer *s = mToggles[p]->GetContainingSizer();
                  s->GetItem(mToggles[p])->SetFlag(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL | wxALL);

                  S.AddSpace(1, 1);
                  S.AddSpace(1, 1);
                  S.AddSpace(1, 1);

               }
               else if (mParameters[p].isQuantized &&
                        mParameters[p].quantizeStep == 1.0 &&
                        !mParameters[p].valueNames.empty())
               {
                  wxArrayString choices;
                  wxString selected;

                  for (size_t i = 0, cnt = mParameters[p].valueNames.size(); i < cnt; i++)
                  {
                     wxString choice = wxString::FromUTF8(mParameters[p].valueNames[i].c_str());
                     if (size_t(value - mParameters[p].minValue + 0.5) == i)
                     {
                        selected = choice;
                     }
                     choices.Add(choice);
                  }

                  S.Id(ID_Choices + p);
                  mChoices[p] = S.AddChoice(wxT(""), selected, &choices);
                  mChoices[p]->SetName(labelText);
                  mChoices[p]->SetSizeHints(-1, -1);
                  if (!tip.IsEmpty())
                  {
                     mChoices[p]->SetToolTip(tip);
                  }
                  wxSizer *s = mChoices[p]->GetContainingSizer();
                  s->GetItem(mChoices[p])->SetFlag(wxEXPAND | wxALIGN_CENTER_VERTICAL | wxALL);

                  S.AddSpace(1, 1);
                  S.AddSpace(1, 1);
                  S.AddSpace(1, 1);
               }
               else
               {
                  mValues[p] = value;
                  FloatingPointValidator<float> vld(6, &mValues[p]);
                  vld.SetRange(mParameters[p].minValue, mParameters[p].maxValue);

                  float range = mParameters[p].maxValue - mParameters[p].minValue;
                  int style = range < 10 ? NUM_VAL_THREE_TRAILING_ZEROES :
                              range < 100 ? NUM_VAL_TWO_TRAILING_ZEROES :
                              NUM_VAL_ONE_TRAILING_ZERO;
                  vld.SetStyle(style);

                  S.Id(ID_Texts + p);
                  mFields[p] = S.AddTextBox(wxT(""), wxT(""), 12);
                  mFields[p]->SetName(labelText);
                  mFields[p]->SetValidator(vld);
                  if (!tip.IsEmpty())
                  {
                     mFields[p]->SetToolTip(tip);
                  }
                  wxSizer *s = mFields[p]->GetContainingSizer();
                  s->GetItem(mFields[p])->SetFlag(wxALIGN_CENTER_VERTICAL | wxALL);

                  wxString str = Internat::ToDisplayString(mParameters[p].minValue);
                  S.AddPrompt(str);

                  S.SetStyle(wxSL_HORIZONTAL);
                  S.Id(ID_Sliders + p);
                  mSliders[p] = S.AddSlider(wxT(""), 0, 1000, 0);
                  mSliders[p]->SetName(labelText);
                  mSliders[p]->SetSizeHints(150, -1);
                  if (!tip.IsEmpty())
                  {
                     mSliders[p]->SetToolTip(tip);
                  }
                  
                  str = Internat::ToDisplayString(mParameters[p].maxValue);
                  S.AddUnits(str);
               }
            }
         }
         S.EndMultiColumn();
      }
      S.EndStatic();
   }
   S.EndScroller();

   scroller->SetScrollRate(0, 20);

   return;
}

bool VampEffect::TransferDataToWindow()
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   UpdateFromPlugin();

   return true;
}

bool VampEffect::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   return true;
}

// VampEffect implementation

void VampEffect::AddFeatures(LabelTrack *ltrack,
                             Vamp::Plugin::FeatureSet &features)
{
   for (Vamp::Plugin::FeatureList::iterator fli = features[mOutput].begin();
        fli != features[mOutput].end(); ++fli)
   {
      Vamp::RealTime ftime0 = fli->timestamp;
      double ltime0 = ftime0.sec + (double(ftime0.nsec) / 1000000000.0);

      Vamp::RealTime ftime1 = ftime0;
      if (fli->hasDuration) ftime1 = ftime0 + fli->duration;
      double ltime1 = ftime1.sec + (double(ftime1.nsec) / 1000000000.0);

      wxString label = LAT1CTOWX(fli->label.c_str());
      if (label == wxString())
      {
         if (fli->values.empty())
         {
            label = wxString::Format(LAT1CTOWX("%.3f"), ltime0);
         }
         else
         {
            label = wxString::Format(LAT1CTOWX("%.3f"), *fli->values.begin());
         }
      }

      ltrack->AddLabel(SelectedRegion(ltime0, ltime1), label);
   }
}

void VampEffect::UpdateFromPlugin()
{
   for (size_t p = 0, cnt = mParameters.size(); p < cnt; p++)
   {
      float value = mPlugin->getParameter(mParameters[p].identifier);

      if (mParameters[p].isQuantized &&
          mParameters[p].quantizeStep == 1.0 &&
          mParameters[p].minValue == 0.0 &&
          mParameters[p].maxValue == 1.0)
      {
         mToggles[p]->SetValue(value > 0.5);
      }
      else if (mParameters[p].isQuantized &&
               mParameters[p].quantizeStep == 1.0 &&
               !mParameters[p].valueNames.empty())
      {
         mChoices[p]->SetSelection(size_t(value - mParameters[p].minValue + 0.5));
      }
      else
      {
         mValues[p] = value;
         mFields[p]->GetValidator()->TransferToWindow();

         float lower = mParameters[p].minValue;
         float upper = mParameters[p].maxValue;
         float range = upper - lower;

         if (mParameters[p].isQuantized)
         {
            float qs = mParameters[p].quantizeStep;

            if (qs != 0.0)
            {
               value = (int)((value - lower) / qs + 0.5) * qs + lower;
            }
         }

         mSliders[p]->SetValue((int)(((value - lower) / range) * 1000.0 + 0.5));
      }
   }
}

void VampEffect::OnCheckBox(wxCommandEvent &event)
{
   int p = event.GetId() - ID_Toggles;

   mPlugin->setParameter(mParameters[p].identifier, mToggles[p]->GetValue());
}

void VampEffect::OnChoice(wxCommandEvent & evt)
{
   int p = evt.GetId();

   // special value for programs
   if (p == ID_Program)
   {
      Vamp::Plugin::ProgramList programs = mPlugin->getPrograms();
      mPlugin->selectProgram(programs[evt.GetInt()]);
      UpdateFromPlugin();
      return;
   }

   mPlugin->setParameter(mParameters[p - ID_Choices].identifier, evt.GetInt());
}

void VampEffect::OnSlider(wxCommandEvent & evt)
{
   int p = evt.GetId() - ID_Sliders;

   float lower = mParameters[p].minValue;
   float upper = mParameters[p].maxValue;
   float range = upper - lower;
   float val = (evt.GetInt() / 1000.0) * range;

   if (mParameters[p].isQuantized)
   {
      float qs = mParameters[p].quantizeStep;

      if (qs != 0.0)
      {
         val = (int)(val / qs + 0.5) * qs;
      }
   }

   val += lower;

   mValues[p] = val;
   mFields[p]->GetValidator()->TransferToWindow();

   mPlugin->setParameter(mParameters[p].identifier, val);
}

void VampEffect::OnTextCtrl(wxCommandEvent & evt)
{
   int p = evt.GetId() - ID_Texts;

   mFields[p]->GetValidator()->TransferFromWindow();

   float lower = mParameters[p].minValue;
   float upper = mParameters[p].maxValue;
   float range = upper - lower;
   float val = mValues[p];

   if (mParameters[p].isQuantized)
   {
      float qs = mParameters[p].quantizeStep;

      if (qs != 0.0)
      {
         val = (int)((val - lower) / qs + 0.5) * qs + lower;
      }
   }

   mPlugin->setParameter(mParameters[p].identifier, val);

   mSliders[p]->SetValue((int)(((val - lower) / range) * 1000.0 + 0.5));
}

#endif