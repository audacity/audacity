/**********************************************************************

  Audacity: A Digital Audio Editor

  VampEffect.cpp

  Chris Cannam, with heavy borrowing from LadspaEffect.cpp

  Vamp is an audio analysis and feature extraction plugin API.
  http://www.vamp-plugins.org/

**********************************************************************/



#if defined(USE_VAMP)
#include "VampEffect.h"

#include <vamp-hostsdk/Plugin.h>
#include <vamp-hostsdk/PluginChannelAdapter.h>
#include <vamp-hostsdk/PluginInputDomainAdapter.h>

#include <wx/wxprec.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/combobox.h>
#include <wx/slider.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/scrolwin.h>
#include <wx/version.h>


#include "ShuttleGui.h"
#include "valnum.h"
#include "../../widgets/AudacityMessageBox.h"

#include "../../LabelTrack.h"
#include "WaveTrack.h"

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
                       const PluginPath & path,
                       int output,
                       bool hasParameters)
:  mPlugin(std::move(plugin)),
   mPath(path),
   mOutput(output),
   mHasParameters(hasParameters),
   mRate(0)
{
   mKey = mPath.BeforeFirst(wxT('/')).ToUTF8();
   mName = mPath.AfterFirst(wxT('/'));
}

VampEffect::~VampEffect()
{
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath VampEffect::GetPath() const
{
   return mPath;
}

ComponentInterfaceSymbol VampEffect::GetSymbol() const
{
   return mName;
}

VendorSymbol VampEffect::GetVendor() const
{
   return { wxString::FromUTF8(mPlugin->getMaker().c_str()) };
}

wxString VampEffect::GetVersion() const
{
   return wxString::Format(wxT("%d"), mPlugin->getPluginVersion());
}

TranslatableString VampEffect::GetDescription() const
{
   return Verbatim(
      wxString::FromUTF8(mPlugin->getCopyright().c_str()) );
}

// ============================================================================
// EffectDefinitionInterface implementation
// ============================================================================

EffectType VampEffect::GetType() const
{
   return EffectTypeAnalyze;
}

EffectFamilySymbol VampEffect::GetFamily() const
{
   return VAMPEFFECTS_FAMILY;
}

bool VampEffect::IsInteractive() const
{
   return mHasParameters;
}

bool VampEffect::IsDefault() const
{
   return false;
}

unsigned VampEffect::GetAudioInCount() const
{
   return mPlugin->getMaxChannelCount();
}

bool VampEffect::SaveSettings(
   const EffectSettings &, CommandParameters & parms) const
{
   for (size_t p = 0, paramCount = mParameters.size(); p < paramCount; p++)
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
         std::vector<EnumValueSymbol> choices;
         int val = 0;

         for (size_t i = 0, choiceCount = mParameters[p].valueNames.size(); i < choiceCount; i++)
         {
            wxString choice = wxString::FromUTF8(mParameters[p].valueNames[i].c_str());
            if (size_t(value - mParameters[p].minValue + 0.5) == i)
            {
               val = i;
            }
            choices.push_back(choice);
         }

         parms.WriteEnum(key, val, choices.data(), choices.size());
      }
      else
      {
         parms.Write(key, value);
      }
   }

   return true;
}

bool VampEffect::LoadSettings(
   const CommandParameters & parms, EffectSettings &settings) const
{
   // First pass verifies values
   for (size_t p = 0, paramCount = mParameters.size(); p < paramCount; p++)
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
         std::vector<EnumValueSymbol> choices;
         int val;

         for (size_t i = 0, choiceCount = mParameters[p].valueNames.size(); i < choiceCount; i++)
         {
            wxString choice = wxString::FromUTF8(mParameters[p].valueNames[i].c_str());
            choices.push_back(choice);
         }

         good = parms.ReadEnum(key, &val, choices.data(), choices.size()) && val != wxNOT_FOUND;
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
   for (size_t p = 0, paramCount = mParameters.size(); p < paramCount; p++)
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
         std::vector<EnumValueSymbol> choices;
         int val = 0;

         for (size_t i = 0, choiceCount = mParameters[p].valueNames.size(); i < choiceCount; i++)
         {
            wxString choice = wxString::FromUTF8(mParameters[p].valueNames[i].c_str());
            choices.push_back(choice);
         }

         parms.ReadEnum(key, &val, choices.data(), choices.size());

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
   mRate = 0.0;

   // PRL: this loop checked that channels of a track have the same rate,
   // but there was no check that all tracks have one rate, and only the first
   // is remembered in mRate.  Is that correct?

   for (auto leader : inputTracks()->Leaders<const WaveTrack>()) {
      auto channelGroup = TrackList::Channels( leader );
      auto rate = (*channelGroup.first++) -> GetRate();
      for(auto channel : channelGroup) {
         if (rate != channel->GetRate())
         // PRL:  Track rate might not match individual clip rates.
         // So is this check not adequate?
          {
             // TODO: more-than-two-channels-message
             Effect::MessageBox(
                XO(
"Sorry, Vamp Plug-ins cannot be run on stereo tracks where the individual channels of the track do not match.") );
             return false;
         }
      }
      if (mRate == 0.0)
         mRate = rate;
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
      Effect::MessageBox( XO("Sorry, failed to load Vamp Plug-in.") );
      return false;
   }

   return true;
}

bool VampEffect::Process(EffectInstance &, EffectSettings &)
{
   if (!mPlugin)
   {
      return false;
   }

   int count = 0;

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

   for (auto leader : inputTracks()->Leaders<const WaveTrack>())
   {
      auto channelGroup = TrackList::Channels(leader);
      auto left = *channelGroup.first++;

      unsigned channels = 1;

      // channelGroup now contains all but the first channel
      const WaveTrack *right =
         channelGroup.size() ? *channelGroup.first++ : nullptr;
      if (right)
         channels = 2;

      sampleCount start = 0;
      sampleCount len = 0;
      GetBounds(*left, right, &start, &len);

      // TODO: more-than-two-channels

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
            Effect::MessageBox(
               XO("Sorry, Vamp Plug-in failed to initialize.") );
            return false;
         }
      }

      const auto effectName = GetSymbol().Translation();
      addedTracks.push_back(AddAnalysisTrack(
         multiple
         ? wxString::Format( _("%s: %s"), left->GetName(), effectName )
         : effectName
      ));
      LabelTrack *ltrack = addedTracks.back()->get();

      FloatBuffers data{ channels, block };

      auto originalLen = len;

      auto pos = start;

      while (len != 0)
      {
         const auto request = limitSampleBufferSize( block, len );

         if (left)
         {
            left->GetFloats(data[0].get(), pos, request);
         }

         if (right)
         {
            right->GetFloats(data[1].get(), pos, request);
         }

         if (request < block)
         {
            for (unsigned int c = 0; c < channels; ++c)
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
            long( pos.as_long_long() ),
            (int)(mRate + 0.5)
         );

         Vamp::Plugin::FeatureSet features = mPlugin->process(
            reinterpret_cast< float** >( data.get() ), timestamp);
         AddFeatures(ltrack, features);

         if (len > (int)step)
         {
            len -= step;
         }
         else
         {
            len = 0;
         }

         pos += step;

         if (channels > 1)
         {
            if (TrackGroupProgress(count,
                  (pos - start).as_double() /
                  originalLen.as_double() ))
            {
               return false;
            }
         }
         else
         {
            if (TrackProgress(count,
                  (pos - start).as_double() /
                  originalLen.as_double() ))
            {
               return false;
            }
         }
      }

      Vamp::Plugin::FeatureSet features = mPlugin->getRemainingFeatures();
      AddFeatures(ltrack, features);

      prevTrackChannels = channels;
   }

   // All completed without cancellation, so commit the addition of tracks now
   for (auto &addedTrack : addedTracks)
      addedTrack->Commit();

   return true;
}

std::unique_ptr<EffectUIValidator> VampEffect::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *)
{
   mUIParent = S.GetParent();
   Vamp::Plugin::ProgramList programs = mPlugin->getPrograms();

   mParameters = mPlugin->getParameterDescriptors();

   auto count = mParameters.size();

   mToggles.reinit( count );
   mSliders.reinit( count );
   mFields.reinit( count );
   mLabels.reinit( count );
   mChoices.reinit( count );
   mValues.reinit( count );

   wxScrolledWindow *scroller = S.Style(wxVSCROLL | wxTAB_TRAVERSAL)
      .StartScroller(2);
   {
      S.StartStatic(XO("Plugin Settings"));
      {
         S.StartMultiColumn(5, wxEXPAND);
         {
            S.SetStretchyCol(3);

            if (!programs.empty())
            {
               S.AddPrompt(XXO("Program"));

               S.Id(ID_Program);
               mProgram = S.Name(XO("Program"))
                  .MinSize( { -1, -1 } )
                  .Position(wxEXPAND | wxALIGN_CENTER_VERTICAL | wxALL)
                  .AddChoice( {},
                     [&]{
                        TranslatableStrings choices;
                        for (const auto &program : programs)
                           choices.push_back(
                              Verbatim(wxString::FromUTF8(program.c_str())));
                        return choices;
                     }(),
                     Verbatim( wxString::FromUTF8(mPlugin->getCurrentProgram().c_str()) )
                  );

               S.AddSpace(1, 1);
               S.AddSpace(1, 1);
               S.AddSpace(1, 1);
            }

            for (size_t p = 0; p < count; p++)
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
               if (!unit.empty())
               {
                  labelText += wxT(" (") + unit + wxT(")");
               }
               /* i18n-hint: An item name introducing a value, which is not part of the string but
               appears in a following text box window; translate with appropriate punctuation */
               S.AddPrompt(XXO("%s:").Format( labelText ));

               if (mParameters[p].isQuantized &&
                   mParameters[p].quantizeStep == 1.0 &&
                   mParameters[p].minValue == 0.0 &&
                   mParameters[p].maxValue == 1.0)
               {
                  S.Id(ID_Toggles + p);
                  mToggles[p] = S.ToolTip( Verbatim( tip ) )
                     .Name( Verbatim( labelText ) )
                     .Position(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL | wxALL)
                     .AddCheckBox( {},
                                  value > 0.5 );

                  S.AddSpace(1, 1);
                  S.AddSpace(1, 1);
                  S.AddSpace(1, 1);

               }
               else if (mParameters[p].isQuantized &&
                        mParameters[p].quantizeStep == 1.0 &&
                        !mParameters[p].valueNames.empty())
               {
                  TranslatableStrings choices;
                  int selected = -1;

                  for (size_t i = 0, cnt = mParameters[p].valueNames.size(); i < cnt; i++)
                  {
                     wxString choice = wxString::FromUTF8(mParameters[p].valueNames[i].c_str());
                     if (size_t(value - mParameters[p].minValue + 0.5) == i)
                     {
                        selected = i;
                     }
                     choices.push_back( Verbatim( choice ) );
                  }

                  S.Id(ID_Choices + p);
                  mChoices[p] = S.ToolTip( Verbatim( tip ) )
                     .Name( Verbatim( labelText ) )
                     .Position(wxEXPAND | wxALIGN_CENTER_VERTICAL | wxALL)
                     .MinSize( { -1, -1 } )
                     .AddChoice( {}, choices, selected );

                  S.AddSpace(1, 1);
                  S.AddSpace(1, 1);
                  S.AddSpace(1, 1);
               }
               else
               {
                  mValues[p] = value;

                  float range = mParameters[p].maxValue - mParameters[p].minValue;

                  S.Id(ID_Texts + p);
                  mFields[p] = S.ToolTip( Verbatim( tip ) )
                     .Name( Verbatim( labelText ) )
                     .Position(wxALIGN_CENTER_VERTICAL | wxALL)
                     .Validator<FloatingPointValidator<float>>(
                        6, &mValues[p],
                        (range < 10
                           ? NumValidatorStyle::THREE_TRAILING_ZEROES
                           : range < 100
                              ? NumValidatorStyle::TWO_TRAILING_ZEROES
                              : NumValidatorStyle::ONE_TRAILING_ZERO),
                        mParameters[p].minValue, mParameters[p].maxValue)
                     .AddTextBox( {}, wxT(""), 12);

                  wxString str = Internat::ToDisplayString(mParameters[p].minValue);
                  S.AddPrompt( Verbatim( str ) );

                  S.Id(ID_Sliders + p);
                  mSliders[p] = S.ToolTip( Verbatim( tip ) )
                     .Name( Verbatim( labelText ) )
                     .Style(wxSL_HORIZONTAL)
                     .MinSize( { 150, -1 } )
                     .AddSlider( {}, 0, 1000, 0);

                  str = Internat::ToDisplayString(mParameters[p].maxValue);
                  S.AddUnits( Verbatim( str ) );
               }
            }
         }
         S.EndMultiColumn();
      }
      S.EndStatic();
   }
   S.EndScroller();

   scroller->SetScrollRate(0, 20);

   return nullptr;
}

bool VampEffect::TransferDataToWindow(const EffectSettings &)
{
   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   UpdateFromPlugin();

   return true;
}

bool VampEffect::TransferDataFromWindow(EffectSettings &)
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
