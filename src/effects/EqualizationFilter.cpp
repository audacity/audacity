/**********************************************************************

   Audacity: A Digital Audio Editor

   EqualizationFilter.cpp

   Mitch Golden
   Vaughan Johnson (Preview)
   Martyn Shaw (FIR filters, response curve, graphic EQ)

   Paul Licameli split from Equalization.cpp

**********************************************************************/
#include "EqualizationFilter.h"
#include "Envelope.h"
#include "FFT.h"

EqualizationFilter::EqualizationFilter(const EffectSettingsManager &manager)
   : EqualizationParameters{ manager }
{
   mLogEnvelope = std::make_unique<Envelope>
      (false,
       dBMin.min, dBMax.max, // MB: this is the highest possible range
       0.0);
   mLogEnvelope->SetTrackLen(1.0);

   mLinEnvelope = std::make_unique<Envelope>
      (false,
       dBMin.min, dBMax.max, // MB: this is the highest possible range
       0.0);
   mLinEnvelope->SetTrackLen(1.0);
}

#if 0
EffectEqualization::EffectEqualization(int Options)
   : mParameters{ GetDefinition() }
{
   auto &mLinEnvelope = mParameters.mLinEnvelope;
   auto &mLogEnvelope = mParameters.mLogEnvelope;
   auto &mHiFreq = mParameters.mHiFreq;

   const auto &mLin = mParameters.mLin;

   Parameters().Reset(*this);

   mOptions = Options;
   mGraphic = NULL;
   mDraw = NULL;
   mCurve = NULL;
   mPanel = NULL;
   mMSlider = NULL;

   SetLinearEffectFlag(true);

   mEnvelope = (mLin ? mLinEnvelope : mLogEnvelope).get();

   mDirty = false;
   mDisallowCustom = false;

   // Load the EQ curves
   EQCurveReader{ mCurves, GetName(), mOptions }.LoadCurves();

   // Note: initial curve is set in TransferDataToWindow

   mBandsInUse = NUMBER_OF_BANDS;
   //double loLog = log10(mLoFreq);
   //double stepLog = (log10(mHiFreq) - loLog)/((double)NUM_PTS-1.);
   for(int i=0; i<NUM_PTS-1; i++)
      mWhens[i] = (double)i/(NUM_PTS-1.);
   mWhens[NUM_PTS-1] = 1.;
   mWhenSliders[NUMBER_OF_BANDS] = 1.;
   mEQVals[NUMBER_OF_BANDS] = 0.;

   // We expect these Hi and Lo frequencies to be overridden by Init().
   // Don't use inputTracks().  See bug 2321.
#if 0
   auto trackList = inputTracks();
   const auto t = trackList
      ? *trackList->Any< const WaveTrack >().first
      : nullptr
   ;
   mHiFreq =
      (t
         ? t->GetRate()
         : mProjectRate)
      / 2.0;
#endif
   mHiFreq = mProjectRate / 2.0;
}


EffectEqualization::~EffectEqualization()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectEqualization::GetSymbol() const
{
   if( mOptions == kEqOptionGraphic )
      return EffectEqualizationGraphic::Symbol;
   if( mOptions == kEqOptionCurve )
      return EffectEqualizationCurve::Symbol;
   return EffectEqualization::Symbol;
}

TranslatableString EffectEqualization::GetDescription() const
{
   return XO("Adjusts the volume levels of particular frequencies");
}

ManualPageID EffectEqualization::ManualPage() const
{
   // Bug 2509: Must use _ and not space in names.
   if( mOptions == kEqOptionGraphic )
      return L"Graphic_EQ";
   if( mOptions == kEqOptionCurve )
      return L"Filter_Curve_EQ";
   return L"Equalization";
}

// EffectDefinitionInterface implementation

EffectType EffectEqualization::GetType() const
{
   return EffectTypeProcess;
}

bool EffectEqualization::VisitSettings(
   ConstSettingsVisitor &visitor, const EffectSettings &settings) const
{
   Effect::VisitSettings(visitor, settings);

   // Curve point parameters -- how many isn't known statically
   if( dynamic_cast<ShuttleGetAutomation*>(&visitor)) {
      int numPoints = mCurves[ 0 ].points.size();
      int point;
      for( point = 0; point < numPoints; point++ )
      {
         const wxString nameFreq = wxString::Format("f%i",point);
         const wxString nameVal = wxString::Format("v%i",point);
         visitor.Define( mCurves[ 0 ].points[ point ].Freq, nameFreq,
            0.0, 0.0, 0.0, 0.0 );
         visitor.Define( mCurves[ 0 ].points[ point ].dB, nameVal,
            0.0, 0.0, 0.0, 0.0 );
      }
   }
   return true;
}

bool EffectEqualization::VisitSettings(
   SettingsVisitor &visitor, EffectSettings &settings)
{
   Effect::VisitSettings(visitor, settings);

   // Curve point parameters -- how many isn't known statically
   {
      mCurves[0].points.clear();
   
      for (int i = 0; i < 200; i++)
      {
         const wxString nameFreq = wxString::Format("f%i",i);
         const wxString nameVal = wxString::Format("v%i",i);
         double f = -1000.0;
         double d = 0.0;
         visitor.Define( f, nameFreq, 0.0,  -10000.0, 1000000.0, 0.0 );
         visitor.Define( d, nameVal,  0.0, -10000.0, 10000.0, 0.0 );
         if( f <= 0.0 )
            break;
         mCurves[0].points.push_back( EQPoint( f,d ));
      }
      setCurve( 0 );
   }
   return true;
}

OptionalMessage
EffectEqualization::LoadFactoryDefaults(EffectSettings &settings) const
{
   // To do: externalize state so const_cast isn't needed
   if (!const_cast<EffectEqualization&>(*this).DoLoadFactoryDefaults(settings))
      return {};
   return { nullptr };
}

OptionalMessage
EffectEqualization::DoLoadFactoryDefaults(EffectSettings &settings)
{
   mParameters.LoadDefaults(mOptions);
   return Effect::LoadFactoryDefaults(settings);
}

// Constants determining who the prests are for.
const bool kCURVE = false;
const bool kBOTH = true;

static const struct
{
   const bool bForBoth; // more extended set is used for Filter EQ
   // See Bug 2254 for rationale.
   const TranslatableString name;
   const wxChar *values;
}
FactoryPresets[] =
{
   { kCURVE, XO("100Hz Rumble"),           wxT("f0=\"20.0\" v0=\"-80.0\" f1=\"49.237316986327\" v1=\"-33.107692718506\" f2=\"54.196034330446\" v2=\"-29.553844451904\" f3=\"88.033573501041\" v3=\"-6.923076629639\" f4=\"95.871851182279\" v4=\"-4.523078918457\" f5=\"108.957037410504\" v5=\"-1.938461303711\" f6=\"123.828171198057\" v6=\"-0.73846244812\" f7=\"149.228077614658\" v7=\"-0.092308044434\"") },
   { kCURVE, XO("AM Radio"),               wxT("f0=\"20.0\" v0=\"-63.67\" f1=\"31.0\" v1=\"-33.219\" f2=\"50.0\" v2=\"-3.01\" f3=\"63.0\" v3=\"-0.106\" f4=\"100.0\" v4=\"0.0\" f5=\"2500.0\" v5=\"0.0\" f6=\"4000.0\" v6=\"-0.614\" f7=\"5000.0\" v7=\"-8.059\" f8=\"8000.0\" v8=\"-39.981\" f9=\"20000.0\" v9=\"-103.651\" f10=\"48000.0\" v10=\"-164.485\"") },
   { kBOTH,  XO("Bass Boost"),             wxT("f0=\"100.0\" v0=\"9.0\" f1=\"500.0\" v1=\"0.0\"") },
   { kBOTH,  XO("Bass Cut"),               wxT("f0=\"150.0\" v0=\"-50.0\" f1=\"300.0\" v1=\"0.0\"") },
   { kCURVE, XO("Low rolloff for speech"), wxT("f0=\"50.0\" v0=\"-120.0\" f1=\"60.0\" v1=\"-50.0\" f2=\"65.0\" v2=\"-24.0\" f3=\"70.0\" v3=\"-12.0\" f4=\"80.0\" v4=\"-4.0\" f5=\"90.0\" v5=\"-1.0\" f6=\"100.0\" v6=\"0.0\"") },
   { kBOTH,  XO("RIAA"),                   wxT("f0=\"20.0\" v0=\"19.274\" f1=\"25.0\" v1=\"18.954\" f2=\"31.0\" v2=\"18.516\" f3=\"40.0\" v3=\"17.792\" f4=\"50.0\" v4=\"16.946\" f5=\"63.0\" v5=\"15.852\" f6=\"80.0\" v6=\"14.506\" f7=\"100.0\" v7=\"13.088\" f8=\"125.0\" v8=\"11.563\" f9=\"160.0\" v9=\"9.809\" f10=\"200.0\" v10=\"8.219\" f11=\"250.0\" v11=\"6.677\" f12=\"315.0\" v12=\"5.179\" f13=\"400.0\" v13=\"3.784\" f14=\"500.0\" v14=\"2.648\" f15=\"630.0\" v15=\"1.642\" f16=\"800.0\" v16=\"0.751\" f17=\"1000.0\" v17=\"0.0\" f18=\"1250.0\" v18=\"-0.744\" f19=\"1600.0\" v19=\"-1.643\" f20=\"2000.0\" v20=\"-2.589\" f21=\"2500.0\" v21=\"-3.7\" f22=\"3150.0\" v22=\"-5.038\" f23=\"4000.0\" v23=\"-6.605\" f24=\"5000.0\" v24=\"-8.21\" f25=\"6300.0\" v25=\"-9.98\" f26=\"8000.0\" v26=\"-11.894\" f27=\"10000.0\" v27=\"-13.734\" f28=\"12500.0\" v28=\"-15.609\" f29=\"16000.0\" v29=\"-17.708\" f30=\"20000.0\" v30=\"-19.62\" f31=\"25000.0\" v31=\"-21.542\" f32=\"48000.0\" v32=\"-27.187\"") },
   { kCURVE, XO("Telephone"),              wxT("f0=\"20.0\" v0=\"-94.087\" f1=\"200.0\" v1=\"-14.254\" f2=\"250.0\" v2=\"-7.243\" f3=\"315.0\" v3=\"-2.245\" f4=\"400.0\" v4=\"-0.414\" f5=\"500.0\" v5=\"0.0\" f6=\"2500.0\" v6=\"0.0\" f7=\"3150.0\" v7=\"-0.874\" f8=\"4000.0\" v8=\"-3.992\" f9=\"5000.0\" v9=\"-9.993\" f10=\"48000.0\" v10=\"-88.117\"") },
   { kBOTH,  XO("Treble Boost"),           wxT("f0=\"4000.0\" v0=\"0.0\" f1=\"5000.0\" v1=\"9.0\"") },
   { kBOTH,  XO("Treble Cut"),             wxT("f0=\"6000.0\" v0=\"0.0\" f1=\"10000.0\" v1=\"-110.0\"") },
   { kCURVE, XO("Walkie-talkie"),          wxT("f0=\"100.0\" v0=\"-120.0\" f1=\"101.0\" v1=\"0.0\" f2=\"2000.0\" v2=\"0.0\" f3=\"2001.0\" v3=\"-120.0\"") },
};




RegistryPaths EffectEqualization::GetFactoryPresets() const
{
   RegistryPaths names;

   for (size_t i = 0; i < WXSIZEOF(FactoryPresets); i++)
   {
      if ((mOptions == kEqOptionGraphic) && (FactoryPresets[i].bForBoth == false))
         continue;
      names.push_back(FactoryPresets[i].name.Translation());
   }

   return names;
}

OptionalMessage
EffectEqualization::LoadFactoryPreset(int id, EffectSettings &settings) const
{
   int index = -1;
   for (size_t i = 0; i < WXSIZEOF(FactoryPresets); i++)
   {
      if ((mOptions == kEqOptionGraphic) && (FactoryPresets[i].bForBoth == false))
         continue;
      if (id-- == 0) {
         index = i;
         break;
      }
   }
   if (index < 0)
      return {};

   // mParams = 
   wxString params = FactoryPresets[index].values;

   CommandParameters eap(params);
   ShuttleSetAutomation S;
   S.SetForWriting( &eap );
   // To do: externalize state so const_cast isn't needed
   if (!const_cast<EffectEqualization*>(this)->VisitSettings(S, settings))
      return {};
   return { nullptr };
}



// EffectUIClientInterface implementation

bool EffectEqualization::ValidateUI(EffectSettings &)
{
   const auto &mCurveName = mParameters.mCurveName;
   const auto &mDrawMode = mParameters.mDrawMode;
   auto &mLogEnvelope = mParameters.mLogEnvelope;

   // If editing a macro, we don't want to be using the unnamed curve so
   // we offer to save it.

   if (mDisallowCustom && mCurveName == wxT("unnamed"))
   {
      // PRL:  This is unreachable.  mDisallowCustom is always false.

      Effect::MessageBox(
         XO("To use this filter curve in a macro, please choose a new name for it.\nChoose the 'Save/Manage Curves...' button and rename the 'unnamed' curve, then use that one."),
         wxOK | wxCENTRE,
         XO("Filter Curve EQ needs a different name") );
      return false;
   }

   // Update unnamed curve (so it's there for next time)
   //(done in a hurry, may not be the neatest -MJS)
   if (mDirty && !mDrawMode)
   {
      size_t numPoints = mLogEnvelope->GetNumberOfPoints();
      Doubles when{ numPoints };
      Doubles value{ numPoints };
      mLogEnvelope->GetPoints(when.get(), value.get(), numPoints);
      for (size_t i = 0, j = 0; j + 2 < numPoints; i++, j++)
      {
         if ((value[i] < value[i + 1] + .05) && (value[i] > value[i + 1] - .05) &&
            (value[i + 1] < value[i + 2] + .05) && (value[i + 1] > value[i + 2] - .05))
         {   // within < 0.05 dB?
            mLogEnvelope->Delete(j + 1);
            numPoints--;
            j--;
         }
      }
      Select((int) mCurves.size() - 1);
   }
   EQCurveWriter{ mCurves }.SaveCurves();

   mParameters.SaveConfig(GetDefinition());

   return true;
}

// Effect implementation

bool EffectEqualization::Init()
{
   constexpr auto loFreqI = EqualizationFilter::loFreqI;

   const auto &mLin = mParameters.mLin;
   const auto &mCurveName = mParameters.mCurveName;
   auto &mLinEnvelope = mParameters.mLinEnvelope;
   auto &mLogEnvelope = mParameters.mLogEnvelope;
   auto &mLoFreq = mParameters.mLoFreq;
   auto &mHiFreq = mParameters.mHiFreq;

   int selcount = 0;
   double rate = 0.0;

   if (const auto project = FindProject()) {
      auto trackRange = TrackList::Get(*project).Selected<const WaveTrack>();
      if (trackRange) {
         rate = (*(trackRange.first++)) -> GetRate();
         ++selcount;

         for (auto track : trackRange) {
            if (track->GetRate() != rate) {
               Effect::MessageBox(
                  XO(
   "To apply Equalization, all selected tracks must have the same sample rate.") );
               return(false);
            }
            ++selcount;
         }
      }
   }
   else
      // Editing macro parameters, use this default
      rate = 44100.0;

   mHiFreq = rate / 2.0;
   // Unlikely, but better than crashing.
   if (mHiFreq <= loFreqI) {
      Effect::MessageBox(
         XO("Track sample rate is too low for this effect."),
         wxOK | wxCENTRE,
         XO("Effect Unavailable") );
      return(false);
   }

   mLoFreq = loFreqI;

   mBandsInUse = 0;
   while (kThirdOct[mBandsInUse] <= mHiFreq) {
      mBandsInUse++;
      if (mBandsInUse == NUMBER_OF_BANDS)
         break;
   }

   mEnvelope = (mLin ? mLinEnvelope : mLogEnvelope).get();

   setCurve(mCurveName);

   mParameters.CalcFilter();

   return(true);
}

bool EffectEqualization::Process(EffectInstance &, EffectSettings &)
{
   this->CopyInputTracks(); // Set up mOutputTracks.
   mParameters.CalcFilter();
   bool bGoodResult = true;

   int count = 0;
   for( auto track : mOutputTracks->Selected< WaveTrack >() ) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;

      if (t1 > t0) {
         auto start = track->TimeToLongSamples(t0);
         auto end = track->TimeToLongSamples(t1);
         auto len = end - start;

         if (!ProcessOne(count, track, start, len))
         {
            bGoodResult = false;
            break;
         }
      }

      count++;
   }

   this->ReplaceProcessedTracks(bGoodResult);
   return bGoodResult;
}

bool EffectEqualization::CloseUI()
{
   mCurve = NULL;
   mPanel = NULL;

   return Effect::CloseUI();
}

std::unique_ptr<EffectUIValidator> EffectEqualization::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &access,
   const EffectOutputs *)
{
   const auto &mM = mParameters.mM;
   const auto &mLoFreq = mParameters.mLoFreq;
   const auto &mHiFreq = mParameters.mHiFreq;

   auto &mDrawMode = mParameters.mDrawMode;

   S.SetBorder(0);

   S.SetSizerProportion(1);
   S.Prop(1).StartMultiColumn(1, wxEXPAND);
   {
      S.SetStretchyCol(0);
      //S.SetStretchyRow(0); // The 5px Top border
      S.SetStretchyRow(1);   // The Graph
      S.SetStretchyRow(2);   // The EQ sliders
      szrV = S.GetSizer();

      // -------------------------------------------------------------------
      // ROW 0: Top border
      // -------------------------------------------------------------------
      S.AddSpace(5);

      // -------------------------------------------------------------------
      // ROW 1: Equalization panel and sliders for vertical scale
      // -------------------------------------------------------------------
      S.SetSizerProportion(1);
      S.Prop(1).StartMultiColumn(3, wxEXPAND);
      {
         S.SetStretchyCol(1);
         S.SetStretchyRow(0);
         szr1 = S.GetSizer();

         S.StartVerticalLay(wxEXPAND, 1);
         {
            mdBRuler = safenew RulerPanel(
               S.GetParent(), wxID_ANY, wxVERTICAL,
               wxSize{ 100, 100 }, // Ruler can't handle small sizes
               RulerPanel::Range{ 60.0, -120.0 },
               Ruler::LinearDBFormat,
               XO("dB"),
               RulerPanel::Options{}
                  .LabelEdges(true)
                  .TicksAtExtremes(true)
                  .TickColour( { 0, 0, 0 } )
            );

            S.Prop(0).AddSpace(0, 1);
            S.Prop(1)
               .Position(wxEXPAND)
               .AddWindow(mdBRuler);
            S.AddSpace(0, 1);
         }
         S.EndVerticalLay();

         mPanel = safenew EqualizationPanel(S.GetParent(), wxID_ANY, this);
         S.Prop(1)
            .Position(wxEXPAND)
            .MinSize( { wxDefaultCoord, wxDefaultCoord } )
            .AddWindow(mPanel);

         S.SetBorder(5);
         S.StartVerticalLay();
         {
            S.AddVariableText(XO("+ dB"), false, wxCENTER);
            mdBMaxSlider = S.Id(ID_dBMax)
               .Name(XO("Max dB"))
               .Style(wxSL_VERTICAL | wxSL_INVERSE)
               .AddSlider( {}, 30, 60, 0);
#if wxUSE_ACCESSIBILITY
            mdBMaxSlider->SetAccessible(safenew SliderAx(mdBMaxSlider, XO("%d dB")));
#endif

            mdBMinSlider = S.Id(ID_dBMin)
               .Name(XO("Min dB"))
               .Style(wxSL_VERTICAL | wxSL_INVERSE)
               .AddSlider( {}, -30, -10, -120);
            S.AddVariableText(XO("- dB"), false, wxCENTER);
#if wxUSE_ACCESSIBILITY
            mdBMinSlider->SetAccessible(safenew SliderAx(mdBMinSlider, XO("%d dB")));
#endif
         }
         S.EndVerticalLay();
         S.SetBorder(0);

         // -------------------------------------------------------------------
         // Frequency ruler below graph
         // -------------------------------------------------------------------

         // Column 1 is empty
         S.AddSpace(1, 1);

         mFreqRuler  = safenew RulerPanel(
            S.GetParent(), wxID_ANY, wxHORIZONTAL,
            wxSize{ 100, 100 }, // Ruler can't handle small sizes
            RulerPanel::Range{ mLoFreq, mHiFreq },
            Ruler::IntFormat,
            XO("Hz"),
            RulerPanel::Options{}
               .Log(true)
               .Flip(true)
               .LabelEdges(true)
               .TicksAtExtremes(true)
               .TickColour( { 0, 0, 0 } )
         );

         S.SetBorder(1);
         S.Prop(1)
            .Position(wxEXPAND | wxALIGN_LEFT | wxALIGN_TOP | wxLEFT)
            .AddWindow(mFreqRuler);
         S.SetBorder(0);

         // Column 3 is empty
         S.AddSpace(1, 1);
      }
      S.EndMultiColumn();

      // -------------------------------------------------------------------
      // ROW 2: Graphic EQ
      // -------------------------------------------------------------------
      S.SetSizerProportion(1);
      S.StartHorizontalLay(wxEXPAND, 1);
      {
         szrG = S.GetSizer();

         // Panel used to host the sliders since they will be positioned manually.
         //mGraphicPanel = S.Prop(1)
            //.Position(wxEXPAND)
            //.Size( { -1, 150 } )
            //.StartPanel();
         wxWindow *pParent = S.GetParent();
         S.AddSpace(15,0);
         {

         // for (int i = 0; (i < NUMBER_OF_BANDS) && (kThirdOct[i] <= mHiFreq); ++i)
         // May show more sliders than needed.  Fixes Bug 2269
         for (int i = 0; i < NUMBER_OF_BANDS; ++i)
         {
            TranslatableString freq = kThirdOct[i] < 1000.
               ? XO("%d Hz").Format((int)kThirdOct[i])
               : XO("%g kHz").Format(kThirdOct[i] / 1000.);
            TranslatableString fNum = kThirdOct[i] < 1000.
               ? Verbatim("%d").Format((int)kThirdOct[i])
               /* i18n-hint k is SI abbreviation for x1,000.  Usually unchanged in translation. */
               : XO("%gk").Format(kThirdOct[i] / 1000.);
            S.StartVerticalLay();
            {
               S.AddFixedText( fNum  );
               mSliders[i] = safenew wxSliderWrapper(pParent, ID_Slider + i, 0, -20, +20,
                  wxDefaultPosition, wxSize(-1,50), wxSL_VERTICAL | wxSL_INVERSE);

#if wxUSE_ACCESSIBILITY
               mSliders[i]->SetAccessible(safenew SliderAx(mSliders[i], XO("%d dB")));
#endif

               mSlidersOld[i] = 0;
               mEQVals[i] = 0.;
               S.Prop(1)
                  .Name(freq)
                  .ConnectRoot(
                     wxEVT_ERASE_BACKGROUND, &EffectEqualization::OnErase)
                  .Position(wxEXPAND)
                  .Size({ -1, 50 })
                  .AddWindow(mSliders[i]);
            }
            S.EndVerticalLay();
         }
         S.AddSpace(15,0);

         } //S.EndPanel();
      }
      S.EndHorizontalLay();

      // -------------------------------------------------------------------
      // ROW 4: Various controls
      // -------------------------------------------------------------------
      S.SetSizerProportion(1);
      S.Prop(1).StartMultiColumn(7, wxALIGN_CENTER_HORIZONTAL);
      {
         S.SetBorder(5);

         S.AddSpace(5, 5);

         if( mOptions == kEqLegacy )
         {
            S.StartHorizontalLay(wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
            {
               S.AddPrompt(XXO("&EQ Type:"));
            }
            S.EndHorizontalLay();

            S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL, 1);
            {
               S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL, 1);
               {
                  mDraw = S.Id(ID_Draw)
                     .Name(XO("Draw Curves"))
                     .AddRadioButton(XXO("&Draw"));

                  mGraphic = S.Id(ID_Graphic)
                     .Name(XO("Graphic EQ"))
                     .AddRadioButtonToGroup(XXO("&Graphic"));
               }
               S.EndHorizontalLay();
            }
            S.EndHorizontalLay();
         }

         S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL, 0);
         {
            szrH = S.GetSizer();

            S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL, 1);
            {
               szrI = S.GetSizer();

               mInterpChoice = S.Id(ID_Interp)
                  .Name(XO("Interpolation type"))
                  .AddChoice( {},
                     Msgids(EqualizationParameters::kInterpStrings,
                        EqualizationParameters::nInterpolations),
                     0 );
#if wxUSE_ACCESSIBILITY
               // so that name can be set on a standard control
               mInterpChoice->SetAccessible(safenew WindowAccessible(mInterpChoice));
#endif
            }
            S.EndHorizontalLay();

            S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL, 1);
            {
               szrL = S.GetSizer();

               mLinFreq = S.Id(ID_Linear)
                  .Name(XO("Linear Frequency Scale"))
                  .AddCheckBox(XXO("Li&near Frequency Scale"), false);
            }
            S.EndHorizontalLay();
         }
         S.EndHorizontalLay();

         // -------------------------------------------------------------------
         // Filter length grouping
         // -------------------------------------------------------------------

         if( mOptions == kEqLegacy ){
            S.StartHorizontalLay(wxEXPAND, 0);
            {
               S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL, 0);
               {
                  S.AddPrompt(XXO("Length of &Filter:"));
               }
               S.EndHorizontalLay();

               S.StartHorizontalLay(wxEXPAND, 1);
               {
                  mMSlider = S.Id(ID_Length)
                     .Name(XO("Length of Filter"))
                     .Style(wxSL_HORIZONTAL)
                     .AddSlider( {}, (mM - 1) / 2, 4095, 10);
               }
               S.EndHorizontalLay();

               S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL, 0);
               {
                  wxString label;
                  label.Printf(wxT("%ld"), mM);
                  mMText = S.Name( Verbatim( label ) )
                  // fix for bug 577 (NVDA/Narrator screen readers do not
                  // read static text in dialogs)
                     .AddVariableText( Verbatim( label ) );
               }
               S.EndHorizontalLay();
            }
            S.EndHorizontalLay();

            S.AddSpace(1, 1);
         }

         S.AddSpace(5, 5);

         if( mOptions == kEqLegacy ){
            S.AddSpace(5, 5);
            S.StartHorizontalLay(wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
            {
               S.AddPrompt(XXO("&Select Curve:"));
            }
            S.EndHorizontalLay();

            S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL, 1);
            {
               S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL, 1);
               {
                  mCurve = S.Id(ID_Curve)
                     .Name(XO("Select Curve"))
                     .AddChoice( {},
                        [this]{
                           TranslatableStrings curves;
                           for (const auto &curve : mCurves)
                              curves.push_back( Verbatim( curve.Name ) );
                           return curves;
                        }()
                     );
               }
               S.EndHorizontalLay();
            }
            S.EndHorizontalLay();

            S.Id(ID_Manage).AddButton(XXO("S&ave/Manage Curves..."));
         }

         S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL, 1);
         {
            S.Id(ID_Clear).AddButton(XXO("Fla&tten"));
            S.Id(ID_Invert).AddButton(XXO("&Invert"));

            mGridOnOff = S.Id(ID_Grid)
               .Name(XO("Show grid lines"))
               .AddCheckBox(XXO("Show g&rid lines"), false);
         }
         S.EndHorizontalLay();

         S.AddSpace(5, 5);
      }
      S.EndMultiColumn();
   }
   S.EndMultiColumn();

   mUIParent->SetAutoLayout(false);
   if( mOptions != kEqOptionGraphic)
      mUIParent->Layout();

   if( mOptions == kEqOptionCurve)
      mDrawMode = true;
   if( mOptions == kEqOptionGraphic)
      mDrawMode = false;

   // "show" settings for graphics mode before setting the size of the dialog
   // as this needs more space than draw mode
   szrV->Show(szrG,!mDrawMode);  // eq sliders
   szrH->Show(szrI,true);  // interpolation choice
   szrH->Show(szrL,false); // linear freq checkbox

   if( mOptions == kEqOptionGraphic){
      mPanel->Show( false );
      wxSize sz = szrV->GetMinSize();
      sz += wxSize( 30, 0);
      mUIParent->SetMinSize(sz);
   }
   else{
      mPanel->Show( true );
      szrV->Show(szr1, true);
      // This sizing calculation is hacky.
      // Rather than set the true minimum size we set a size we would 
      // like to have.
      // This makes the default size of the dialog good, but has the 
      // downside that the user can't adjust the dialog smaller.
      wxSize sz = szrV->GetMinSize();
      sz += wxSize( 400, 100);
      szrV->SetMinSize(sz);
   }
   ForceRecalc();

   return nullptr;
}

//
// Populate the window with relevant variables
//
bool EffectEqualization::TransferDataToWindow(const EffectSettings &settings)
{
   const auto &mLin = mParameters.mLin;
   const auto &mDrawGrid = mParameters.mDrawGrid;
   const auto &mM = mParameters.mM;
   const auto &mdBMin = mParameters.mdBMin;
   const auto &mdBMax = mParameters.mdBMax;
   const auto &mInterp = mParameters.mInterp;

   auto &mDrawMode = mParameters.mDrawMode;

   // Set log or lin freq scale (affects interpolation as well)
   mLinFreq->SetValue( mLin );
   wxCommandEvent dummyEvent;
   OnLinFreq(dummyEvent);  // causes a CalcFilter

   mGridOnOff->SetValue( mDrawGrid ); // checks/unchecks the box on the interface

   if( mMSlider )
      mMSlider->SetValue((mM - 1) / 2);

   mdBMinSlider->SetValue((int)mdBMin);
   mdBMaxSlider->SetValue((int)mdBMax);

   // Reload the curve names
   UpdateCurves();

   // Set graphic interpolation mode
   mInterpChoice->SetSelection(mInterp);

   // Override draw mode, if we're not displaying the radio buttons.
   if( mOptions == kEqOptionCurve)
      mDrawMode = true;
   if( mOptions == kEqOptionGraphic)
      mDrawMode = false;

   if( mDraw )
      mDraw->SetValue(mDrawMode);
   szrV->Show(szr1,mOptions != kEqOptionGraphic); // Graph
   szrV->Show(szrG,!mDrawMode);    // eq sliders
   szrH->Show(szrI,mOptions == kEqLegacy );    // interpolation choice
   szrH->Show(szrL, mDrawMode);    // linear freq checkbox
   if( mGraphic) 
      mGraphic->SetValue(!mDrawMode);
   mGridOnOff->Show( mDrawMode );

   // Set Graphic (Fader) or Draw mode
   if (!mDrawMode)
      UpdateGraphic();

   UpdateRuler();

   mUIParent->Layout();
   wxGetTopLevelParent(mUIParent)->Layout();

   return true;
}

void EffectEqualization::UpdateRuler()
{
   const auto &mdBMin = mParameters.mdBMin;
   const auto &mdBMax = mParameters.mdBMax;

   // Refresh ruler when values have changed
   int w1, w2, h;
   mdBRuler->ruler.GetMaxSize(&w1, &h);
   mdBRuler->ruler.SetRange(mdBMax, mdBMin);
   mdBRuler->ruler.GetMaxSize(&w2, &h);
   if( w1 != w2 )   // Reduces flicker
   {
      mdBRuler->SetSize(wxSize(w2,h));
      mFreqRuler->Refresh(false);
   }
   mdBRuler->Refresh(false);

   mPanel->Refresh(false);
}

// EffectEqualization implementation

bool EffectEqualization::ProcessOne(int count, WaveTrack * t,
                                    sampleCount start, sampleCount len)
{
   constexpr auto windowSize = EqualizationFilter::windowSize;

   const auto &mM = mParameters.mM;

   // create a NEW WaveTrack to hold all of the output, including 'tails' each end
   auto output = t->EmptyCopy();
   t->ConvertToSampleFormat( floatSample );

   wxASSERT(mM - 1 < windowSize);
   size_t L = windowSize - (mM - 1);   //Process L samples at a go
   auto s = start;
   auto idealBlockLen = t->GetMaxBlockSize() * 4;
   if (idealBlockLen % L != 0)
      idealBlockLen += (L - (idealBlockLen % L));

   Floats buffer{ idealBlockLen };

   Floats window1{ windowSize };
   Floats window2{ windowSize };
   float *thisWindow = window1.get();
   float *lastWindow = window2.get();

   auto originalLen = len;

   for(size_t i = 0; i < windowSize; i++)
      lastWindow[i] = 0;

   TrackProgress(count, 0.);
   bool bLoopSuccess = true;
   size_t wcopy = 0;
   int offset = (mM - 1) / 2;

   while (len != 0)
   {
      auto block = limitSampleBufferSize( idealBlockLen, len );

      t->GetFloats(buffer.get(), s, block);

      for(size_t i = 0; i < block; i += L)   //go through block in lumps of length L
      {
         wcopy = std::min <size_t> (L, block - i);
         for(size_t j = 0; j < wcopy; j++)
            thisWindow[j] = buffer[i+j];   //copy the L (or remaining) samples
         for(auto j = wcopy; j < windowSize; j++)
            thisWindow[j] = 0;   //this includes the padding

         mParameters.Filter(windowSize, thisWindow);

         // Overlap - Add
         for(size_t j = 0; (j < mM - 1) && (j < wcopy); j++)
            buffer[i+j] = thisWindow[j] + lastWindow[L + j];
         for(size_t j = mM - 1; j < wcopy; j++)
            buffer[i+j] = thisWindow[j];

         std::swap( thisWindow, lastWindow );
      }  //next i, lump of this block

      output->Append((samplePtr)buffer.get(), floatSample, block);
      len -= block;
      s += block;

      if (TrackProgress(count, ( s - start ).as_double() /
                        originalLen.as_double()))
      {
         bLoopSuccess = false;
         break;
      }
   }

   if(bLoopSuccess)
   {
      // mM-1 samples of 'tail' left in lastWindow, get them now
      if(wcopy < (mM - 1)) {
         // Still have some overlap left to process
         // (note that lastWindow and thisWindow have been exchanged at this point
         //  so that 'thisWindow' is really the window prior to 'lastWindow')
         size_t j = 0;
         for(; j < mM - 1 - wcopy; j++)
            buffer[j] = lastWindow[wcopy + j] + thisWindow[L + wcopy + j];
         // And fill in the remainder after the overlap
         for( ; j < mM - 1; j++)
            buffer[j] = lastWindow[wcopy + j];
      } else {
         for(size_t j = 0; j < mM - 1; j++)
            buffer[j] = lastWindow[wcopy + j];
      }
      output->Append((samplePtr)buffer.get(), floatSample, mM - 1);
      output->Flush();

      // now move the appropriate bit of the output back to the track
      // (this could be enhanced in the future to use the tails)
      double offsetT0 = t->LongSamplesToTime(offset);
      double lenT = t->LongSamplesToTime(originalLen);
      // 'start' is the sample offset in 't', the passed in track
      // 'startT' is the equivalent time value
      // 'output' starts at zero
      double startT = t->LongSamplesToTime(start);

      //output has one waveclip for the total length, even though
      //t might have whitespace separating multiple clips
      //we want to maintain the original clip structure, so
      //only paste the intersections of the NEW clip.

      //Find the bits of clips that need replacing
      std::vector<std::pair<double, double> > clipStartEndTimes;
      std::vector<std::pair<double, double> > clipRealStartEndTimes; //the above may be truncated due to a clip being partially selected
      for (const auto &clip : t->GetClips())
      {
         double clipStartT;
         double clipEndT;

         clipStartT = clip->GetPlayStartTime();
         clipEndT = clip->GetPlayEndTime();
         if( clipEndT <= startT )
            continue;   // clip is not within selection
         if( clipStartT >= startT + lenT )
            continue;   // clip is not within selection

         //save the actual clip start/end so that we can rejoin them after we paste.
         clipRealStartEndTimes.push_back(std::pair<double,double>(clipStartT,clipEndT));

         if( clipStartT < startT )  // does selection cover the whole clip?
            clipStartT = startT; // don't copy all the NEW clip
         if( clipEndT > startT + lenT )  // does selection cover the whole clip?
            clipEndT = startT + lenT; // don't copy all the NEW clip

         //save them
         clipStartEndTimes.push_back(std::pair<double,double>(clipStartT,clipEndT));
      }
      //now go thru and replace the old clips with NEW
      for(unsigned int i = 0; i < clipStartEndTimes.size(); i++)
      {
         //remove the old audio and get the NEW
         t->Clear(clipStartEndTimes[i].first,clipStartEndTimes[i].second);
         auto toClipOutput = output->Copy(clipStartEndTimes[i].first-startT+offsetT0,clipStartEndTimes[i].second-startT+offsetT0);
         //put the processed audio in
         t->Paste(clipStartEndTimes[i].first, toClipOutput.get());
         //if the clip was only partially selected, the Paste will have created a split line.  Join is needed to take care of this
         //This is not true when the selection is fully contained within one clip (second half of conditional)
         if( (clipRealStartEndTimes[i].first  != clipStartEndTimes[i].first ||
            clipRealStartEndTimes[i].second != clipStartEndTimes[i].second) &&
            !(clipRealStartEndTimes[i].first <= startT &&
            clipRealStartEndTimes[i].second >= startT+lenT) )
            t->Join(clipRealStartEndTimes[i].first,clipRealStartEndTimes[i].second);
      }
   }

   return bLoopSuccess;
}
#endif

bool EqualizationFilter::CalcFilter()
{
   // Inverse-transform the given curve from frequency domain to time;
   // Apply a taper to define a finite impulse response;
   // Transform that back to frequency domain to get the modified curve.

   double loLog = log10(mLoFreq);
   double hiLog = log10(mHiFreq);
   double denom = hiLog - loLog;

   double delta = mHiFreq / ((double)(mWindowSize / 2.));
   double val0;
   double val1;

   if ( IsLinear() )
   {
      val0 = mLinEnvelope->GetValue(0.0);   //no scaling required - saved as dB
      val1 = mLinEnvelope->GetValue(1.0);
   }
   else
   {
      val0 = mLogEnvelope->GetValue(0.0);   //no scaling required - saved as dB
      val1 = mLogEnvelope->GetValue(1.0);
   }
   mFilterFuncR[0] = val0;
   double freq = delta;

   for(size_t i = 1; i <= mWindowSize / 2; i++)
   {
      double when;
      if ( IsLinear() )
         when = freq/mHiFreq;
      else
         when = (log10(freq) - loLog)/denom;
      if(when < 0.)
      {
         mFilterFuncR[i] = val0;
      }
      else  if(when > 1.0)
      {
         mFilterFuncR[i] = val1;
      }
      else
      {
         if ( IsLinear() )
            mFilterFuncR[i] = mLinEnvelope->GetValue(when);
         else
            mFilterFuncR[i] = mLogEnvelope->GetValue(when);
      }
      freq += delta;
   }
   mFilterFuncR[mWindowSize / 2] = val1;

   mFilterFuncR[0] = DB_TO_LINEAR(mFilterFuncR[0]);

   {
      size_t i = 1;
      for(; i < mWindowSize / 2; i++)
      {
         mFilterFuncR[i] = DB_TO_LINEAR(mFilterFuncR[i]);
         mFilterFuncR[mWindowSize - i] = mFilterFuncR[i];   //Fill entire array
      }
      mFilterFuncR[i] = DB_TO_LINEAR(mFilterFuncR[i]);   //do last one
   }

   //transfer to time domain to do the padding and windowing
   Floats outr{ mWindowSize };
   Floats outi{ mWindowSize };
   InverseRealFFT(mWindowSize, mFilterFuncR.get(), NULL, outr.get()); // To time domain

   {
      size_t i = 0;
      for(; i <= (mM - 1) / 2; i++)
      {  //Windowing - could give a choice, fixed for now - MJS
         //      double mult=0.54-0.46*cos(2*M_PI*(i+(mM-1)/2.0)/(mM-1));   //Hamming
         //Blackman
         double mult =
            0.42 -
            0.5 * cos(2 * M_PI * (i + (mM - 1) / 2.0) / (mM - 1)) +
            .08 * cos(4 * M_PI * (i + (mM - 1) / 2.0) / (mM - 1));
         outr[i] *= mult;
         if(i != 0){
            outr[mWindowSize - i] *= mult;
         }
      }
      for(; i <= mWindowSize / 2; i++)
      {   //Padding
         outr[i] = 0;
         outr[mWindowSize - i] = 0;
      }
   }
   Floats tempr{ mM };
   {
      size_t i = 0;
      for(; i < (mM - 1) / 2; i++)
      {   //shift so that padding on right
         tempr[(mM - 1) / 2 + i] = outr[i];
         tempr[i] = outr[mWindowSize - (mM - 1) / 2 + i];
      }
      tempr[(mM - 1) / 2 + i] = outr[i];
   }

   for (size_t i = 0; i < mM; i++)
   {   //and copy useful values back
      outr[i] = tempr[i];
   }
   for (size_t i = mM; i < mWindowSize; i++)
   {   //rest is padding
      outr[i]=0.;
   }

   //Back to the frequency domain so we can use it
   RealFFT(mWindowSize, outr.get(), mFilterFuncR.get(), mFilterFuncI.get());

   return TRUE;
}

void EqualizationFilter::Filter(size_t len, float *buffer) const
{
   // Transform a window of the time-domain signal to frequency;
   // Multiply by corresponding coefficients;
   // Inverse transform back to time domain:  that's fast convolution.

   float re,im;
   // Apply FFT
   RealFFTf(buffer, hFFT.get());
   //FFT(len, false, inr, NULL, outr, outi);

   // Apply filter
   // DC component is purely real
   mFFTBuffer[0] = buffer[0] * mFilterFuncR[0];
   for(size_t i = 1; i < (len / 2); i++)
   {
      re=buffer[hFFT->BitReversed[i]  ];
      im=buffer[hFFT->BitReversed[i]+1];
      mFFTBuffer[2*i  ] = re*mFilterFuncR[i] - im*mFilterFuncI[i];
      mFFTBuffer[2*i+1] = re*mFilterFuncI[i] + im*mFilterFuncR[i];
   }
   // Fs/2 component is purely real
   mFFTBuffer[1] = buffer[1] * mFilterFuncR[len/2];

   // Inverse FFT and normalization
   InverseRealFFTf(mFFTBuffer.get(), hFFT.get());
   ReorderToTime(hFFT.get(), mFFTBuffer.get(), buffer);
}
