/**********************************************************************

   Audacity: A Digital Audio Editor

   EqualizationCurves.cpp

   Mitch Golden
   Vaughan Johnson (Preview)
   Martyn Shaw (FIR filters, response curve, graphic EQ)

   Paul Licameli split from Equalization.cpp

**********************************************************************/

#include "EqualizationCurves.h"
#include "EqualizationParameters.h"

#include <wx/log.h>
#include "widgets/AudacityMessageBox.h"
#include "FileNames.h"
#include "XMLFileReader.h"

// Increment whenever EQCurves.xml is updated
#define EQCURVES_VERSION   1
#define EQCURVES_REVISION  0
#define UPDATE_ALL 0 // 0 = merge NEW presets only, 1 = Update all factory presets.

#if 0
static const double kThirdOct[] =
{
   20., 25., 31., 40., 50., 63., 80., 100., 125., 160., 200.,
   250., 315., 400., 500., 630., 800., 1000., 1250., 1600., 2000.,
   2500., 3150., 4000., 5000., 6300., 8000., 10000., 12500., 16000., 20000.,
};

const EffectParameterMethods& EffectEqualization::Parameters() const
{
   static CapturedParameters<EffectEqualization,
      EqualizationParameters::FilterLength,
      // CurveName,
      EqualizationParameters::InterpLin,
      // Pretty sure the interpolation name shouldn't have been interpreted when
      // specified in chains, but must keep it that way for compatibility.
      EqualizationParameters::InterpMeth
   > parameters {
      [](EffectEqualization &effect, EffectSettings &,
         EqualizationParameters &params, bool updating
      ){
         constexpr auto nInterpolations =
            EqualizationParameters::nInterpolations;
         if (updating) {
            if (params.mInterp >= nInterpolations)
               params.mInterp -= nInterpolations;
            effect.mEnvelope = (effect.mParameters.mLin
               ? effect.mLinEnvelope
               : effect.mLogEnvelope
            ).get();
         }
         return true;
      }
   };
   return parameters;
}

///----------------------------------------------------------------------------
// EffectEqualization
//----------------------------------------------------------------------------

const ComponentInterfaceSymbol EffectEqualization::Symbol
{ XO("Equalization") };

// namespace{ BuiltinEffectsModule::Registration< EffectEqualization > reg; }

// "Filter Curve EQ" in the user-facing string, but preserve the old
// internal string
const ComponentInterfaceSymbol EffectEqualizationCurve::Symbol
{ wxT("Filter Curve"), XO("Filter Curve EQ") };

namespace{ BuiltinEffectsModule::Registration< EffectEqualizationCurve > reg2; }

const ComponentInterfaceSymbol EffectEqualizationGraphic::Symbol
{ wxT("Graphic EQ"), XO("Graphic EQ") };

namespace{ BuiltinEffectsModule::Registration< EffectEqualizationGraphic > reg3; }

BEGIN_EVENT_TABLE(EffectEqualization, wxEvtHandler)
   EVT_SIZE( EffectEqualization::OnSize )

   EVT_SLIDER( ID_Length, EffectEqualization::OnSliderM )
   EVT_SLIDER( ID_dBMax, EffectEqualization::OnSliderDBMAX )
   EVT_SLIDER( ID_dBMin, EffectEqualization::OnSliderDBMIN )
   EVT_COMMAND_RANGE(ID_Slider,
                     ID_Slider + NUMBER_OF_BANDS - 1,
                     wxEVT_COMMAND_SLIDER_UPDATED,
                     EffectEqualization::OnSlider)
   EVT_CHOICE( ID_Interp, EffectEqualization::OnInterp )

   EVT_CHOICE( ID_Curve, EffectEqualization::OnCurve )
   EVT_BUTTON( ID_Manage, EffectEqualization::OnManage )
   EVT_BUTTON( ID_Clear, EffectEqualization::OnClear )
   EVT_BUTTON( ID_Invert, EffectEqualization::OnInvert )

   EVT_RADIOBUTTON(ID_Draw, EffectEqualization::OnDrawMode)
   EVT_RADIOBUTTON(ID_Graphic, EffectEqualization::OnGraphicMode)
   EVT_CHECKBOX(ID_Linear, EffectEqualization::OnLinFreq)
   EVT_CHECKBOX(ID_Grid, EffectEqualization::OnGridOnOff)

END_EVENT_TABLE()

EffectEqualization::EffectEqualization(int Options)
   : mFFTBuffer{ windowSize }
   , mFilterFuncR{ windowSize }
   , mFilterFuncI{ windowSize }
   , mParameters{ GetDefinition() }
{
   constexpr auto dBMin = EqualizationParameters::dBMin;
   constexpr auto dBMax = EqualizationParameters::dBMax;

   const auto &mLin = mParameters.mLin;

   Parameters().Reset(*this);

   mOptions = Options;
   mGraphic = NULL;
   mDraw = NULL;
   mCurve = NULL;
   mPanel = NULL;
   mMSlider = NULL;

   hFFT = GetFFT(windowSize);

   SetLinearEffectFlag(true);

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

   mEnvelope = (mLin ? mLinEnvelope : mLogEnvelope).get();

   mWindowSize = windowSize;

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
   mLoFreq = loFreqI;
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
#endif

wxString EQCurveReader::GetPrefsPrefix()
{
   wxString base = wxT("/Effects/Equalization/");
   if( mOptions == kEqOptionGraphic )
      base = wxT("/Effects/GraphicEq/");
   else if( mOptions == kEqOptionCurve )
      base = wxT("/Effects/FilterCurve/");
   return base;
}

#if 0
bool EffectEqualization::Init()
{
   const auto &mLin = mParameters.mLin;
   const auto &mCurveName = mParameters.mCurveName;

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

   CalcFilter();

   return(true);
}

bool EffectEqualization::Process(EffectInstance &, EffectSettings &)
{
   this->CopyInputTracks(); // Set up mOutputTracks.
   CalcFilter();
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

         Filter(windowSize, thisWindow);

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

bool EffectEqualization::CalcFilter()
{
   const auto &mM = mParameters.mM;

   double loLog = log10(mLoFreq);
   double hiLog = log10(mHiFreq);
   double denom = hiLog - loLog;

   double delta = mHiFreq / ((double)(mWindowSize / 2.));
   double val0;
   double val1;

   if( mParameters.IsLinear() )
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
      if( mParameters.IsLinear() )
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
         if( mParameters.IsLinear() )
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

void EffectEqualization::Filter(size_t len, float *buffer)
{
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
#endif

int EQUtils::DoMessageBox(
   const TranslatableString &name,
   const TranslatableString &msg,
   const TranslatableString &titleStr,
   long style)
{
   // Compare with Effect::MessageBox
   auto title = titleStr.empty()
      ? name
      : XO("%s: %s").Format( name, titleStr );
   return AudacityMessageBox( msg, title, style, nullptr );
}

//
// Load external curves with fallback to default, then message
//
void EQCurveReader::LoadCurves(const wxString &fileName, bool append)
{
// We've disabled the XML management of curves.
// Just going via .cfg files now.
#if 1
   (void)fileName;
   (void)append;
   mCurves.clear();
   mCurves.push_back( wxT("unnamed") );   // we still need a default curve to use
#else
   // Construct normal curve filename
   //
   // LLL:  Wouldn't you know that as of WX 2.6.2, there is a conflict
   //       between wxStandardPaths and wxConfig under Linux.  The latter
   //       creates a normal file as "$HOME/.audacity", while the former
   //       expects the ".audacity" portion to be a directory.
   // MJS:  I don't know what the above means, or if I have broken it.
   wxFileName fn;

   if(fileName.empty()) {
      // Check if presets are up to date.
      wxString eqCurvesCurrentVersion = wxString::Format(wxT("%d.%d"), EQCURVES_VERSION, EQCURVES_REVISION);
      wxString eqCurvesInstalledVersion;
      gPrefs->Read(GetPrefsPrefix() + "PresetVersion", &eqCurvesInstalledVersion, wxT(""));

      bool needUpdate = (eqCurvesCurrentVersion != eqCurvesInstalledVersion);

      // UpdateDefaultCurves allows us to import NEW factory presets only,
      // or update all factory preset curves.
      if (needUpdate)
         UpdateDefaultCurves( UPDATE_ALL != 0 );
      fn = wxFileName( FileNames::DataDir(), wxT("EQCurves.xml") );
   }
   else
      fn = fileName; // user is loading a specific set of curves

   // If requested file doesn't exist...
   if( !fn.FileExists() && !GetDefaultFileName(fn) ) {
      mCurves.clear();
      /* i18n-hint: name of the 'unnamed' custom curve */
      mCurves.push_back( _("unnamed") );   // we still need a default curve to use
      return;
   }

   EQCurve tempCustom(wxT("temp"));
   if( append == false ) // Start from scratch
      mCurves.clear();
   else  // appending so copy and remove 'unnamed', to replace later
   {
      tempCustom.points = mCurves.back().points;
      mCurves.pop_back();
   }

   // Load the curves
   XMLFileReader reader;
   const wxString fullPath{ fn.GetFullPath() };
   if( !reader.Parse( this, fullPath ) )
   {
      /* i18n-hint: EQ stands for 'Equalization'.*/
      auto msg = XO("Error Loading EQ Curves from file:\n%s\nError message says:\n%s")
         .Format( fullPath, reader.GetErrorStr() );
      // Inform user of load failure
      EQUtils::DoMessageBox(mName, msg, XO("Error Loading EQ Curves"));
      mCurves.push_back( _("unnamed") );  // we always need a default curve to use
      return;
   }

   // Move "unnamed" to end, if it exists in current language.
   int numCurves = mCurves.size();
   int curve;
   EQCurve tempUnnamed(wxT("tempUnnamed"));
   for( curve = 0; curve < numCurves-1; curve++ )
   {
      if( mCurves[curve].Name == _("unnamed") )
      {
         tempUnnamed.points = mCurves[curve].points;
         mCurves.erase(mCurves.begin() + curve);
         mCurves.push_back( _("unnamed") );   // add 'unnamed' back at the end
         mCurves.back().points = tempUnnamed.points;
      }
   }

   if( mCurves.back().Name != _("unnamed") )
      mCurves.push_back( _("unnamed") );   // we always need a default curve to use
   if( append == true )
   {
      mCurves.back().points = tempCustom.points;
   }
#endif
   return;
}

//
// Update presets to match Audacity version.
//
void EQCurveReader::UpdateDefaultCurves(bool updateAll /* false */)
{
   if (mCurves.size() == 0)
      return;

   wxString unnamed = wxT("unnamed");

   // Save the "unnamed" curve and remove it so we can add it back as the final curve.
   EQCurve userUnnamed(wxT("temp"));
   userUnnamed = mCurves.back();
   mCurves.pop_back();

   EQCurveArray userCurves = mCurves;
   mCurves.clear();
   // We only wamt to look for the shipped EQDefaultCurves.xml
   wxFileName fn = wxFileName(FileNames::ResourcesDir(), wxT("EQDefaultCurves.xml"));
   wxLogDebug(wxT("Attempting to load EQDefaultCurves.xml from %s"),fn.GetFullPath());
   XMLFileReader reader;

   if(!reader.Parse(this, fn.GetFullPath())) {
      wxLogError(wxT("EQDefaultCurves.xml could not be read."));
      return;
   }
   else {
      wxLogDebug(wxT("Loading EQDefaultCurves.xml successful."));
   }

   EQCurveArray defaultCurves = mCurves;
   mCurves.clear(); // clear now so that we can sort then add back.

   // Remove "unnamed" if it exists.
   if (defaultCurves.back().Name == unnamed) {
      defaultCurves.pop_back();
   }
   else {
      wxLogError(wxT("Error in EQDefaultCurves.xml"));
   }

   int numUserCurves = userCurves.size();
   int numDefaultCurves = defaultCurves.size();
   EQCurve tempCurve(wxT("test"));

   if (updateAll) {
      // Update all factory preset curves.
      // Sort and add factory defaults first;
      mCurves = defaultCurves;
      std::sort(mCurves.begin(), mCurves.end());
      // then add remaining user curves:
      for (int curveCount = 0; curveCount < numUserCurves; curveCount++) {
         bool isCustom = true;
         tempCurve = userCurves[curveCount];
         // is the name in the default set?
         for (int defCurveCount = 0; defCurveCount < numDefaultCurves; defCurveCount++) {
            if (tempCurve.Name == mCurves[defCurveCount].Name) {
               isCustom = false;
               break;
            }
         }
         // if tempCurve is not in the default set, add it to mCurves.
         if (isCustom) {
            mCurves.push_back(tempCurve);
         }
      }
   }
   else {
      // Import NEW factory defaults but retain all user modified curves.
      for (int defCurveCount = 0; defCurveCount < numDefaultCurves; defCurveCount++) {
         bool isUserCurve = false;
         // Add if the curve is in the user's set (preserve user's copy)
         for (int userCurveCount = 0; userCurveCount < numUserCurves; userCurveCount++) {
            if (userCurves[userCurveCount].Name == defaultCurves[defCurveCount].Name) {
               isUserCurve = true;
               mCurves.push_back(userCurves[userCurveCount]);
               break;
            }
         }
         if (!isUserCurve) {
            mCurves.push_back(defaultCurves[defCurveCount]);
         }
      }
      std::sort(mCurves.begin(), mCurves.end());
      // now add the rest of the user's curves.
      for (int userCurveCount = 0; userCurveCount < numUserCurves; userCurveCount++) {
         bool isDefaultCurve = false;
         tempCurve = userCurves[userCurveCount];
         for (int defCurveCount = 0; defCurveCount < numDefaultCurves; defCurveCount++) {
            if (tempCurve.Name == defaultCurves[defCurveCount].Name) {
               isDefaultCurve = true;
               break;
            }
         }
         if (!isDefaultCurve) {
            mCurves.push_back(tempCurve);
         }
      }
   }
   defaultCurves.clear();
   userCurves.clear();

   // Add back old "unnamed"
   if(userUnnamed.Name == unnamed) {
      mCurves.push_back( userUnnamed );   // we always need a default curve to use
   }

   EQCurveWriter{ mCurves }.SaveCurves();

   // Write current EqCurve version number
   // TODO: Probably better if we used pluginregistry.cfg
   wxString eqCurvesCurrentVersion = wxString::Format(wxT("%d.%d"), EQCURVES_VERSION, EQCURVES_REVISION);
   gPrefs->Write(GetPrefsPrefix()+"PresetVersion", eqCurvesCurrentVersion);
   gPrefs->Flush();

   return;
}

//
// Get fully qualified filename of EQDefaultCurves.xml
//
bool EQCurveReader::GetDefaultFileName(wxFileName &fileName)
{
   // look in data dir first, in case the user has their own defaults (maybe downloaded ones)
   fileName = wxFileName( FileNames::DataDir(), wxT("EQDefaultCurves.xml") );
   if( !fileName.FileExists() )
   {  // Default file not found in the data dir.  Fall back to Resources dir.
      // See http://docs.wxwidgets.org/trunk/classwx_standard_paths.html#5514bf6288ee9f5a0acaf065762ad95d
      fileName = wxFileName( FileNames::ResourcesDir(), wxT("EQDefaultCurves.xml") );
   }
   if( !fileName.FileExists() )
   {
      // LLL:  Is there really a need for an error message at all???
      //auto errorMessage = XO("EQCurves.xml and EQDefaultCurves.xml were not found on your system.\nPlease press 'help' to visit the download page.\n\nSave the curves at %s")
      //   .Format( FileNames::DataDir() );
      //BasicUI::ShowErrorDialog( wxWidgetsWindowPlacement{ mUIParent },
      //   XO("EQCurves.xml and EQDefaultCurves.xml missing"),
      //   errorMessage, wxT("http://wiki.audacityteam.org/wiki/EQCurvesDownload"), false);

      // Have another go at finding EQCurves.xml in the data dir, in case 'help' helped
      fileName = wxFileName( FileNames::DataDir(), wxT("EQDefaultCurves.xml") );
   }
   return (fileName.FileExists());
}


//
// Save curves to external file
//
void EQCurveWriter::SaveCurves(const wxString &fileName)
{
   wxFileName fn;
   if( fileName.empty() )
   {
      // Construct default curve filename
      //
      // LLL:  Wouldn't you know that as of WX 2.6.2, there is a conflict
      //       between wxStandardPaths and wxConfig under Linux.  The latter
      //       creates a normal file as "$HOME/.audacity", while the former
      //       expects the ".audacity" portion to be a directory.
      fn = wxFileName( FileNames::DataDir(), wxT("EQCurves.xml") );

      // If the directory doesn't exist...
      if( !fn.DirExists() )
      {
         // Attempt to create it
         if( !fn.Mkdir( fn.GetPath(), 511, wxPATH_MKDIR_FULL ) )
         {
            // MkDir() will emit message
            return;
         }
      }
   }
   else
      fn = fileName;

   GuardedCall( [&] {
      // Create/Open the file
      const wxString fullPath{ fn.GetFullPath() };
      XMLFileWriter eqFile{ fullPath, XO("Error Saving Equalization Curves") };

      // Write the curves
      WriteXML( eqFile );

      eqFile.Commit();
   } );
}

#if 0
//
// Make the passed curve index the active one
//
void EffectEqualization::setCurve(int currentCurve)
{
   const auto &mLin = mParameters.mLin;

   // Set current choice
   wxASSERT( currentCurve < (int) mCurves.size() );
   Select(currentCurve);

   Envelope *env;
   int numPoints = (int) mCurves[currentCurve].points.size();

   if (mLin) {  // linear freq mode
      env = mLinEnvelope.get();
   }
   else { // log freq mode
      env = mLogEnvelope.get();
   }
   env->Flatten(0.);
   env->SetTrackLen(1.0);

   // Handle special case of no points.
   if (numPoints == 0) {
      ForceRecalc();
      return;
   }

   double when, value;

   // Handle special case 1 point.
   if (numPoints == 1) {
      // only one point, so ensure it is in range then return.
      when = mCurves[currentCurve].points[0].Freq;
      if (mLin) {
         when = when / mHiFreq;
      }
      else {   // log scale
         // We don't go below loFreqI (20 Hz) in log view.
         double loLog = log10((double)loFreqI);
         double hiLog = log10(mHiFreq);
         double denom = hiLog - loLog;
         when = (log10(std::max((double) loFreqI, when)) - loLog)/denom;
      }
      value = mCurves[currentCurve].points[0].dB;
      env->Insert(std::min(1.0, std::max(0.0, when)), value);
      ForceRecalc();
      return;
   }

   // We have at least two points, so ensure they are in frequency order.
   std::sort(mCurves[currentCurve].points.begin(),
             mCurves[currentCurve].points.end());

   if (mCurves[currentCurve].points[0].Freq < 0) {
      // Corrupt or invalid curve, so bail.
      ForceRecalc();
      return;
   }

   if(mLin) {   // linear Hz scale
      for(int pointCount = 0; pointCount < numPoints; pointCount++) {
         when = mCurves[currentCurve].points[pointCount].Freq / mHiFreq;
         value = mCurves[currentCurve].points[pointCount].dB;
         if(when <= 1) {
            env->Insert(when, value);
            if (when == 1)
               break;
         }
         else {
            // There are more points at higher freqs,
            // so interpolate next one then stop.
            when = 1.0;
            double nextDB = mCurves[currentCurve].points[pointCount].dB;
            if (pointCount > 0) {
               double nextF = mCurves[currentCurve].points[pointCount].Freq;
               double lastF = mCurves[currentCurve].points[pointCount-1].Freq;
               double lastDB = mCurves[currentCurve].points[pointCount-1].dB;
               value = lastDB +
                  ((nextDB - lastDB) *
                     ((mHiFreq - lastF) / (nextF - lastF)));
            }
            else
               value = nextDB;
            env->Insert(when, value);
            break;
         }
      }
   }
   else {   // log Hz scale
      double loLog = log10((double) loFreqI);
      double hiLog = log10(mHiFreq);
      double denom = hiLog - loLog;
      int firstAbove20Hz;

      // log scale EQ starts at 20 Hz (threshold of hearing).
      // so find the first point (if any) above 20 Hz.
      for (firstAbove20Hz = 0; firstAbove20Hz < numPoints; firstAbove20Hz++) {
         if (mCurves[currentCurve].points[firstAbove20Hz].Freq > loFreqI)
            break;
      }

      if (firstAbove20Hz == numPoints) {
         // All points below 20 Hz, so just use final point.
         when = 0.0;
         value = mCurves[currentCurve].points[numPoints-1].dB;
         env->Insert(when, value);
         ForceRecalc();
         return;
      }

      if (firstAbove20Hz > 0) {
         // At least one point is before 20 Hz and there are more
         // beyond 20 Hz, so interpolate the first
         double prevF = mCurves[currentCurve].points[firstAbove20Hz-1].Freq;
         prevF = log10(std::max(1.0, prevF)); // log zero is bad.
         double prevDB = mCurves[currentCurve].points[firstAbove20Hz-1].dB;
         double nextF = log10(mCurves[currentCurve].points[firstAbove20Hz].Freq);
         double nextDB = mCurves[currentCurve].points[firstAbove20Hz].dB;
         when = 0.0;
         value = nextDB - ((nextDB - prevDB) * ((nextF - loLog) / (nextF - prevF)));
         env->Insert(when, value);
      }

      // Now get the rest.
      for(int pointCount = firstAbove20Hz; pointCount < numPoints; pointCount++)
      {
         double flog = log10(mCurves[currentCurve].points[pointCount].Freq);
         wxASSERT(mCurves[currentCurve].points[pointCount].Freq >= loFreqI);

         when = (flog - loLog)/denom;
         value = mCurves[currentCurve].points[pointCount].dB;
         if(when <= 1.0) {
            env->Insert(when, value);
         }
         else {
            // This looks weird when adjusting curve in Draw mode if
            // there is a point off-screen.

            /*
            // we have a point beyond fs/2.  Insert it so that env code can use it.
            // but just this one, we have no use for the rest
            env->SetTrackLen(when); // can't Insert if the envelope isn't long enough
            env->Insert(when, value);
            break;
            */

            // interpolate the final point instead
            when = 1.0;
            if (pointCount > 0) {
               double lastDB = mCurves[currentCurve].points[pointCount-1].dB;
               double logLastF =
                  log10(mCurves[currentCurve].points[pointCount-1].Freq);
               value = lastDB +
                  ((value - lastDB) *
                     ((log10(mHiFreq) - logLastF) / (flog - logLastF)));
            }
            env->Insert(when, value);
            break;
         }
      }
   }
   ForceRecalc();
}

void EffectEqualization::setCurve()
{
   setCurve((int) mCurves.size() - 1);
}

void EffectEqualization::setCurve(const wxString &curveName)
{
   unsigned i = 0;
   for( i = 0; i < mCurves.size(); i++ )
      if( curveName == mCurves[ i ].Name )
         break;
   if( i == mCurves.size())
   {
      Effect::MessageBox(
         XO("Requested curve not found, using 'unnamed'"),
         wxOK|wxICON_ERROR,
         XO("Curve not found") );
      setCurve();
   }
   else
      setCurve( i );
}

//
// Set NEW curve selection (safe to call outside of the UI)
//
void EffectEqualization::Select( int curve )
{
   auto &mCurveName = mParameters.mCurveName;

   // Set current choice
   if (mCurve)
   {
      mCurve->SetSelection( curve );
      mCurveName = mCurves[ curve ].Name;
   }
}

//
// Tell panel to recalc (safe to call outside of UI)
//
void EffectEqualization::ForceRecalc()
{
   if (mPanel)
   {
      mPanel->ForceRecalc();
   }
}

//
// Capture updated envelope
//
void EffectEqualization::EnvelopeUpdated()
{
   if (mParameters.IsLinear())
   {
      EnvelopeUpdated(mLinEnvelope.get(), true);
   }
   else
   {
      EnvelopeUpdated(mLogEnvelope.get(), false);
   }
}

void EffectEqualization::EnvelopeUpdated(Envelope *env, bool lin)
{
   // Allocate and populate point arrays
   size_t numPoints = env->GetNumberOfPoints();
   Doubles when{ numPoints };
   Doubles value{ numPoints };
   env->GetPoints( when.get(), value.get(), numPoints );

   // Clear the unnamed curve
   int curve = mCurves.size() - 1;
   mCurves[ curve ].points.clear();

   if(lin)
   {
      // Copy and convert points
      for (size_t point = 0; point < numPoints; point++)
      {
         double freq = when[ point ] * mHiFreq;
         double db = value[ point ];

         // Add it to the curve
         mCurves[ curve ].points.push_back( EQPoint( freq, db ) );
      }
   }
   else
   {
      double loLog = log10( 20. );
      double hiLog = log10( mHiFreq );
      double denom = hiLog - loLog;

      // Copy and convert points
      for (size_t point = 0; point < numPoints; point++)
      {
         double freq = pow( 10., ( ( when[ point ] * denom ) + loLog ));
         double db = value[ point ];

         // Add it to the curve
         mCurves[ curve ].points.push_back( EQPoint( freq, db ) );
      }
   }
   // Remember that we've updated the unnamed curve
   mDirty = true;

   // set 'unnamed' as the selected curve
   Select( (int) mCurves.size() - 1 );
}

//
// Flatten the curve
//
void EffectEqualization::Flatten()
{
   const auto &mDrawMode = mParameters.mDrawMode;

   mLogEnvelope->Flatten(0.);
   mLogEnvelope->SetTrackLen(1.0);
   mLinEnvelope->Flatten(0.);
   mLinEnvelope->SetTrackLen(1.0);
   ForceRecalc();
   if( !mDrawMode )
   {
      for( size_t i = 0; i < mBandsInUse; i++)
      {
         mSliders[i]->SetValue(0);
         mSlidersOld[i] = 0;
         mEQVals[i] = 0.;

         wxString tip;
         if( kThirdOct[i] < 1000.)
            tip.Printf( wxT("%dHz\n%.1fdB"), (int)kThirdOct[i], 0. );
         else
            tip.Printf( wxT("%gkHz\n%.1fdB"), kThirdOct[i]/1000., 0. );
         mSliders[i]->SetToolTip(tip);
      }
   }
   EnvelopeUpdated();
}
#endif

//
// Process XML tags and handle the ones we recognize
//
bool EQCurveReader::HandleXMLTag(
   const std::string_view& tag, const AttributesList &attrs)
{
   // May want to add a version strings...
   if (tag == "equalizationeffect")
   {
      return true;
   }

   // Located a NEW curve
   if (tag == "curve")
   {
      // Process the attributes
      for (auto pair : attrs)
      {
         auto attr = pair.first;
         auto value = pair.second;

         // Create a NEW curve and name it
         if( attr == "name" )
         {
            const wxString strValue = value.ToWString();
            // check for a duplicate name and add (n) if there is one
            int n = 0;
            wxString strValueTemp = strValue;
            bool exists;
            do
            {
               exists = false;
               for(size_t i = 0; i < mCurves.size(); i++)
               {
                  if(n>0)
                     strValueTemp.Printf(wxT("%s (%d)"),strValue,n);
                  if(mCurves[i].Name == strValueTemp)
                  {
                     exists = true;
                     break;
                  }
               }
               n++;
            }
            while(exists == true);

            mCurves.push_back( EQCurve( strValueTemp ) );
         }
      }

      // Tell caller it was processed
      return true;
   }

   // Located a NEW point
   if(tag == "point")
   {
      // Set defaults in case attributes are missing
      double f = 0.0;
      double d = 0.0;

      // Process the attributes
      double dblValue;
      for (auto pair : attrs)
      {
         auto attr = pair.first;
         auto value = pair.second;

         // Get the frequency
         if( attr == "f" )
         {
            if (!value.TryGet(dblValue))
               return false;
            f = dblValue;
         }
         // Get the dB
         else if( attr == "d" )
         {
            if (!value.TryGet(dblValue))
               return false;
            d = dblValue;
         }
      }

      // Create a NEW point
      mCurves[ mCurves.size() - 1 ].points.push_back( EQPoint( f, d ) );

      // Tell caller it was processed
      return true;
   }

   // Tell caller we didn't understand the tag
   return false;
}

//
// Return handler for recognized tags
//
XMLTagHandler *EQCurveReader::HandleXMLChild(const std::string_view& tag)
{
   if (tag == "equalizationeffect")
   {
      return this;
   }

   if (tag == "curve")
   {
      return this;
   }

   if (tag == "point")
   {
      return this;
   }

   return NULL;
}

//
// Write all of the curves to the XML file
//
void EQCurveWriter::WriteXML(XMLWriter &xmlFile) const
// may throw
{
   // Start our hierarchy
   xmlFile.StartTag( wxT( "equalizationeffect" ) );

   // Write all curves
   int numCurves = mCurves.size();
   int curve;
   for( curve = 0; curve < numCurves; curve++ )
   {
      // Start a NEW curve
      xmlFile.StartTag( wxT( "curve" ) );
      xmlFile.WriteAttr( wxT( "name" ), mCurves[ curve ].Name );

      // Write all points
      int numPoints = mCurves[ curve ].points.size();
      int point;
      for( point = 0; point < numPoints; point++ )
      {
         // Write NEW point
         xmlFile.StartTag( wxT( "point" ) );
         xmlFile.WriteAttr( wxT( "f" ), mCurves[ curve ].points[ point ].Freq, 12 );
         xmlFile.WriteAttr( wxT( "d" ), mCurves[ curve ].points[ point ].dB, 12 );
         xmlFile.EndTag( wxT( "point" ) );
      }

      // Terminate curve
      xmlFile.EndTag( wxT( "curve" ) );
   }

   // Terminate our hierarchy
   xmlFile.EndTag( wxT( "equalizationeffect" ) );
}
