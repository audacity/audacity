/**********************************************************************

  Audacity: A Digital Audio Editor

  Effect/ScienFilter.cpp

  Norm C
  Mitch Golden
  Vaughan Johnson (Preview)

*******************************************************************//**

\file ScienFilter.cpp
\brief Implements EffectScienFilter, EffectScienFilterPanel.

*//****************************************************************//**

\class EffectScienFilter
\brief An Effect that applies 'classical' IIR filters.

  Performs IIR filtering that emulates analog filters, specifically
  Butterworth, Chebyshev Type I and Type II. Highpass and lowpass filters
  are supported, as are filter orders from 1 to 10.

  The filter is applied using biquads

*//****************************************************************//**

\class EffectScienFilterPanel
\brief EffectScienFilterPanel is used with EffectScienFilter and controls
a graph for EffectScienFilter.

*//*******************************************************************/

#include "../Audacity.h"
#include "ScienFilter.h"

#include <math.h>
#include <float.h>

#include <wx/brush.h>
#include <wx/dcmemory.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/settings.h>
#include <wx/utils.h>
#include <wx/valgen.h>

#include "../AColor.h"
#include "../AllThemeResources.h"
#include "../PlatformCompatibility.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../ShuttleGui.h"
#include "../Theme.h"
#include "../WaveTrack.h"
#include "../widgets/valnum.h"

#include "Equalization.h" // For SliderAx

#if !defined(M_PI)
#define PI = 3.1415926535897932384626433832795
#else
#define PI M_PI
#endif
#define square(a) ((a)*(a))

enum
{
   ID_FilterPanel = 10000,
   ID_dBMax,
   ID_dBMin,
   ID_Type,
   ID_SubType,
   ID_Order,
   ID_Ripple,
   ID_Cutoff,
   ID_StopbandRipple
};

enum kTypes
{
   kButterworth,
   kChebyshevTypeI,
   kChebyshevTypeII,
   kNumTypes
};

static const wxChar *kTypeStrings[] =
{
   /*i18n-hint: Butterworth is the name of the person after whom the filter type is named.*/
   XO("Butterworth"),
   /*i18n-hint: Chebyshev is the name of the person after whom the filter type is named.*/
   XO("Chebyshev Type I"),
   /*i18n-hint: Chebyshev is the name of the person after whom the filter type is named.*/
   XO("Chebyshev Type II")
};

enum kSubTypes
{
   kLowPass,
   kHighPass,
   kNumSubTypes
};

static const wxChar *kSubTypeStrings[] =
{
   XO("Lowpass"),
   XO("Highpass")
};

// Define keys, defaults, minimums, and maximums for the effect parameters
//
//     Name       Type     Key                     Def            Min   Max               Scale
Param( Type,      int,     XO("FilterType"),       kButterworth,  0,    kNumTypes - 1,    1  );
Param( Subtype,   int,     XO("FilterSubtype"),    kLowPass,      0,    kNumSubTypes - 1, 1  );
Param( Order,     int,     XO("Order"),            1,             1,    10,               1  );
Param( Cutoff,    float,   XO("Cutoff"),           1000.0,        1.0,  FLT_MAX,          1  );
Param( Passband,  float,   XO("PassbandRipple"),   1.0,           0.0,  100.0,            1  );
Param( Stopband,  float,   XO("StopbandRipple"),   30.0,          0.0,  100.0,            1  );

static const double s_fChebyCoeffs[MAX_Order][MAX_Order + 1] =
{
   // For Chebyshev polynomials of the first kind (see http://en.wikipedia.org/wiki/Chebyshev_polynomial)
   // Coeffs are in the order 0, 1, 2...9
   { 0,  1},        // order 1
   {-1,  0,   2},   // order 2 etc.
   { 0, -3,   0,    4},
   { 1,  0,  -8,    0,    8},
   { 0,  5,   0,  -20,    0,   16},
   {-1,  0,  18,    0,  -48,    0,   32},
   { 0, -7,   0,   56,    0, -112,    0,   64},
   { 1,  0, -32,    0,  160,    0, -256,    0,  128},
   { 0,  9,   0, -120,    0,  432,    0, -576,    0,   256},
   {-1,  0,  50,    0, -400,    0, 1120,    0, -1280,    0, 512}
};

//----------------------------------------------------------------------------
// EffectScienFilter
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(EffectScienFilter, wxEvtHandler)
   EVT_SIZE(EffectScienFilter::OnSize)

   EVT_SLIDER(ID_dBMax, EffectScienFilter::OnSliderDBMAX)
   EVT_SLIDER(ID_dBMin, EffectScienFilter::OnSliderDBMIN)
   EVT_CHOICE(ID_Order, EffectScienFilter::OnOrder)
   EVT_CHOICE(ID_Type, EffectScienFilter::OnFilterType)
   EVT_CHOICE(ID_SubType, EffectScienFilter::OnFilterSubtype)
   EVT_TEXT(ID_Cutoff, EffectScienFilter::OnCutoff)
   EVT_TEXT(ID_Ripple, EffectScienFilter::OnRipple)
   EVT_TEXT(ID_StopbandRipple, EffectScienFilter::OnStopbandRipple)
END_EVENT_TABLE()

EffectScienFilter::EffectScienFilter()
{
   mOrder = DEF_Order;
   mFilterType = DEF_Type;
   mFilterSubtype = DEF_Subtype;
   mCutoff = DEF_Cutoff;
   mRipple = DEF_Passband;
   mStopbandRipple = DEF_Stopband;

   SetLinearEffectFlag(true);

   mOrderIndex = mOrder - 1;

   mdBMin = -30.0;
   mdBMax = 30.0;

   mLoFreq = 20;              // Lowest frequency to display in response graph
   mNyquist = 44100.0 / 2.0;  // only used during initialization, updated when effect is used

   mpBiquad = new BiquadStruct[MAX_Order / 2];
   memset(mpBiquad, 0, sizeof(BiquadStruct) * MAX_Order / 2);
   for (int i = 0; i < MAX_Order / 2; i++)
   {
      mpBiquad[i].fNumerCoeffs[0] = 1.0;	// straight-through
   }
}

EffectScienFilter::~EffectScienFilter()
{
   delete [] mpBiquad;
}

// IdentInterface implementation

wxString EffectScienFilter::GetSymbol()
{
   return CLASSICFILTERS_PLUGIN_SYMBOL;
}

wxString EffectScienFilter::GetDescription()
{
   return XO("Performs IIR filtering that emulates analog filters");
}

// EffectIdentInterface implementation

EffectType EffectScienFilter::GetType()
{
   return EffectTypeProcess;
}

// EffectClientInterface implementation

unsigned EffectScienFilter::GetAudioInCount()
{
   return 1;
}

unsigned EffectScienFilter::GetAudioOutCount()
{
   return 1;
}

bool EffectScienFilter::ProcessInitialize(sampleCount WXUNUSED(totalLen), ChannelNames WXUNUSED(chanMap))
{
   for (int iPair = 0; iPair < (mOrder + 1) / 2; iPair++)
   {
      mpBiquad[iPair].fPrevIn = 0;
      mpBiquad[iPair].fPrevPrevIn = 0;
      mpBiquad[iPair].fPrevOut = 0;
      mpBiquad[iPair].fPrevPrevOut = 0;
   }

   return true;
}

size_t EffectScienFilter::ProcessBlock(float **inBlock, float **outBlock, size_t blockLen)
{
   float *ibuf = inBlock[0];
   for (int iPair = 0; iPair < (mOrder + 1) / 2; iPair++)
   {
      mpBiquad[iPair].pfIn = ibuf;
      mpBiquad[iPair].pfOut = outBlock[0];
      Biquad_Process(&mpBiquad[iPair], blockLen);
      ibuf = outBlock[0];
   }

   return blockLen;
}

bool EffectScienFilter::GetAutomationParameters(EffectAutomationParameters & parms)
{
   parms.Write(KEY_Type, kTypeStrings[mFilterType]);
   parms.Write(KEY_Subtype, kSubTypeStrings[mFilterSubtype]);
   parms.Write(KEY_Order, mOrder);
   parms.WriteFloat(KEY_Cutoff, mCutoff);
   parms.WriteFloat(KEY_Passband, mRipple);
   parms.WriteFloat(KEY_Stopband, mStopbandRipple);

   return true;
}

bool EffectScienFilter::SetAutomationParameters(EffectAutomationParameters & parms)
{
   ReadAndVerifyEnum(Type, wxArrayString(kNumTypes, kTypeStrings));
   ReadAndVerifyEnum(Subtype, wxArrayString(kNumSubTypes, kSubTypeStrings));
   ReadAndVerifyInt(Order);
   ReadAndVerifyFloat(Cutoff);
   ReadAndVerifyFloat(Passband);
   ReadAndVerifyFloat(Stopband);

   mFilterType = Type;
   mFilterSubtype = Subtype;
   mOrder = Order;
   mCutoff = Cutoff;
   mRipple = Passband;
   mStopbandRipple = Stopband;

   mOrderIndex = mOrder - 1;

   CalcFilter();

   return true;
}

// Effect implementation

bool EffectScienFilter::Startup()
{
   wxString base = wxT("/SciFilter/");

   // Migrate settings from 2.1.0 or before

   // Already migrated, so bail
   if (gPrefs->Exists(base + wxT("Migrated")))
   {
      return true;
   }

   // Load the old "current" settings
   if (gPrefs->Exists(base))
   {
	   double dTemp;
      gPrefs->Read(base + wxT("Order"), &mOrder, 1);
      mOrder = wxMax (1, mOrder);
      mOrder = wxMin (MAX_Order, mOrder);
      gPrefs->Read(base + wxT("FilterType"), &mFilterType, 0);
      mFilterType = wxMax (0, mFilterType);
      mFilterType = wxMin (2, mFilterType);
      gPrefs->Read(base + wxT("FilterSubtype"), &mFilterSubtype, 0);
      mFilterSubtype = wxMax (0, mFilterSubtype);
      mFilterSubtype = wxMin (1, mFilterSubtype);
      gPrefs->Read(base + wxT("Cutoff"), &dTemp, 1000.0);
      mCutoff = (float)dTemp;
      mCutoff = wxMax (1, mCutoff);
      mCutoff = wxMin (100000, mCutoff);
      gPrefs->Read(base + wxT("Ripple"), &dTemp, 1.0);
      mRipple = dTemp;
      mRipple = wxMax (0, mRipple);
      mRipple = wxMin (100, mRipple);
      gPrefs->Read(base + wxT("StopbandRipple"), &dTemp, 30.0);
      mStopbandRipple = dTemp;
      mStopbandRipple = wxMax (0, mStopbandRipple);
      mStopbandRipple = wxMin (100, mStopbandRipple);

      SaveUserPreset(GetCurrentSettingsGroup());

      // Do not migrate again
      gPrefs->Write(base + wxT("Migrated"), true);
      gPrefs->Flush();
   }

   return true;
}

bool EffectScienFilter::Init()
{
   int selcount = 0;
   double rate = 0.0;

   TrackListOfKindIterator iter(Track::Wave, mTracks);
   WaveTrack *t = (WaveTrack *) iter.First();

   mNyquist = (t ? t->GetRate() : GetActiveProject()->GetRate()) / 2.0;

   while (t)
   {
      if (t->GetSelected())
      {
         if (selcount == 0)
         {
            rate = t->GetRate();
         }
         else
         {
            if (t->GetRate() != rate)
            {
               wxMessageBox(_("To apply a filter, all selected tracks must have the same sample rate."));
               return false;
            }
         }
         selcount++;
      }
      t = (WaveTrack *) iter.Next();
   }

   return true;
}

void EffectScienFilter::PopulateOrExchange(ShuttleGui & S)
{
   wxWindow *const parent = S.GetParent();

   S.AddSpace(5);
   S.SetSizerProportion(1);
   S.StartMultiColumn(3, wxEXPAND);
   {
      S.SetStretchyCol(1);
      S.SetStretchyRow(0);

      // -------------------------------------------------------------------
      // ROW 1: Freq response panel and sliders for vertical scale
      // -------------------------------------------------------------------

      S.StartVerticalLay();
      {
         mdBRuler = safenew RulerPanel(parent, wxID_ANY);
         mdBRuler->ruler.SetBounds(0, 0, 100, 100); // Ruler can't handle small sizes
         mdBRuler->ruler.SetOrientation(wxVERTICAL);
         mdBRuler->ruler.SetRange(30.0, -120.0);
         mdBRuler->ruler.SetFormat(Ruler::LinearDBFormat);
         mdBRuler->ruler.SetUnits(_("dB"));
         mdBRuler->ruler.SetLabelEdges(true);
         int w;
         mdBRuler->ruler.GetMaxSize(&w, NULL);
         mdBRuler->SetSize(wxSize(w, 150));  // height needed for wxGTK

         S.SetBorder(1);
         S.AddSpace(1, 1);
         S.Prop(1);
         S.AddWindow(mdBRuler, wxALIGN_RIGHT | wxTOP);
         S.AddSpace(1, 1);
      }
      S.EndVerticalLay();

      mPanel = safenew EffectScienFilterPanel(this, parent);
      mPanel->SetFreqRange(mLoFreq, mNyquist);

      S.SetBorder(5);
      S.Prop(1);
      S.AddWindow(mPanel, wxEXPAND | wxRIGHT);
      S.SetSizeHints(-1, -1);

      S.StartVerticalLay();
      {
         S.AddVariableText(_("+ dB"), false, wxCENTER);
         S.SetStyle(wxSL_VERTICAL | wxSL_INVERSE);
         mdBMaxSlider = S.Id(ID_dBMax).AddSlider(wxT(""), 10, 20, 0);
#if wxUSE_ACCESSIBILITY
         mdBMaxSlider->SetName(_("Max dB"));
         mdBMaxSlider->SetAccessible(safenew SliderAx(mdBMaxSlider, wxString(wxT("%d ")) + _("dB")));
#endif

         S.SetStyle(wxSL_VERTICAL | wxSL_INVERSE);
         mdBMinSlider = S.Id(ID_dBMin).AddSlider(wxT(""), -10, -10, -120);
         S.AddVariableText(_("- dB"), false, wxCENTER);
#if wxUSE_ACCESSIBILITY
         mdBMinSlider->SetName(_("Min dB"));
         mdBMinSlider->SetAccessible(safenew SliderAx(mdBMinSlider, wxString(wxT("%d ")) + _("dB")));
#endif
      }
      S.EndVerticalLay();

      // -------------------------------------------------------------------
      // ROW 2: Frequency ruler
      // -------------------------------------------------------------------

      S.AddSpace(1, 1);

      mfreqRuler  = safenew RulerPanel(parent, wxID_ANY);
      mfreqRuler->ruler.SetBounds(0, 0, 100, 100); // Ruler can't handle small sizes
      mfreqRuler->ruler.SetOrientation(wxHORIZONTAL);
      mfreqRuler->ruler.SetLog(true);
      mfreqRuler->ruler.SetRange(mLoFreq, mNyquist);
      mfreqRuler->ruler.SetFormat(Ruler::IntFormat);
      mfreqRuler->ruler.SetUnits(wxT(""));
      mfreqRuler->ruler.SetFlip(true);
      mfreqRuler->ruler.SetLabelEdges(true);
      int h;
      mfreqRuler->ruler.GetMaxSize(NULL, &h);
      mfreqRuler->SetMinSize(wxSize(-1, h));

      S.Prop(1);
      S.AddWindow(mfreqRuler, wxEXPAND | wxALIGN_LEFT | wxRIGHT);

      S.AddSpace(1, 1);

      // -------------------------------------------------------------------
      // ROW 3 and 4: Type, Order, Ripple, Subtype, Cutoff
      // -------------------------------------------------------------------

      S.AddSpace(1, 1);
      S.SetSizerProportion(0);
      S.StartMultiColumn(8, wxALIGN_CENTER);
      {
         wxASSERT(kNumTypes == WXSIZEOF(kTypeStrings));

         wxArrayString typeChoices;
         for (int i = 0; i < kNumTypes; i++)
         {
            typeChoices.Add(wxGetTranslation(kTypeStrings[i]));
         }

         mFilterTypeCtl = S.Id(ID_Type).AddChoice(_("&Filter Type:"), wxT(""), &typeChoices);
         mFilterTypeCtl->SetValidator(wxGenericValidator(&mFilterType));
         S.SetSizeHints(-1, -1);

         wxArrayString orders;
         for (int i = 1; i <= 10; i++)
         {
            orders.Add(wxString::Format(wxT("%d"), i));
         }
         /*i18n-hint: 'Order' means the complexity of the filter, and is a number between 1 and 10.*/
         mFilterOrderCtl = S.Id(ID_Order).AddChoice(_("O&rder:"), wxT(""), &orders);
         mFilterOrderCtl->SetValidator(wxGenericValidator(&mOrderIndex));
         S.SetSizeHints(-1, -1);
         S.AddSpace(1, 1);

         FloatingPointValidator<float> vldRipple(1, &mRipple);
         vldRipple.SetRange(MIN_Passband, MAX_Passband);
         
         mRippleCtlP = S.AddVariableText(_("&Passband Ripple:"), false, wxALL | wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         mRippleCtl = S.Id(ID_Ripple).AddTextBox(wxT(""), wxT(""), 10);
         mRippleCtl->SetName(_("Passband Ripple (dB)"));
         mRippleCtl->SetValidator(vldRipple);
         mRippleCtlU = S.AddVariableText(_("dB"), false, wxALL | wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         wxASSERT(kNumSubTypes == WXSIZEOF(kSubTypeStrings));

         wxArrayString subTypeChoices;
         for (int i = 0; i < kNumSubTypes; i++)
         {
            subTypeChoices.Add(wxGetTranslation(kSubTypeStrings[i]));
         }

         mFilterSubTypeCtl = S.Id(ID_SubType).AddChoice(_("&Subtype:"), wxT(""), &subTypeChoices);
         mFilterSubTypeCtl->SetValidator(wxGenericValidator(&mFilterSubtype));
         S.SetSizeHints(-1, -1);
      
         FloatingPointValidator<float> vldCutoff(1, &mCutoff);
         vldCutoff.SetRange(MIN_Cutoff, mNyquist - 1);
         
         mCutoffCtl = S.Id(ID_Cutoff).AddTextBox(_("C&utoff:"), wxT(""), 10);
         mCutoffCtl->SetName(_("Cutoff (Hz)"));
         mCutoffCtl->SetValidator(vldCutoff);
         S.AddUnits(_("Hz"));

         FloatingPointValidator<float> vldStopbandRipple(1, &mStopbandRipple);
         vldStopbandRipple.SetRange(MIN_Stopband, MAX_Stopband);
         
         mStopbandRippleCtlP = S.AddVariableText(_("Minimum S&topband Attenuation:"), false, wxALL | wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         mStopbandRippleCtl = S.Id(ID_StopbandRipple).AddTextBox(wxT(""), wxT(""), 10);
         mStopbandRippleCtl->SetName(_("Minimum S&topband Attenuation (dB)"));
         mStopbandRippleCtl->SetValidator(vldStopbandRipple);
         mStopbandRippleCtlU = S.AddVariableText(_("dB"), false, wxALL | wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);
      }
      S.EndMultiColumn();
      S.AddSpace(1, 1);
   }
   S.EndMultiColumn();

   mFilterTypeCtl->SetFocus();

   return;
}

//
// Populate the window with relevant variables
//
bool EffectScienFilter::TransferDataToWindow()
{
   mOrderIndex = mOrder - 1;

   if (!mUIParent->TransferDataToWindow())
   {
      return false;
   }

   mdBMinSlider->SetValue((int) mdBMin);
   mdBMin = 0.0;                     // force refresh in TransferGraphLimitsFromWindow()

   mdBMaxSlider->SetValue((int) mdBMax);
   mdBMax = 0.0;                    // force refresh in TransferGraphLimitsFromWindow()

   EnableDisableRippleCtl(mFilterType);

   return TransferGraphLimitsFromWindow();
}

bool EffectScienFilter::TransferDataFromWindow()
{
   if (!mUIParent->Validate() || !mUIParent->TransferDataFromWindow())
   {
      return false;
   }

   mOrder = mOrderIndex + 1;

   CalcFilter();

   return true;
}

// EffectScienFilter implementation

//
// Retrieve data from the window
//
bool EffectScienFilter::TransferGraphLimitsFromWindow()
{
   // Read the sliders and send to the panel
   wxString tip;

   bool rr = false;
   int dB = mdBMinSlider->GetValue();
   if (dB != mdBMin) {
      rr = true;
      mdBMin = dB;
      tip.Printf(wxString(wxT("%d ")) + _("dB"), (int)mdBMin);
      mdBMinSlider->SetToolTip(tip);
   }

   dB = mdBMaxSlider->GetValue();
   if (dB != mdBMax) {
      rr = true;
      mdBMax = dB;
      tip.Printf(wxString(wxT("%d ")) + _("dB"),(int)mdBMax);
      mdBMaxSlider->SetToolTip(tip);
   }

   if (rr) {
      mPanel->SetDbRange(mdBMin, mdBMax);
   }

   // Refresh ruler if values have changed
   if (rr) {
      int w1, w2, h;
      mdBRuler->ruler.GetMaxSize(&w1, &h);
      mdBRuler->ruler.SetRange(mdBMax, mdBMin);
      mdBRuler->ruler.GetMaxSize(&w2, &h);
      if( w1 != w2 )   // Reduces flicker
      {
         mdBRuler->SetSize(wxSize(w2,h));
         mUIParent->Layout();
         mfreqRuler->Refresh(false);
      }
      mdBRuler->Refresh(false);
   }

   mPanel->Refresh(false);

   return true;
}

bool EffectScienFilter::CalcFilter()
{
   // Set up the coefficients in all the biquads
   float fNorm = mCutoff / mNyquist;
   if (fNorm >= 0.9999)
      fNorm = 0.9999F;
   float fC = tan (PI * fNorm / 2);
   float fDCPoleDistSqr = 1.0F;
   float fZPoleX, fZPoleY;
   float fZZeroX, fZZeroY;
   float beta = cos (fNorm*PI);
   switch (mFilterType)
   {
   case kButterworth:     // Butterworth
      if ((mOrder & 1) == 0)
      {
         // Even order
         for (int iPair = 0; iPair < mOrder/2; iPair++)
         {
            float fSPoleX = fC * cos (PI - (iPair + 0.5) * PI / mOrder);
            float fSPoleY = fC * sin (PI - (iPair + 0.5) * PI / mOrder);
            BilinTransform (fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
            mpBiquad[iPair].fNumerCoeffs [0] = 1;
            if (mFilterSubtype == kLowPass)		// LOWPASS
               mpBiquad[iPair].fNumerCoeffs [1] = 2;
            else
               mpBiquad[iPair].fNumerCoeffs [1] = -2;
            mpBiquad[iPair].fNumerCoeffs [2] = 1;
            mpBiquad[iPair].fDenomCoeffs [0] = -2 * fZPoleX;
            mpBiquad[iPair].fDenomCoeffs [1] = square(fZPoleX) + square(fZPoleY);
            if (mFilterSubtype == kLowPass)		// LOWPASS
               fDCPoleDistSqr *= Calc2D_DistSqr (1, 0, fZPoleX, fZPoleY);
            else
               fDCPoleDistSqr *= Calc2D_DistSqr (-1, 0, fZPoleX, fZPoleY);		// distance from Nyquist
         }
      }
      else
      {
         // Odd order - first do the 1st-order section
         float fSPoleX = -fC;
         float fSPoleY = 0;
         BilinTransform (fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
         mpBiquad[0].fNumerCoeffs [0] = 1;
         if (mFilterSubtype == kLowPass)		// LOWPASS
            mpBiquad[0].fNumerCoeffs [1] = 1;
         else
            mpBiquad[0].fNumerCoeffs [1] = -1;
         mpBiquad[0].fNumerCoeffs [2] = 0;
         mpBiquad[0].fDenomCoeffs [0] = -fZPoleX;
         mpBiquad[0].fDenomCoeffs [1] = 0;
         if (mFilterSubtype == kLowPass)		// LOWPASS
            fDCPoleDistSqr = 1 - fZPoleX;
         else
            fDCPoleDistSqr = fZPoleX + 1;    // dist from Nyquist
         for (int iPair = 1; iPair <= mOrder/2; iPair++)
         {
            float fSPoleX = fC * cos (PI - iPair * PI / mOrder);
            float fSPoleY = fC * sin (PI - iPair * PI / mOrder);
            BilinTransform (fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
            mpBiquad[iPair].fNumerCoeffs [0] = 1;
            if (mFilterSubtype == kLowPass)		// LOWPASS
               mpBiquad[iPair].fNumerCoeffs [1] = 2;
            else
               mpBiquad[iPair].fNumerCoeffs [1] = -2;
            mpBiquad[iPair].fNumerCoeffs [2] = 1;
            mpBiquad[iPair].fDenomCoeffs [0] = -2 * fZPoleX;
            mpBiquad[iPair].fDenomCoeffs [1] = square(fZPoleX) + square(fZPoleY);
            if (mFilterSubtype == kLowPass)		// LOWPASS
               fDCPoleDistSqr *= Calc2D_DistSqr (1, 0, fZPoleX, fZPoleY);
            else
               fDCPoleDistSqr *= Calc2D_DistSqr (-1, 0, fZPoleX, fZPoleY);		// distance from Nyquist
         }
      }
      mpBiquad[0].fNumerCoeffs [0] *= fDCPoleDistSqr / (1 << mOrder);	// mult by DC dist from poles, divide by dist from zeroes
      mpBiquad[0].fNumerCoeffs [1] *= fDCPoleDistSqr / (1 << mOrder);
      mpBiquad[0].fNumerCoeffs [2] *= fDCPoleDistSqr / (1 << mOrder);
      break;

   case kChebyshevTypeI:     // Chebyshev Type 1
      double eps; eps = sqrt (pow (10.0, wxMax(0.001, mRipple) / 10.0) - 1);
      double a; a = log (1 / eps + sqrt(1 / square(eps) + 1)) / mOrder;
      // Assume even order to start
      for (int iPair = 0; iPair < mOrder/2; iPair++)
      {
         float fSPoleX = -fC * sinh (a) * sin ((2*iPair + 1) * PI / (2 * mOrder));
         float fSPoleY = fC * cosh (a) * cos ((2*iPair + 1) * PI / (2 * mOrder));
         BilinTransform (fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
         if (mFilterSubtype == kLowPass)		// LOWPASS
         {
            fZZeroX = -1;
            fDCPoleDistSqr = Calc2D_DistSqr (1, 0, fZPoleX, fZPoleY);
            fDCPoleDistSqr /= 2*2;  // dist from zero at Nyquist
         }
         else
         {
            // Highpass - do the digital LP->HP transform on the poles and zeroes
            ComplexDiv (beta - fZPoleX, -fZPoleY, 1 - beta * fZPoleX, -beta * fZPoleY, &fZPoleX, &fZPoleY);
            fZZeroX = 1;
            fDCPoleDistSqr = Calc2D_DistSqr (-1, 0, fZPoleX, fZPoleY);		// distance from Nyquist
            fDCPoleDistSqr /= 2*2;  // dist from zero at Nyquist
         }
         mpBiquad[iPair].fNumerCoeffs [0] = fDCPoleDistSqr;
         mpBiquad[iPair].fNumerCoeffs [1] = -2 * fZZeroX * fDCPoleDistSqr;
         mpBiquad[iPair].fNumerCoeffs [2] = fDCPoleDistSqr;
         mpBiquad[iPair].fDenomCoeffs [0] = -2 * fZPoleX;
         mpBiquad[iPair].fDenomCoeffs [1] = square(fZPoleX) + square(fZPoleY);
      }
      if ((mOrder & 1) == 0)
      {
         float fTemp = DB_TO_LINEAR(-wxMax(0.001, mRipple));      // at DC the response is down R dB (for even-order)
         mpBiquad[0].fNumerCoeffs [0] *= fTemp;
         mpBiquad[0].fNumerCoeffs [1] *= fTemp;
         mpBiquad[0].fNumerCoeffs [2] *= fTemp;
      }
      else
      {
         // Odd order - now do the 1st-order section
         float fSPoleX = -fC * sinh (a);
         float fSPoleY = 0;
         BilinTransform (fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
         if (mFilterSubtype == kLowPass)		// LOWPASS
         {
            fZZeroX = -1;
            fDCPoleDistSqr = sqrt(Calc2D_DistSqr (1, 0, fZPoleX, fZPoleY));
            fDCPoleDistSqr /= 2;  // dist from zero at Nyquist
         }
         else
         {
            // Highpass - do the digital LP->HP transform on the poles and zeroes
            ComplexDiv (beta - fZPoleX, -fZPoleY, 1 - beta * fZPoleX, -beta * fZPoleY, &fZPoleX, &fZPoleY);
            fZZeroX = 1;
            fDCPoleDistSqr = sqrt(Calc2D_DistSqr (-1, 0, fZPoleX, fZPoleY));		// distance from Nyquist
            fDCPoleDistSqr /= 2;  // dist from zero at Nyquist
         }
         mpBiquad[(mOrder-1)/2].fNumerCoeffs [0] = fDCPoleDistSqr;
         mpBiquad[(mOrder-1)/2].fNumerCoeffs [1] = -fZZeroX * fDCPoleDistSqr;
         mpBiquad[(mOrder-1)/2].fNumerCoeffs [2] = 0;
         mpBiquad[(mOrder-1)/2].fDenomCoeffs [0] = -fZPoleX;
         mpBiquad[(mOrder-1)/2].fDenomCoeffs [1] = 0;
      }
      break;

   case kChebyshevTypeII:     // Chebyshev Type 2
      float fSZeroX, fSZeroY;
      float fSPoleX, fSPoleY;
      eps = DB_TO_LINEAR(-wxMax(0.001, mStopbandRipple));
      a = log (1 / eps + sqrt(1 / square(eps) + 1)) / mOrder;

      // Assume even order
      for (int iPair = 0; iPair < mOrder/2; iPair++)
      {
         ComplexDiv (fC, 0, -sinh (a) * sin ((2*iPair + 1) * PI / (2 * mOrder)),
            cosh (a) * cos ((2*iPair + 1) * PI / (2 * mOrder)),
            &fSPoleX, &fSPoleY);
         BilinTransform (fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
         fSZeroX = 0;
         fSZeroY = fC / cos (((2 * iPair) + 1) * PI / (2 * mOrder));
         BilinTransform (fSZeroX, fSZeroY, &fZZeroX, &fZZeroY);

         if (mFilterSubtype == kLowPass)		// LOWPASS
         {
            fDCPoleDistSqr = Calc2D_DistSqr (1, 0, fZPoleX, fZPoleY);
            fDCPoleDistSqr /= Calc2D_DistSqr (1, 0, fZZeroX, fZZeroY);
         }
         else
         {
            // Highpass - do the digital LP->HP transform on the poles and zeroes
            ComplexDiv (beta - fZPoleX, -fZPoleY, 1 - beta * fZPoleX, -beta * fZPoleY, &fZPoleX, &fZPoleY);
            ComplexDiv (beta - fZZeroX, -fZZeroY, 1 - beta * fZZeroX, -beta * fZZeroY, &fZZeroX, &fZZeroY);
            fDCPoleDistSqr = Calc2D_DistSqr (-1, 0, fZPoleX, fZPoleY);		// distance from Nyquist
            fDCPoleDistSqr /= Calc2D_DistSqr (-1, 0, fZZeroX, fZZeroY);
         }
         mpBiquad[iPair].fNumerCoeffs [0] = fDCPoleDistSqr;
         mpBiquad[iPair].fNumerCoeffs [1] = -2 * fZZeroX * fDCPoleDistSqr;
         mpBiquad[iPair].fNumerCoeffs [2] = (square(fZZeroX) + square(fZZeroY)) * fDCPoleDistSqr;
         mpBiquad[iPair].fDenomCoeffs [0] = -2 * fZPoleX;
         mpBiquad[iPair].fDenomCoeffs [1] = square(fZPoleX) + square(fZPoleY);
      }
      // Now, if it's odd order, we have one more to do
      if (mOrder & 1)
      {
         int iPair = (mOrder-1)/2; // we'll do it as a biquad, but it's just first-order
         ComplexDiv (fC, 0, -sinh (a) * sin ((2*iPair + 1) * PI / (2 * mOrder)),
            cosh (a) * cos ((2*iPair + 1) * PI / (2 * mOrder)),
            &fSPoleX, &fSPoleY);
         BilinTransform (fSPoleX, fSPoleY, &fZPoleX, &fZPoleY);
         fZZeroX = -1;     // in the s-plane, the zero is at infinity
         fZZeroY = 0;
         if (mFilterSubtype == kLowPass)		// LOWPASS
         {
            fDCPoleDistSqr = sqrt(Calc2D_DistSqr (1, 0, fZPoleX, fZPoleY));
            fDCPoleDistSqr /= 2;
         }
         else
         {
            // Highpass - do the digital LP->HP transform on the poles and zeroes
            ComplexDiv (beta - fZPoleX, -fZPoleY, 1 - beta * fZPoleX, -fZPoleY, &fZPoleX, &fZPoleY);
            fZZeroX = 1;
            fDCPoleDistSqr = sqrt(Calc2D_DistSqr (-1, 0, fZPoleX, fZPoleY));		// distance from Nyquist
            fDCPoleDistSqr /= 2;
         }
         mpBiquad[iPair].fNumerCoeffs [0] = fDCPoleDistSqr;
         mpBiquad[iPair].fNumerCoeffs [1] = -fZZeroX * fDCPoleDistSqr;
         mpBiquad[iPair].fNumerCoeffs [2] = 0;
         mpBiquad[iPair].fDenomCoeffs [0] = -fZPoleX;
         mpBiquad[iPair].fDenomCoeffs [1] = 0;
      }
      break;
   }
   return true;
}

double EffectScienFilter::ChebyPoly(int Order, double NormFreq)   // NormFreq = 1 at the f0 point (where response is R dB down)
{
   // Calc cosh (Order * acosh (NormFreq));
   double x = 1;
   double fSum = 0;
   wxASSERT (Order >= MIN_Order && Order <= MAX_Order);
   for (int i = 0; i <= Order; i++)
   {
      fSum += s_fChebyCoeffs [Order-1][i] * x;
      x *= NormFreq;
   }
   return fSum;
}

float EffectScienFilter::FilterMagnAtFreq(float Freq)
{
   float Magn;
   if (Freq >= mNyquist)
      Freq = mNyquist - 1;	// prevent tan(PI/2)
   float FreqWarped = tan (PI * Freq/(2*mNyquist));
   if (mCutoff >= mNyquist)
      mCutoff = mNyquist - 1;
   float CutoffWarped = tan (PI * mCutoff/(2*mNyquist));
   float fOverflowThresh = pow (10.0, 12.0 / (2*mOrder));    // once we exceed 10^12 there's not much to be gained and overflow could happen

   switch (mFilterType)
   {
   case kButterworth:		// Butterworth
   default:
      switch (mFilterSubtype)
      {
      case kLowPass:	// lowpass
      default:
         if (FreqWarped/CutoffWarped > fOverflowThresh)	// prevent pow() overflow
            Magn = 0;
         else
            Magn = sqrt (1 / (1 + pow (FreqWarped/CutoffWarped, 2*mOrder)));
         break;
      case kHighPass:	// highpass
         if (FreqWarped/CutoffWarped > fOverflowThresh)
            Magn = 1;
         else
            Magn = sqrt (pow (FreqWarped/CutoffWarped, 2*mOrder) / (1 + pow (FreqWarped/CutoffWarped, 2*mOrder)));
         break;
      }
      break;

   case kChebyshevTypeI:     // Chebyshev Type 1
      double eps; eps = sqrt(pow (10.0, wxMax(0.001, mRipple)/10.0) - 1);
      switch (mFilterSubtype)
      {
      case 0:	// lowpass
      default:
         Magn = sqrt (1 / (1 + square(eps) * square(ChebyPoly(mOrder, FreqWarped/CutoffWarped))));
         break;
      case 1:
         Magn = sqrt (1 / (1 + square(eps) * square(ChebyPoly(mOrder, CutoffWarped/FreqWarped))));
         break;
      }
      break;

   case kChebyshevTypeII:     // Chebyshev Type 2
      eps = 1 / sqrt(pow (10.0, wxMax(0.001, mStopbandRipple)/10.0) - 1);
      switch (mFilterSubtype)
      {
      case kLowPass:	// lowpass
      default:
         Magn = sqrt (1 / (1 + 1 / (square(eps) * square(ChebyPoly(mOrder, CutoffWarped/FreqWarped)))));
         break;
      case kHighPass:
         Magn = sqrt (1 / (1 + 1 / (square(eps) * square(ChebyPoly(mOrder, FreqWarped/CutoffWarped)))));
         break;
      }
      break;
   }

   return Magn;
}

void EffectScienFilter::OnOrder(wxCommandEvent & WXUNUSED(evt))
{
   mOrderIndex = mFilterOrderCtl->GetSelection();
   mOrder = mOrderIndex + 1;	// 0..n-1 -> 1..n
   mPanel->Refresh(false);
}

void EffectScienFilter::OnFilterType(wxCommandEvent & WXUNUSED(evt))
{
   mFilterType = mFilterTypeCtl->GetSelection();
   EnableDisableRippleCtl(mFilterType);
   mPanel->Refresh(false);
}

void EffectScienFilter::OnFilterSubtype(wxCommandEvent & WXUNUSED(evt))
{
   mFilterSubtype = mFilterSubTypeCtl->GetSelection();
   mPanel->Refresh(false);
}

void EffectScienFilter::OnCutoff(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mPanel->Refresh(false);
}

void EffectScienFilter::OnRipple(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mPanel->Refresh(false);
}

void EffectScienFilter::OnStopbandRipple(wxCommandEvent & WXUNUSED(evt))
{
   if (!EnableApply(mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mPanel->Refresh(false);
}

void EffectScienFilter::OnSliderDBMIN(wxCommandEvent & WXUNUSED(evt))
{
   TransferGraphLimitsFromWindow();
}

void EffectScienFilter::OnSliderDBMAX(wxCommandEvent & WXUNUSED(evt))
{
   TransferGraphLimitsFromWindow();
}

void EffectScienFilter::OnSize(wxSizeEvent & evt)
{
   // On Windows the Passband and Stopband boxes do not refresh properly
   // on a resize...no idea why.
   mUIParent->Refresh();
   evt.Skip();
}

void EffectScienFilter::EnableDisableRippleCtl(int FilterType)
{
   bool ripple;
   bool stop;

   if (FilterType == kButterworth)    // Butterworth
   {
      ripple = false;
      stop = false;
   }
   else if (FilterType == kChebyshevTypeI)    // Chebyshev Type1
   {
      ripple = true;
      stop = false;
   }
   else                        // Chebyshev Type2
   {
      ripple = false;
      stop = true;
   }

   mRippleCtlP->Enable(ripple);
   mRippleCtl->Enable(ripple);
   mRippleCtlU->Enable(ripple);
   mStopbandRippleCtlP->Enable(stop);
   mStopbandRippleCtl->Enable(stop);
   mStopbandRippleCtlU->Enable(stop);
}

//----------------------------------------------------------------------------
// EffectScienFilterPanel
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(EffectScienFilterPanel, wxPanelWrapper)
    EVT_PAINT(EffectScienFilterPanel::OnPaint)
    EVT_SIZE(EffectScienFilterPanel::OnSize)
END_EVENT_TABLE()

EffectScienFilterPanel::EffectScienFilterPanel(EffectScienFilter *effect, wxWindow *parent)
:  wxPanelWrapper(parent, wxID_ANY, wxDefaultPosition, wxSize(400, 200))
{
   mEffect = effect;
   mParent = parent;

   mBitmap = NULL;
   mWidth = 0;
   mHeight = 0;
   mLoFreq = 0.0;
   mHiFreq = 0.0;
   mDbMin = 0.0;
   mDbMax = 0.0;
}

EffectScienFilterPanel::~EffectScienFilterPanel()
{
}

void EffectScienFilterPanel::SetFreqRange(double lo, double hi)
{
   mLoFreq = lo;
   mHiFreq = hi;
   Refresh(false);
}

void EffectScienFilterPanel::SetDbRange(double min, double max)
{
   mDbMin = min;
   mDbMax = max;
   Refresh(false);
}

bool EffectScienFilterPanel::AcceptsFocus() const
{
   return false;
}

bool EffectScienFilterPanel::AcceptsFocusFromKeyboard() const
{
   return false;
}

void EffectScienFilterPanel::OnSize(wxSizeEvent & WXUNUSED(evt))
{
   Refresh(false);
}

void EffectScienFilterPanel::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
   wxPaintDC dc(this);
   int width, height;
   GetSize(&width, &height);

   if (!mBitmap || mWidth != width || mHeight != height)
   {
      mWidth = width;
      mHeight = height;
      mBitmap = std::make_unique<wxBitmap>(mWidth, mHeight);
   }

   wxBrush bkgndBrush(wxSystemSettings::GetColour(wxSYS_COLOUR_3DFACE));

   wxMemoryDC memDC;
   memDC.SelectObject(*mBitmap);

   wxRect bkgndRect;
   bkgndRect.x = 0;
   bkgndRect.y = 0;
   bkgndRect.width = mWidth;
   bkgndRect.height = mHeight;
   memDC.SetBrush(bkgndBrush);
   memDC.SetPen(*wxTRANSPARENT_PEN);
   memDC.DrawRectangle(bkgndRect);

   bkgndRect.y = mHeight;
   memDC.DrawRectangle(bkgndRect);

   wxRect border;
   border.x = 0;
   border.y = 0;
   border.width = mWidth;
   border.height = mHeight;

   memDC.SetBrush(*wxWHITE_BRUSH);
   memDC.SetPen(*wxBLACK_PEN);
   memDC.DrawRectangle(border);

   mEnvRect = border;
   mEnvRect.Deflate(2, 2);

   // Pure blue x-axis line
   memDC.SetPen(wxPen(theTheme.Colour(clrGraphLines), 1, wxSOLID));
   int center = (int) (mEnvRect.height * mDbMax / (mDbMax - mDbMin) + 0.5);
   AColor::Line(memDC,
                mEnvRect.GetLeft(), mEnvRect.y + center,
                mEnvRect.GetRight(), mEnvRect.y + center);

   //Now draw the actual response that you will get.
   //mFilterFunc has a linear scale, window has a log one so we have to fiddle about
   memDC.SetPen(wxPen(theTheme.Colour(clrResponseLines), 3, wxSOLID));
   double scale = (double) mEnvRect.height / (mDbMax - mDbMin);    // pixels per dB
   double yF;                                                     // gain at this freq

   double loLog = log10(mLoFreq);
   double step = log10(mHiFreq) - loLog;
   step /= ((double) mEnvRect.width - 1.0);
   double freq;                                    // actual freq corresponding to x position
   int x, y, xlast = 0, ylast = 0;
   for (int i = 0; i < mEnvRect.width; i++)
   {
      x = mEnvRect.x + i;
      freq = pow(10.0, loLog + i * step);          //Hz
      yF = mEffect->FilterMagnAtFreq (freq);
      yF = LINEAR_TO_DB(yF);

      if (yF < mDbMin)
      {
         yF = mDbMin;
      }

      yF = center-scale * yF;
      if (yF > mEnvRect.height)
      {
         yF = (double) mEnvRect.height - 1.0;
      }
      if (yF < 0.0)
      {
         yF = 0.0;
      }
      y = (int) (yF + 0.5);

      if (i != 0 && (y < mEnvRect.height - 1 || ylast < mEnvRect.y + mEnvRect.height - 1))
      {
         AColor::Line(memDC, xlast, ylast, x, mEnvRect.y + y);
      }
      xlast = x;
      ylast = mEnvRect.y + y;
   }

   memDC.SetPen(*wxBLACK_PEN);
   mEffect->mfreqRuler->ruler.DrawGrid(memDC, mEnvRect.height + 2, true, true, 0, 1);
   mEffect->mdBRuler->ruler.DrawGrid(memDC, mEnvRect.width + 2, true, true, 1, 2);

   dc.Blit(0, 0, mWidth, mHeight, &memDC, 0, 0, wxCOPY, FALSE);

   memDC.SelectObject(wxNullBitmap);
}
