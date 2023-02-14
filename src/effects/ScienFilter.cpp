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


#include "ScienFilter.h"
#include "LoadEffects.h"

#include <math.h>

#include <wx/setup.h> // for wxUSE_* macros

#include <wx/brush.h>
#include <wx/choice.h>
#include <wx/dcclient.h>
#include <wx/dcmemory.h>
#include <wx/settings.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/utils.h>
#include <wx/valgen.h>

#include "AColor.h"
#include "AllThemeResources.h"
#include "PlatformCompatibility.h"
#include "Prefs.h"
#include "Project.h"
#include "ShuttleGui.h"
#include "Theme.h"
#include "WaveTrack.h"
#include "valnum.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/Ruler.h"
#include "WindowAccessible.h"

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

const EnumValueSymbol EffectScienFilter::kTypeStrings[nTypes] =
{
   /*i18n-hint: Butterworth is the name of the person after whom the filter type is named.*/
   { XO("Butterworth") },
   /*i18n-hint: Chebyshev is the name of the person after whom the filter type is named.*/
   { XO("Chebyshev Type I") },
   /*i18n-hint: Chebyshev is the name of the person after whom the filter type is named.*/
   { XO("Chebyshev Type II") }
};

const EnumValueSymbol EffectScienFilter::kSubTypeStrings[nSubTypes] =
{
   // These are acceptable dual purpose internal/visible names
   { XO("Lowpass") },
   { XO("Highpass") }
};

const EffectParameterMethods& EffectScienFilter::Parameters() const
{
   static CapturedParameters<EffectScienFilter,
      Type, Subtype, Order, Cutoff, Passband, Stopband
   > parameters{
      [](EffectScienFilter &, EffectSettings &, EffectScienFilter &e,
         bool updating){
         if (updating) {
            e.mOrderIndex = e.mOrder - 1;
            e.CalcFilter();
         }
         return true;
      },
   };
   return parameters;
}

//----------------------------------------------------------------------------
// EffectScienFilter
//----------------------------------------------------------------------------

const ComponentInterfaceSymbol EffectScienFilter::Symbol
{ XO("Classic Filters") };

#ifdef EXPERIMENTAL_SCIENCE_FILTERS
// true argument means don't automatically enable this effect
namespace{ BuiltinEffectsModule::Registration< EffectScienFilter > reg( true ); }
#endif

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
   Parameters().Reset(*this);
   SetLinearEffectFlag(true);

   mOrderIndex = mOrder - 1;

   mdBMin = -30.0;
   mdBMax = 30.0;

   mLoFreq = 20;              // Lowest frequency to display in response graph
   mNyquist = 44100.0 / 2.0;  // only used during initialization, updated when effect is used
}

EffectScienFilter::~EffectScienFilter()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol EffectScienFilter::GetSymbol() const
{
   return Symbol;
}

TranslatableString EffectScienFilter::GetDescription() const
{
   /* i18n-hint: "infinite impulse response" */
   return XO("Performs IIR filtering that emulates analog filters");
}

ManualPageID EffectScienFilter::ManualPage() const
{
   return L"Classic_Filters";
}


// EffectDefinitionInterface implementation

EffectType EffectScienFilter::GetType() const
{
   return EffectTypeProcess;
}

unsigned EffectScienFilter::GetAudioInCount() const
{
   return 1;
}

unsigned EffectScienFilter::GetAudioOutCount() const
{
   return 1;
}

bool EffectScienFilter::ProcessInitialize(
   EffectSettings &, double, ChannelNames chanMap)
{
   for (int iPair = 0; iPair < (mOrder + 1) / 2; iPair++)
      mpBiquad[iPair].Reset();
   return true;
}

size_t EffectScienFilter::ProcessBlock(EffectSettings &,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   const float *ibuf = inBlock[0];
   for (int iPair = 0; iPair < (mOrder + 1) / 2; iPair++)
   {
      mpBiquad[iPair].Process(ibuf, outBlock[0], blockLen);
      ibuf = outBlock[0];
   }

   return blockLen;
}

// Effect implementation

bool EffectScienFilter::Init()
{
   int selcount = 0;
   double rate = 0.0;

   auto trackRange = inputTracks()->Selected< const WaveTrack >();

   {
      auto t = *trackRange.begin();
      mNyquist =
         (t
            ? t->GetRate()
            : mProjectRate)
         / 2.0;
   }

   for (auto t : trackRange)
   {
      if (selcount == 0)
      {
         rate = t->GetRate();
      }
      else
      {
         if (t->GetRate() != rate)
         {
            Effect::MessageBox(
               XO(
"To apply a filter, all selected tracks must have the same sample rate.") );
            return false;
         }
      }
      selcount++;
   }

   return true;
}

std::unique_ptr<EffectUIValidator> EffectScienFilter::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &,
   const EffectOutputs *)
{
   mUIParent = S.GetParent();
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
         mdBRuler = safenew RulerPanel(
            S.GetParent(), wxID_ANY, wxVERTICAL,
            wxSize{ 100, 100 }, // Ruler can't handle small sizes
            RulerPanel::Range{ 30.0, -120.0 },
            Ruler::LinearDBFormat,
            XO("dB"),
            RulerPanel::Options{}
               .LabelEdges(true)
         );

         S.SetBorder(1);
         S.AddSpace(1, 1);
         S.Prop(1)
            .Position(wxALIGN_RIGHT | wxTOP)
            .AddWindow(mdBRuler);
         S.AddSpace(1, 1);
      }
      S.EndVerticalLay();

      mPanel = safenew EffectScienFilterPanel(
         S.GetParent(), wxID_ANY,
         this, mLoFreq, mNyquist
      );

      S.SetBorder(5);
      S.Prop(1)
         .Position(wxEXPAND | wxRIGHT)
         .MinSize( { -1, -1 } )
         .AddWindow(mPanel);

      S.StartVerticalLay();
      {
         S.AddVariableText(XO("+ dB"), false, wxCENTER);
         mdBMaxSlider = S.Id(ID_dBMax)
            .Name(XO("Max dB"))
            .Style(wxSL_VERTICAL | wxSL_INVERSE)
            .AddSlider( {}, 10, 20, 0);
#if wxUSE_ACCESSIBILITY
         mdBMaxSlider->SetAccessible(safenew SliderAx(mdBMaxSlider, XO("%d dB")));
#endif
         mdBMinSlider = S.Id(ID_dBMin)
            .Name(XO("Min dB"))
            .Style(wxSL_VERTICAL | wxSL_INVERSE)
            .AddSlider( {}, -10, -10, -120);
#if wxUSE_ACCESSIBILITY
         mdBMinSlider->SetAccessible(safenew SliderAx(mdBMinSlider, XO("%d dB")));
#endif

         S.AddVariableText(XO("- dB"), false, wxCENTER);
      }
      S.EndVerticalLay();

      // -------------------------------------------------------------------
      // ROW 2: Frequency ruler
      // -------------------------------------------------------------------

      S.AddSpace(1, 1);

      mfreqRuler = safenew RulerPanel(
         S.GetParent(), wxID_ANY, wxHORIZONTAL,
         wxSize{ 100, 100 }, // Ruler can't handle small sizes
         RulerPanel::Range{ mLoFreq, mNyquist },
         Ruler::IntFormat,
         {},
         RulerPanel::Options{}
            .Log(true)
            .Flip(true)
            .LabelEdges(true)
      );

      S.Prop(1)
         .Position(wxEXPAND | wxALIGN_LEFT | wxRIGHT)
         .AddWindow(mfreqRuler);

      S.AddSpace(1, 1);

      // -------------------------------------------------------------------
      // ROW 3 and 4: Type, Order, Ripple, Subtype, Cutoff
      // -------------------------------------------------------------------

      S.AddSpace(1, 1);
      S.SetSizerProportion(0);
      S.StartMultiColumn(8, wxALIGN_CENTER);
      {
         wxASSERT(nTypes == WXSIZEOF(kTypeStrings));

         mFilterTypeCtl = S.Id(ID_Type)
            .Focus()
            .Validator<wxGenericValidator>(&mFilterType)
            .MinSize( { -1, -1 } )
            .AddChoice(XXO("&Filter Type:"),
               Msgids(kTypeStrings, nTypes)
            );

         mFilterOrderCtl = S.Id(ID_Order)
            .Validator<wxGenericValidator>(&mOrderIndex)
            .MinSize( { -1, -1 } )
            /*i18n-hint: 'Order' means the complexity of the filter, and is a number between 1 and 10.*/
            .AddChoice(XXO("O&rder:"),
               []{
                  TranslatableStrings orders;
                  for (int i = 1; i <= 10; i++)
                     orders.emplace_back( Verbatim("%d").Format( i ) );
                  return orders;
               }()
            );
         S.AddSpace(1, 1);

         mRippleCtlP = S.AddVariableText( XO("&Passband Ripple:"),
            false, wxALL | wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         mRippleCtl = S.Id(ID_Ripple)
            .Name(XO("Passband Ripple (dB)"))
            .Validator<FloatingPointValidator<float>>(
               1, &mRipple, NumValidatorStyle::DEFAULT,
               Passband.min, Passband.max)
            .AddTextBox( {}, L"", 10);
         mRippleCtlU = S.AddVariableText(XO("dB"),
            false, wxALL | wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);

         mFilterSubTypeCtl = S.Id(ID_SubType)
            .Validator<wxGenericValidator>(&mFilterSubtype)
            .MinSize( { -1, -1 } )
            .AddChoice(XXO("&Subtype:"),
               Msgids(kSubTypeStrings, nSubTypes)
            );

         mCutoffCtl = S.Id(ID_Cutoff)
            .Name(XO("Cutoff (Hz)"))
            .Validator<FloatingPointValidator<float>>(
               1, &mCutoff, NumValidatorStyle::DEFAULT,
               Cutoff.min, mNyquist - 1)
            .AddTextBox(XXO("C&utoff:"), L"", 10);
         S.AddUnits(XO("Hz"));

         mStopbandRippleCtlP =
            S.AddVariableText(XO("Minimum S&topband Attenuation:"),
            false, wxALL | wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL);
         mStopbandRippleCtl = S.Id(ID_StopbandRipple)
            .Name(XO("Minimum S&topband Attenuation (dB)"))
            .Validator<FloatingPointValidator<float>>(
               1, &mStopbandRipple, NumValidatorStyle::DEFAULT,
               Stopband.min, Stopband.max)
            .AddTextBox( {}, L"", 10);
         mStopbandRippleCtlU =
            S.AddVariableText(XO("dB"),
            false, wxALL | wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL);
      }
      S.EndMultiColumn();
      S.AddSpace(1, 1);
   }
   S.EndMultiColumn();

   return nullptr;
}

//
// Populate the window with relevant variables
//
bool EffectScienFilter::TransferDataToWindow(const EffectSettings &)
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

bool EffectScienFilter::TransferDataFromWindow(EffectSettings &)
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
      tip.Printf(_("%d dB"), (int)mdBMin);
      mdBMinSlider->SetToolTip(tip);
   }

   dB = mdBMaxSlider->GetValue();
   if (dB != mdBMax) {
      rr = true;
      mdBMax = dB;
      tip.Printf(_("%d dB"),(int)mdBMax);
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

void EffectScienFilter::CalcFilter()
{
   switch (mFilterType)
   {
   case kButterworth:
      mpBiquad = Biquad::CalcButterworthFilter(mOrder, mNyquist, mCutoff, mFilterSubtype);
      break;
   case kChebyshevTypeI:
      mpBiquad = Biquad::CalcChebyshevType1Filter(mOrder, mNyquist, mCutoff, mRipple, mFilterSubtype);
      break;
   case kChebyshevTypeII:
      mpBiquad = Biquad::CalcChebyshevType2Filter(mOrder, mNyquist, mCutoff, mStopbandRipple, mFilterSubtype);
      break;
   }
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
      double chebyPolyVal;
      switch (mFilterSubtype)
      {
      case 0:	// lowpass
      default:
         chebyPolyVal = Biquad::ChebyPoly(mOrder, FreqWarped/CutoffWarped);
         Magn = sqrt (1 / (1 + square(eps) * square(chebyPolyVal)));
         break;
      case 1:
         chebyPolyVal = Biquad::ChebyPoly(mOrder, CutoffWarped/FreqWarped);
         Magn = sqrt (1 / (1 + square(eps) * square(chebyPolyVal)));
         break;
      }
      break;

   case kChebyshevTypeII:     // Chebyshev Type 2
      eps = 1 / sqrt(pow (10.0, wxMax(0.001, mStopbandRipple)/10.0) - 1);
      switch (mFilterSubtype)
      {
      case kLowPass:	// lowpass
      default:
         chebyPolyVal = Biquad::ChebyPoly(mOrder, CutoffWarped/FreqWarped);
         Magn = sqrt (1 / (1 + 1 / (square(eps) * square(chebyPolyVal))));
         break;
      case kHighPass:
         chebyPolyVal = Biquad::ChebyPoly(mOrder, FreqWarped/CutoffWarped);
         Magn = sqrt (1 / (1 + 1 / (square(eps) * square(chebyPolyVal))));
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
   if (!EffectUIValidator::EnableApply(
      mUIParent, mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mPanel->Refresh(false);
}

void EffectScienFilter::OnRipple(wxCommandEvent & WXUNUSED(evt))
{
   if (!EffectUIValidator::EnableApply(
      mUIParent, mUIParent->TransferDataFromWindow()))
   {
      return;
   }

   mPanel->Refresh(false);
}

void EffectScienFilter::OnStopbandRipple(wxCommandEvent & WXUNUSED(evt))
{
   if (!EffectUIValidator::EnableApply(
      mUIParent, mUIParent->TransferDataFromWindow()))
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

EffectScienFilterPanel::EffectScienFilterPanel(
   wxWindow *parent, wxWindowID winid,
   EffectScienFilter *effect, double lo, double hi)
:  wxPanelWrapper(parent, winid, wxDefaultPosition, wxSize(400, 200))
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

   SetFreqRange(lo, hi);
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
      mBitmap = std::make_unique<wxBitmap>(mWidth, mHeight,24);
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
   memDC.SetPen(wxPen(theTheme.Colour(clrGraphLines), 1, wxPENSTYLE_SOLID));
   int center = (int) (mEnvRect.height * mDbMax / (mDbMax - mDbMin) + 0.5);
   AColor::Line(memDC,
                mEnvRect.GetLeft(), mEnvRect.y + center,
                mEnvRect.GetRight(), mEnvRect.y + center);

   //Now draw the actual response that you will get.
   //mFilterFunc has a linear scale, window has a log one so we have to fiddle about
   memDC.SetPen(wxPen(theTheme.Colour(clrResponseLines), 3, wxPENSTYLE_SOLID));
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
