/**********************************************************************

   Audacity: A Digital Audio Editor

   EqualizationUI.cpp

   Mitch Golden
   Vaughan Johnson (Preview)
   Martyn Shaw (FIR filters, response curve, graphic EQ)

   Paul Licameli split from Equalization.cpp

**********************************************************************/
#include "EqualizationUI.h"
#include "EqualizationCurvesDialog.h"
#include "EqualizationPanel.h"
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/checkbox.h>
#include "../ShuttleGui.h"
#include "../widgets/Ruler.h"

#if wxUSE_ACCESSIBILITY
#include "../widgets/WindowAccessible.h"
#endif

BEGIN_EVENT_TABLE(EqualizationUI, wxEvtHandler)
   EVT_SIZE( EqualizationUI::OnSize )
   EVT_IDLE(EqualizationUI::OnIdle)

END_EVENT_TABLE()

bool EqualizationUI::ValidateUI(EffectSettings &)
{
   const auto &mParameters = mCurvesList.mParameters;
   const auto &mCurveName = mParameters.mCurveName;
   const auto &mDrawMode = mParameters.mDrawMode;
   auto &mLogEnvelope = mParameters.mLogEnvelope;
   const auto &mCurves = mCurvesList.mCurves;

   // If editing a macro, we don't want to be using the unnamed curve so
   // we offer to save it.

   if (mDisallowCustom && mCurveName == wxT("unnamed"))
   {
      // PRL:  This is unreachable.  mDisallowCustom is always false.

      EQUtils::DoMessageBox(
         mName,
         XO("To use this filter curve in a macro, please choose a new name for it.\nChoose the 'Save/Manage Curves...' button and rename the 'unnamed' curve, then use that one."),
         XO("Filter Curve EQ needs a different name") );
      return false;
   }

   EQCurveWriter{ mCurves }.SaveCurves();

   mParameters.SaveConfig(mManager);

   return true;
}

std::unique_ptr<EffectUIValidator> EqualizationUI::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &access,
   const EffectOutputs *)
{
   auto &mParameters = mCurvesList.mParameters;
   const auto &mM = mParameters.mM;
   const auto &mLoFreq = mParameters.mLoFreq;
   const auto &mHiFreq = mParameters.mHiFreq;
   const auto &mCurves = mCurvesList.mCurves;

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
            // Inserted into sizer later, but the EQ panel needs to point to it
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

         mParameters.ChooseEnvelope().Flatten(0.);
         mParameters.ChooseEnvelope().SetTrackLen(1.0);
         mPanel = safenew EqualizationPanel(S.GetParent(), wxID_ANY,
            mCurvesList, *mFreqRuler, *mdBRuler);
         S.Prop(1)
            .Position(wxEXPAND)
            .MinSize( { wxDefaultCoord, wxDefaultCoord } )
            .AddWindow(mPanel);

         S.SetBorder(5);
         S.StartVerticalLay();
         {
            S.AddVariableText(XO("+ dB"), false, wxCENTER);
            mdBMaxSlider = S
               .Name(XO("Max dB"))
               .Style(wxSL_VERTICAL | wxSL_INVERSE)
               .AddSlider( {}, 30, 60, 0);
#if wxUSE_ACCESSIBILITY
            mdBMaxSlider->SetAccessible(safenew SliderAx(mdBMaxSlider, XO("%d dB")));
#endif
            BindTo(*mdBMaxSlider, wxEVT_SLIDER,
               &EqualizationUI::OnSliderDBMAX);

            mdBMinSlider = S
               .Name(XO("Min dB"))
               .Style(wxSL_VERTICAL | wxSL_INVERSE)
               .AddSlider( {}, -30, -10, -120);
            S.AddVariableText(XO("- dB"), false, wxCENTER);
#if wxUSE_ACCESSIBILITY
            mdBMinSlider->SetAccessible(safenew SliderAx(mdBMinSlider, XO("%d dB")));
#endif
            BindTo(*mdBMinSlider, wxEVT_SLIDER,
               &EqualizationUI::OnSliderDBMIN);
         }
         S.EndVerticalLay();
         S.SetBorder(0);

         // -------------------------------------------------------------------
         // Frequency ruler below graph
         // -------------------------------------------------------------------

         // Column 1 is empty
         S.AddSpace(1, 1);

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
         S.AddSpace(15,0);
         {
         mBands.AddBandSliders(S);
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
                  mDraw = S
                     .Name(XO("Draw Curves"))
                     .AddRadioButton(XXO("&Draw"));
                  BindTo(*mDraw, wxEVT_RADIOBUTTON,
                     &EqualizationUI::OnDrawMode);

                  mGraphic = S
                     .Name(XO("Graphic EQ"))
                     .AddRadioButtonToGroup(XXO("&Graphic"));
                  BindTo(*mGraphic, wxEVT_RADIOBUTTON,
                     &EqualizationUI::OnGraphicMode);
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

               mInterpChoice = S
                  .Name(XO("Interpolation type"))
                  .AddChoice( {},
                     Msgids(EqualizationParameters::kInterpStrings,
                        EqualizationParameters::nInterpolations),
                     0 );
#if wxUSE_ACCESSIBILITY
               // so that name can be set on a standard control
               mInterpChoice->SetAccessible(safenew WindowAccessible(mInterpChoice));
#endif
               BindTo(*mInterpChoice, wxEVT_CHOICE,
                  &EqualizationUI::OnInterp);
            }
            S.EndHorizontalLay();

            S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL, 1);
            {
               szrL = S.GetSizer();

               mLinFreq = S
                  .Name(XO("Linear Frequency Scale"))
                  .AddCheckBox(XXO("Li&near Frequency Scale"), false);
               BindTo(*mLinFreq, wxEVT_CHECKBOX,
                  &EqualizationUI::OnLinFreq);
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
                  mMSlider = S
                     .Name(XO("Length of Filter"))
                     .Style(wxSL_HORIZONTAL)
                     .AddSlider( {}, (mM - 1) / 2, 4095, 10);
                  BindTo(*mMSlider, wxEVT_SLIDER,
                     &EqualizationUI::OnSliderM);
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
                  mCurve = S
                     .Name(XO("Select Curve"))
                     .AddChoice( {},
                        [&mCurves]{
                           TranslatableStrings curves;
                           for (const auto &curve : mCurves)
                              curves.push_back( Verbatim( curve.Name ) );
                           return curves;
                        }()
                     );
                  BindTo(*mCurve, wxEVT_CHOICE,
                     &EqualizationUI::OnCurve);
               }
               S.EndHorizontalLay();
            }
            S.EndHorizontalLay();

            const auto pButton = S
               .AddButton(XXO("S&ave/Manage Curves..."));
            BindTo(*pButton, wxEVT_BUTTON, &EqualizationUI::OnManage);
         }

         S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL, 1);
         {
            auto pButton = S
               .AddButton(XXO("Fla&tten"));
            BindTo(*pButton, wxEVT_BUTTON, &EqualizationUI::OnClear);

            pButton = S
               .AddButton(XXO("&Invert"));
            BindTo(*pButton, wxEVT_BUTTON, &EqualizationUI::OnInvert);

            mGridOnOff = S
               .Name(XO("Show grid lines"))
               .AddCheckBox(XXO("Show g&rid lines"), false);
            BindTo(*mGridOnOff, wxEVT_CHECKBOX,
               &EqualizationUI::OnGridOnOff);
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
   mCurvesList.ForceRecalc();

   return nullptr;
}

bool EqualizationUI::TransferDataToWindow(const EffectSettings &settings)
{
   auto &mParameters = mCurvesList.mParameters;
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

void EqualizationUI::UpdateRuler()
{
   const auto &mParameters = mCurvesList.mParameters;
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

//
// Make the passed curve index the active one
//
void EqualizationUI::setCurve(int currentCurve)
{
   auto &mParameters = mCurvesList.mParameters;
   constexpr auto loFreqI = EqualizationFilter::loFreqI;

   const auto &mLin = mParameters.mLin;
   const auto &mHiFreq = mParameters.mHiFreq;
   auto &mCurves = mCurvesList.mCurves;

   // Set current choice
   wxASSERT( currentCurve < (int) mCurves.size() );
   mCurvesList.Select(currentCurve);

   int numPoints = (int) mCurves[currentCurve].points.size();

   auto &env = mParameters.ChooseEnvelope();
   env.Flatten(0.);
   env.SetTrackLen(1.0);

   // Handle special case of no points.
   if (numPoints == 0) {
      mCurvesList.ForceRecalc();
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
         when =
            (log10(std::max<double>(loFreqI, when))
             - loLog) / denom;
      }
      value = mCurves[currentCurve].points[0].dB;
      env.Insert(std::min(1.0, std::max(0.0, when)), value);
      mCurvesList.ForceRecalc();
      return;
   }

   // We have at least two points, so ensure they are in frequency order.
   std::sort(mCurves[currentCurve].points.begin(),
             mCurves[currentCurve].points.end());

   if (mCurves[currentCurve].points[0].Freq < 0) {
      // Corrupt or invalid curve, so bail.
      mCurvesList.ForceRecalc();
      return;
   }

   if(mLin) {   // linear Hz scale
      for(int pointCount = 0; pointCount < numPoints; pointCount++) {
         when = mCurves[currentCurve].points[pointCount].Freq / mHiFreq;
         value = mCurves[currentCurve].points[pointCount].dB;
         if(when <= 1) {
            env.Insert(when, value);
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
            env.Insert(when, value);
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
         env.Insert(when, value);
         mCurvesList.ForceRecalc();
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
         env.Insert(when, value);
      }

      // Now get the rest.
      for(int pointCount = firstAbove20Hz; pointCount < numPoints; pointCount++)
      {
         double flog = log10(mCurves[currentCurve].points[pointCount].Freq);
         wxASSERT(mCurves[currentCurve].points[pointCount].Freq >= loFreqI);

         when = (flog - loLog)/denom;
         value = mCurves[currentCurve].points[pointCount].dB;
         if(when <= 1.0) {
            env.Insert(when, value);
         }
         else {
            // This looks weird when adjusting curve in Draw mode if
            // there is a point off-screen.

            /*
            // we have a point beyond fs/2.  Insert it so that env code can use it.
            // but just this one, we have no use for the rest
            env.SetTrackLen(when); // can't Insert if the envelope isn't long enough
            env.Insert(when, value);
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
            env.Insert(when, value);
            break;
         }
      }
   }
   mCurvesList.ForceRecalc();
}

void EqualizationUI::setCurve()
{
   const auto &mCurves = mCurvesList.mCurves;
   setCurve((int) mCurves.size() - 1);
}

void EqualizationUI::setCurve(const wxString &curveName)
{
   const auto &mCurves = mCurvesList.mCurves;
   unsigned i = 0;
   for( i = 0; i < mCurves.size(); i++ )
      if( curveName == mCurves[ i ].Name )
         break;
   if( i == mCurves.size())
   {
      EQUtils::DoMessageBox( mName,
         XO("Requested curve not found, using 'unnamed'"),
         XO("Curve not found"),
         wxOK|wxICON_ERROR);
      setCurve();
   }
   else
      setCurve( i );
}

///////////////////////////////////////////////////////////////////////////////
//
// All EffectEqualization methods beyond this point interact with the UI, so
// can't be called while the UI is not displayed.
//
///////////////////////////////////////////////////////////////////////////////

void EqualizationUI::UpdateCurves()
{
   auto &mParameters = mCurvesList.mParameters;
   auto &mCurveName = mParameters.mCurveName;
   const auto &mCurves = mCurvesList.mCurves;

   // Reload the curve names
   if( mCurve ) 
      mCurve->Clear();
   bool selectedCurveExists = false;
   for (size_t i = 0, cnt = mCurves.size(); i < cnt; i++)
   {
      if (mCurveName == mCurves[ i ].Name)
         selectedCurveExists = true;
      if( mCurve ) 
         mCurve->Append(mCurves[ i ].Name);
   }
   // In rare circumstances, mCurveName may not exist (bug 1891)
   if (!selectedCurveExists)
      mCurveName = mCurves[ (int)mCurves.size() - 1 ].Name;
   if( mCurve ) 
      mCurve->SetStringSelection(mCurveName);
   
   // Allow the control to resize
   if( mCurve ) 
      mCurve->SetMinSize({-1, -1});

   // Set initial curve
   setCurve( mCurveName );
}

void EqualizationUI::UpdateDraw()
{
   auto &mParameters = mCurvesList.mParameters;
   const auto &mLin = mParameters.mLin;
   auto &mLinEnvelope = mParameters.mLinEnvelope;
   auto &mLogEnvelope = mParameters.mLogEnvelope;
   const auto &mHiFreq = mParameters.mHiFreq;

   size_t numPoints = mLogEnvelope.GetNumberOfPoints();
   Doubles when{ numPoints };
   Doubles value{ numPoints };
   double deltadB = 0.1;
   double dx, dy, dx1, dy1, err;

   mLogEnvelope.GetPoints( when.get(), value.get(), numPoints );

   // set 'unnamed' as the selected curve
   mCurvesList.EnvelopeUpdated();

   bool flag = true;
   while (flag)
   {
      flag = false;
      int numDeleted = 0;
      mLogEnvelope.GetPoints( when.get(), value.get(), numPoints );
      for (size_t j = 0; j + 2 < numPoints; j++)
      {
         dx = when[j+2+numDeleted] - when[j+numDeleted];
         dy = value[j+2+numDeleted] - value[j+numDeleted];
         dx1 = when[j+numDeleted+1] - when[j+numDeleted];
         dy1 = dy * dx1 / dx;
         err = fabs(value[j+numDeleted+1] - (value[j+numDeleted] + dy1));
         if( err < deltadB )
         {   // within < deltadB dB?
            mLogEnvelope.Delete(j+1);
            numPoints--;
            numDeleted++;
            flag = true;
         }
      }
   }

   if(mLin) // do not use IsLinear() here
   {
      mBands.EnvLogToLin();
      mFreqRuler->ruler.SetLog(false);
      mFreqRuler->ruler.SetRange(0, mHiFreq);
   }

   szrV->Show(szrG,false);
   szrH->Show(szrI,false);
   szrH->Show(szrL,true);

   mUIParent->Layout();
   wxGetTopLevelParent(mUIParent)->Layout();
   mCurvesList.ForceRecalc();     // it may have changed slightly due to the deletion of points
}

void EqualizationUI::UpdateGraphic()
{
   auto &mParameters = mCurvesList.mParameters;
   const auto &mLin = mParameters.mLin;
   auto &mLinEnvelope = mParameters.mLinEnvelope;
   auto &mLogEnvelope = mParameters.mLogEnvelope;
   const auto &mLoFreq = mParameters.mLoFreq;
   const auto &mHiFreq = mParameters.mHiFreq;

   auto &mDrawMode = mParameters.mDrawMode;

   if(mLin)  //going from lin to log freq scale - do not use IsLinear() here
   {  // add some extra points to the linear envelope for the graphic to follow
      double step = pow(2., 1./12.);   // twelve steps per octave
      double when,value;
      for(double freq=10.; freq<mHiFreq; freq*=step)
      {
         when = freq/mHiFreq;
         value = mLinEnvelope.GetValue(when);
         mLinEnvelope.Insert(when, value);
      }

      mBands.EnvLinToLog();
      mFreqRuler->ruler.SetLog(true);
      mFreqRuler->ruler.SetRange(mLoFreq, mHiFreq);
   }

   mBands.ErrMin();                  //move sliders to minimise error

   szrV->Show(szrG,true);  // eq sliders
   szrH->Show(szrI,mOptions == kEqLegacy );  // interpolation choice
   szrH->Show(szrL,false); // linear freq checkbox

   mUIParent->Layout();
   wxGetTopLevelParent(mUIParent)->Layout();
   mUIParent->Layout();
   wxGetTopLevelParent(mUIParent)->Layout();

   mBands.GraphicEQ(mLogEnvelope);
   mDrawMode = false;
}

void EqualizationUI::OnSize(wxSizeEvent & event)
{
   mUIParent->Layout();
   event.Skip();
}

void EqualizationUI::OnInterp(wxCommandEvent & WXUNUSED(event))
{
   auto &mParameters = mCurvesList.mParameters;
   bool bIsGraphic = !mParameters.mDrawMode;
   if (bIsGraphic)
   {
      mBands.GraphicEQ(mParameters.mLogEnvelope);
      mCurvesList.EnvelopeUpdated();
   }
   mParameters.mInterp = mInterpChoice->GetSelection();
}

void EqualizationUI::OnDrawMode(wxCommandEvent & WXUNUSED(event))
{
   mCurvesList.mParameters.mDrawMode = true;
   UpdateDraw();
}

void EqualizationUI::OnGraphicMode(wxCommandEvent & WXUNUSED(event))
{
   mCurvesList.mParameters.mDrawMode = false;
   UpdateGraphic();
}

void EqualizationUI::OnSliderM(wxCommandEvent & WXUNUSED(event))
{
   auto &mM = mCurvesList.mParameters.mM;

   size_t m = 2 * mMSlider->GetValue() + 1;
   // Must be odd
   wxASSERT( (m & 1) == 1 );

   if (m != mM) {
      mM = m;
      wxString tip;
      tip.Printf(wxT("%d"), (int)mM);
      mMText->SetLabel(tip);
      mMText->SetName(mMText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
      mMSlider->SetToolTip(tip);

      mCurvesList.ForceRecalc();
   }
}

void EqualizationUI::OnSliderDBMIN(wxCommandEvent & WXUNUSED(event))
{
   auto &mdBMin = mCurvesList.mParameters.mdBMin;

   float dB = mdBMinSlider->GetValue();
   if (dB != mdBMin) {
      mdBMin = dB;
      wxString tip;
      tip.Printf(_("%d dB"), (int)mdBMin);
      mdBMinSlider->SetToolTip(tip);
      UpdateRuler();
   }
}

void EqualizationUI::OnSliderDBMAX(wxCommandEvent & WXUNUSED(event))
{
   auto &mdBMax = mCurvesList.mParameters.mdBMax;

   float dB = mdBMaxSlider->GetValue();
   if (dB != mdBMax) {
      mdBMax = dB;
      wxString tip;
      tip.Printf(_("%d dB"), (int)mdBMax);
      mdBMaxSlider->SetToolTip(tip);
      UpdateRuler();
   }
}

//
// New curve was selected
//
void EqualizationUI::OnCurve(wxCommandEvent & WXUNUSED(event))
{
   // Select NEW curve
   wxASSERT( mCurve != NULL );
   setCurve( mCurve->GetCurrentSelection() );
   if( !mCurvesList.mParameters.mDrawMode )
      UpdateGraphic();
}

//
// User wants to modify the list in some way
//
void EqualizationUI::OnManage(wxCommandEvent & WXUNUSED(event))
{
   auto &mCurves = mCurvesList.mCurves;
   EqualizationCurvesDialog d(mUIParent, mName, mOptions,
      mCurves, mCurve->GetSelection());
   if (d.ShowModal()) {
      wxGetTopLevelParent(mUIParent)->Layout();
      setCurve(d.GetItem());
   }

   // Reload the curve names
   UpdateCurves();

   // Allow control to resize
   mUIParent->Layout();
}

void EqualizationUI::OnClear(wxCommandEvent & WXUNUSED(event))
{
   mBands.Flatten();
}

void EqualizationUI::OnInvert(wxCommandEvent & WXUNUSED(event))
{
   mBands.Invert();
}

void EqualizationUI::OnGridOnOff(wxCommandEvent & WXUNUSED(event))
{
   mCurvesList.mParameters.mDrawGrid = mGridOnOff->IsChecked();
   mPanel->Refresh(false);
}

void EqualizationUI::OnLinFreq(wxCommandEvent & WXUNUSED(event))
{
   auto &mParameters = mCurvesList.mParameters;
   auto &mLin = mParameters.mLin;
   const auto &mLoFreq = mParameters.mLoFreq;
   const auto &mHiFreq = mParameters.mHiFreq;

   mLin = mLinFreq->IsChecked();
   if(mParameters.IsLinear())  //going from log to lin freq scale
   {
      mFreqRuler->ruler.SetLog(false);
      mFreqRuler->ruler.SetRange(0, mHiFreq);
      mBands.EnvLogToLin();
      mLin = true;
   }
   else  //going from lin to log freq scale
   {
      mFreqRuler->ruler.SetLog(true);
      mFreqRuler->ruler.SetRange(mLoFreq, mHiFreq);
      mBands.EnvLinToLog();
      mLin = false;
   }
   mFreqRuler->Refresh(false);
   mCurvesList.ForceRecalc();
}

void EqualizationUI::OnIdle(wxIdleEvent &event)
{
   event.Skip();
   if (mCurve)
      mCurve->SetStringSelection(mCurvesList.mParameters.mCurveName);
}
