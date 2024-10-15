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
#include "EffectEditor.h"
#include "BasicUI.h"
#include <wx/button.h>
#include <wx/choice.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/stattext.h>
#include "ShuttleGui.h"
#include "../widgets/RulerPanel.h"
#include "../widgets/LinearUpdater.h"
#include "../widgets/LogarithmicUpdater.h"
#include "../widgets/IntFormat.h"
#include "../widgets/LinearDBFormat.h"

#if wxUSE_ACCESSIBILITY
#include "WindowAccessible.h"
#endif

BEGIN_EVENT_TABLE(EqualizationUI, wxEvtHandler)
   EVT_SIZE( EqualizationUI::OnSize )
   EVT_IDLE(EqualizationUI::OnIdle)

END_EVENT_TABLE()

namespace
{
class EqualizationUIEditor : public EffectEditor, protected wxEvtHandler
{
public:
   /*!
    @param pParent if not null, caller will push an event handler onto this
    window; then this object is responsible to pop it
    */
   EqualizationUIEditor(
      EqualizationUI& ui, EffectUIServices& services,
      EffectSettingsAccess& access, wxWindow* pParent = nullptr)
       : EffectEditor { services, access }
       , mEqualizationUI { ui }
       , mpParent { pParent }
   {
      if (mpParent)
         mpParent->PushEventHandler(&ui);
   }
   //! Calls Disconnect
   ~EqualizationUIEditor() override
   {
      Disconnect();
   }
   //! Calls mServices.ValidateUI()
   bool ValidateUI() override
   {
      bool result {};
      mAccess.ModifySettings(
         [&](EffectSettings& settings)
         {
            result = mEqualizationUI.ValidateUI(settings);
            return nullptr;
         });
      return result;
   }

   void Disconnect() override
   {
      if (mpParent)
      {
         mpParent->PopEventHandler();
         mpParent = nullptr;
      }
   }

protected:
   EqualizationUI& mEqualizationUI;
   wxWindow* mpParent {};
};
} // namespace

bool EqualizationUI::ValidateUI(EffectSettings &)
{
   auto& parameters = mCurvesList.mParameters;
   const auto &curveName = parameters.mCurveName;
   auto &logEnvelope = parameters.mLogEnvelope;
   const auto &curves = mCurvesList.mCurves;

   EQCurveWriter{ curves }.SaveCurves();

   parameters.SaveConfig();

   return true;
}

std::unique_ptr<EffectEditor> EqualizationUI::PopulateOrExchange(
   ShuttleGui & S, EffectInstance &, EffectSettingsAccess &access,
   const EffectOutputs *)
{
   mBands.Init();

   auto &parameters = mCurvesList.mParameters;
   const auto &M = parameters.mM;
   const auto &loFreq = parameters.mLoFreq;
   const auto &hiFreq = parameters.mHiFreq;
   const auto &curves = mCurvesList.mCurves;

   auto &drawMode = parameters.mDrawMode;

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
               RulerPanel::Range{ loFreq, hiFreq },
               IntFormat::Instance(),
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
               LinearDBFormat::Instance(),
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

         parameters.ChooseEnvelope().Flatten(0.);
         parameters.ChooseEnvelope().SetTrackLen(1.0);
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
                     .AddSlider( {}, (M - 1) / 2, 4095, 10);
                  BindTo(*mMSlider, wxEVT_SLIDER,
                     &EqualizationUI::OnSliderM);
               }
               S.EndHorizontalLay();

               S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_CENTER_VERTICAL, 0);
               {
                  wxString label;
                  label.Printf(wxT("%ld"), M);
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
                        [&curves]{
                           TranslatableStrings names;
                           for (const auto &curve : curves)
                              names.push_back( Verbatim( curve.Name ) );
                           return names;
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
      drawMode = true;
   if( mOptions == kEqOptionGraphic)
      drawMode = false;

   // "show" settings for graphics mode before setting the size of the dialog
   // as this needs more space than draw mode
   szrV->Show(szrG,!drawMode);  // eq sliders
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

   return std::make_unique<EqualizationUIEditor>(*this, mUIServices, access, mUIParent);
}

bool EqualizationUI::TransferDataToWindow(const EffectSettings &settings)
{
   auto &parameters = mCurvesList.mParameters;
   const auto &lin = parameters.mLin;
   const auto &drawGrid = parameters.mDrawGrid;
   const auto &M = parameters.mM;
   const auto &dBMin = parameters.mdBMin;
   const auto &dBMax = parameters.mdBMax;
   const auto &interp = parameters.mInterp;

   auto &drawMode = parameters.mDrawMode;

   // Set log or lin freq scale (affects interpolation as well)
   mLinFreq->SetValue( lin );
   wxCommandEvent dummyEvent;
   OnLinFreq(dummyEvent);  // causes a CalcFilter

   mGridOnOff->SetValue( drawGrid ); // checks/unchecks the box on the interface

   if( mMSlider )
      mMSlider->SetValue((M - 1) / 2);

   mdBMinSlider->SetValue((int)dBMin);
   mdBMaxSlider->SetValue((int)dBMax);

   // Reload the curve names
   UpdateCurves();

   // Set graphic interpolation mode
   mInterpChoice->SetSelection(interp);

   // Override draw mode, if we're not displaying the radio buttons.
   if( mOptions == kEqOptionCurve)
      drawMode = true;
   if( mOptions == kEqOptionGraphic)
      drawMode = false;

   if( mDraw )
      mDraw->SetValue(drawMode);
   szrV->Show(szr1,mOptions != kEqOptionGraphic); // Graph
   szrV->Show(szrG,!drawMode);    // eq sliders
   szrH->Show(szrI,mOptions == kEqLegacy );    // interpolation choice
   szrH->Show(szrL, drawMode);    // linear freq checkbox
   if( mGraphic)
      mGraphic->SetValue(!drawMode);
   mGridOnOff->Show( drawMode );

   // Set Graphic (Fader) or Draw mode
   if (!drawMode)
      UpdateGraphic();

   UpdateRuler();

   mUIParent->Layout();
   wxGetTopLevelParent(mUIParent)->Layout();

   return true;
}

void EqualizationUI::UpdateRuler()
{
   const auto &parameters = mCurvesList.mParameters;
   const auto &dBMin = parameters.mdBMin;
   const auto &dBMax = parameters.mdBMax;

   // Refresh ruler when values have changed
   int w1, w2, h;
   mdBRuler->ruler.GetMaxSize(&w1, &h);
   mdBRuler->ruler.SetRange(dBMax, dBMin);
   mdBRuler->ruler.GetMaxSize(&w2, &h);
   if( w1 != w2 )   // Reduces flicker
   {
      mdBRuler->SetSize(wxSize(w2,h));
      mFreqRuler->Refresh(false);
   }
   mdBRuler->Refresh(false);

   mPanel->Refresh(false);
}

///////////////////////////////////////////////////////////////////////////////
//
// All EffectEqualization methods beyond this point interact with the UI, so
// can't be called while the UI is not displayed.
//
///////////////////////////////////////////////////////////////////////////////

void EqualizationUI::UpdateCurves()
{
   auto &parameters = mCurvesList.mParameters;
   auto &curveName = parameters.mCurveName;
   const auto &curves = mCurvesList.mCurves;

   // Reload the curve names
   if( mCurve )
      mCurve->Clear();
   bool selectedCurveExists = false;
   for (size_t i = 0, cnt = curves.size(); i < cnt; i++)
   {
      if (curveName == curves[ i ].Name)
         selectedCurveExists = true;
      if( mCurve )
         mCurve->Append(curves[ i ].Name);
   }
   // In rare circumstances, curveName may not exist (bug 1891)
   if (!selectedCurveExists)
      curveName = curves[ (int)curves.size() - 1 ].Name;
   if( mCurve )
      mCurve->SetStringSelection(curveName);

   // Allow the control to resize
   if( mCurve )
      mCurve->SetMinSize({-1, -1});

   // Set initial curve
   mCurvesList.setCurve( curveName );
}

void EqualizationUI::UpdateDraw()
{
   auto &parameters = mCurvesList.mParameters;
   const auto &lin = parameters.mLin;
   auto &linEnvelope = parameters.mLinEnvelope;
   auto &logEnvelope = parameters.mLogEnvelope;
   const auto &hiFreq = parameters.mHiFreq;

   size_t numPoints = logEnvelope.GetNumberOfPoints();
   Doubles when{ numPoints };
   Doubles value{ numPoints };
   double deltadB = 0.1;
   double dx, dy, dx1, dy1, err;

   logEnvelope.GetPoints( when.get(), value.get(), numPoints );

   // set 'unnamed' as the selected curve
   mCurvesList.EnvelopeUpdated();

   bool flag = true;
   while (flag)
   {
      flag = false;
      int numDeleted = 0;
      logEnvelope.GetPoints( when.get(), value.get(), numPoints );
      for (size_t j = 0; j + 2 < numPoints; j++)
      {
         dx = when[j+2+numDeleted] - when[j+numDeleted];
         dy = value[j+2+numDeleted] - value[j+numDeleted];
         dx1 = when[j+numDeleted+1] - when[j+numDeleted];
         dy1 = dy * dx1 / dx;
         err = fabs(value[j+numDeleted+1] - (value[j+numDeleted] + dy1));
         if( err < deltadB )
         {   // within < deltadB dB?
            logEnvelope.Delete(j+1);
            numPoints--;
            numDeleted++;
            flag = true;
         }
      }
   }

   if(lin) // do not use IsLinear() here
   {
      mBands.EnvLogToLin();
      mFreqRuler->ruler.SetUpdater(&LinearUpdater::Instance());
      mFreqRuler->ruler.SetRange(0, hiFreq);
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
   auto &parameters = mCurvesList.mParameters;
   const auto &lin = parameters.mLin;
   auto &linEnvelope = parameters.mLinEnvelope;
   auto &logEnvelope = parameters.mLogEnvelope;
   const auto &loFreq = parameters.mLoFreq;
   const auto &hiFreq = parameters.mHiFreq;

   auto &drawMode = parameters.mDrawMode;

   if(lin)  //going from lin to log freq scale - do not use IsLinear() here
   {  // add some extra points to the linear envelope for the graphic to follow
      double step = pow(2., 1./12.);   // twelve steps per octave
      double when,value;
      for(double freq=10.; freq<hiFreq; freq*=step)
      {
         when = freq/hiFreq;
         value = linEnvelope.GetValue(when);
         linEnvelope.Insert(when, value);
      }

      mBands.EnvLinToLog();
      mFreqRuler->ruler.SetUpdater(&LogarithmicUpdater::Instance());
      mFreqRuler->ruler.SetRange(loFreq, hiFreq);
   }

   mBands.ErrMin();                  //move sliders to minimise error

   szrV->Show(szrG,true);  // eq sliders
   szrH->Show(szrI,mOptions == kEqLegacy );  // interpolation choice
   szrH->Show(szrL,false); // linear freq checkbox

   mUIParent->Layout();
   wxGetTopLevelParent(mUIParent)->Layout();
   mUIParent->Layout();
   wxGetTopLevelParent(mUIParent)->Layout();

   mBands.GraphicEQ(logEnvelope);
   drawMode = false;
}

void EqualizationUI::OnSize(wxSizeEvent & event)
{
   mUIParent->Layout();
   event.Skip();
}

void EqualizationUI::OnInterp(wxCommandEvent & WXUNUSED(event))
{
   auto &parameters = mCurvesList.mParameters;
   bool bIsGraphic = !parameters.mDrawMode;
   if (bIsGraphic)
   {
      mBands.GraphicEQ(parameters.mLogEnvelope);
      mCurvesList.EnvelopeUpdated();
   }
   parameters.mInterp = mInterpChoice->GetSelection();
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
   auto &M = mCurvesList.mParameters.mM;

   size_t m = 2 * mMSlider->GetValue() + 1;
   // Must be odd
   wxASSERT( (m & 1) == 1 );

   if (m != M) {
      M = m;
      wxString tip;
      tip.Printf(wxT("%d"), (int)M);
      mMText->SetLabel(tip);
      mMText->SetName(mMText->GetLabel()); // fix for bug 577 (NVDA/Narrator screen readers do not read static text in dialogs)
      mMSlider->SetToolTip(tip);

      mCurvesList.ForceRecalc();
   }
}

void EqualizationUI::OnSliderDBMIN(wxCommandEvent & WXUNUSED(event))
{
   auto &dBMin = mCurvesList.mParameters.mdBMin;

   float dB = mdBMinSlider->GetValue();
   if (dB != dBMin) {
      dBMin = dB;
      wxString tip;
      tip.Printf(_("%d dB"), (int)dBMin);
      mdBMinSlider->SetToolTip(tip);
      UpdateRuler();
   }
}

void EqualizationUI::OnSliderDBMAX(wxCommandEvent & WXUNUSED(event))
{
   auto &dBMax = mCurvesList.mParameters.mdBMax;

   float dB = mdBMaxSlider->GetValue();
   if (dB != dBMax) {
      dBMax = dB;
      wxString tip;
      tip.Printf(_("%d dB"), (int)dBMax);
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
   mCurvesList.setCurve( mCurve->GetCurrentSelection() );
   if( !mCurvesList.mParameters.mDrawMode )
      UpdateGraphic();
}

//
// User wants to modify the list in some way
//
void EqualizationUI::OnManage(wxCommandEvent & WXUNUSED(event))
{
   auto &curves = mCurvesList.mCurves;
   EqualizationCurvesDialog d(mUIParent, mName, mOptions,
      curves, mCurve->GetSelection());
   if (d.ShowModal()) {
      wxGetTopLevelParent(mUIParent)->Layout();
      mCurvesList.setCurve(d.GetItem());
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
   auto &parameters = mCurvesList.mParameters;
   auto &lin = parameters.mLin;
   const auto &loFreq = parameters.mLoFreq;
   const auto &hiFreq = parameters.mHiFreq;

   lin = mLinFreq->IsChecked();
   if(parameters.IsLinear())  //going from log to lin freq scale
   {
      mFreqRuler->ruler.SetUpdater(&LinearUpdater::Instance());
      mFreqRuler->ruler.SetRange(0, hiFreq);
      mBands.EnvLogToLin();
      lin = true;
   }
   else  //going from lin to log freq scale
   {
      mFreqRuler->ruler.SetUpdater(&LogarithmicUpdater::Instance());
      mFreqRuler->ruler.SetRange(loFreq, hiFreq);
      mBands.EnvLinToLog();
      lin = false;
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
