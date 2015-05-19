/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_COMPRESSOR__
#define __AUDACITY_EFFECT_COMPRESSOR__

#include <wx/bitmap.h>
#include <wx/checkbox.h>
#include <wx/event.h>
#include <wx/gdicmn.h>
#include <wx/panel.h>
#include <wx/slider.h>
#include <wx/string.h>
#include <wx/stattext.h>
#include <wx/window.h>

#include "../ShuttleGui.h"

#include "TwoPassSimpleMono.h"

class EffectCompressorPanel;

#define COMPRESSOR_PLUGIN_SYMBOL XO("Compressor")

class EffectCompressor : public EffectTwoPassSimpleMono
{
public:

   EffectCompressor();
   virtual ~EffectCompressor();

   // IdentInterface implementation

   virtual wxString GetSymbol();
   virtual wxString GetDescription();

   // EffectIdentInterface implementation

   virtual EffectType GetType();

   // EffectClientInterface implementation

   virtual bool GetAutomationParameters(EffectAutomationParameters & parms);
   virtual bool SetAutomationParameters(EffectAutomationParameters & parms);

   // Effect implementation

   virtual bool Startup();
   virtual void PopulateOrExchange(ShuttleGui & S);
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

protected:
   // EffectTwoPassSimpleMono implementation

   virtual bool InitPass1();
   virtual bool InitPass2();
   virtual bool NewTrackPass1();
   virtual bool ProcessPass2(float *buffer, sampleCount len);
   virtual bool TwoBufferProcessPass1(float *buffer1, sampleCount len1, float *buffer2, sampleCount len2);

private:
   // EffectCompressor implementation

   void FreshenCircle();
   float AvgCircle(float x);
   void Follow(float *buffer, float *env, int len, float *previous, int previous_len);
   float DoCompression(float x, double env);

   void OnSlider(wxCommandEvent & evt);
   void UpdateUI();

private:
   double    mRMSSum;
   int       mCircleSize;
   int       mCirclePos;
   double   *mCircle;

   double    mAttackTime;
   double    mThresholdDB;
   double    mNoiseFloorDB;
   double    mRatio;
   bool      mNormalize;	//MJS
   bool      mUsePeak;

   double    mDecayTime;   // The "Release" time.
   double    mAttackFactor;
   double    mAttackInverseFactor;
   double    mDecayFactor;
   double    mThreshold;
   double    mCompression;
   double    mNoiseFloor;
   int       mNoiseCounter;
   double    mGain;
   double    mLastLevel;
   float	   *mFollow1;
   float	   *mFollow2;
   sampleCount mFollowLen;

   double    mMax;			//MJS

   EffectCompressorPanel *mPanel;

   wxStaticText *mThresholdLabel;
   wxSlider *mThresholdSlider;
   wxStaticText *mThresholdText;

   wxStaticText *mNoiseFloorLabel;
   wxSlider *mNoiseFloorSlider;
   wxStaticText *mNoiseFloorText;

   wxStaticText *mRatioLabel;
   wxSlider *mRatioSlider;
   wxStaticText *mRatioText;

   wxStaticText *mAttackLabel;
   wxSlider *mAttackSlider;
   wxStaticText *mAttackText;

   wxStaticText *mDecayLabel;
   wxSlider *mDecaySlider;
   wxStaticText *mDecayText;

   wxCheckBox *mGainCheckBox;
   wxCheckBox *mPeakCheckBox;

   DECLARE_EVENT_TABLE();
};

class EffectCompressorPanel: public wxPanel
{
public:
   EffectCompressorPanel(wxWindow *parent,
                         double & threshold,
                         double & noiseFloor,
                         double & ratio);

private:
   void OnPaint(wxPaintEvent & evt);
   void OnSize(wxSizeEvent & evt);

private:
   double & threshold;
   double & noiseFloor;
   double & ratio;

   DECLARE_EVENT_TABLE();
};

#endif

