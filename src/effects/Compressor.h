/**********************************************************************

  Audacity: A Digital Audio Editor

  Compressor.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EFFECT_COMPRESSOR__
#define __AUDACITY_EFFECT_COMPRESSOR__

class wxString;

#include <wx/defs.h>
#include <wx/bitmap.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/slider.h>
#include <wx/panel.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/intl.h>
#include "TwoPassSimpleMono.h"

class WaveTrack;

class EffectCompressor: public EffectTwoPassSimpleMono {

public:

   EffectCompressor();
   virtual ~EffectCompressor();

   virtual wxString GetEffectName() {
      return wxString(_("Compressor..."));
   }

   virtual std::set<wxString> GetEffectCategories() {
     std::set<wxString> result;
     result.insert(wxT("http://lv2plug.in/ns/lv2core#CompressorPlugin"));
     return result;
   }

   virtual wxString GetEffectIdentifier() {
      return wxString(wxT("Compressor"));
   }

   virtual wxString GetEffectAction() {
      return wxString(_("Applying Dynamic Range Compression..."));
   }

   virtual bool Init();
   virtual bool PromptUser();
   virtual bool TransferParameters( Shuttle & shuttle );

 protected:
   virtual bool TwoBufferProcessPass1(float *buffer1, sampleCount len1, float *buffer2, sampleCount len2);
   virtual bool ProcessPass2(float *buffer, sampleCount len);

 private:

   virtual bool NewTrackPass1();
   virtual bool InitPass1();
   virtual bool InitPass2();

   void FreshenCircle();
   float AvgCircle(float x);
   double    mRMSSum;
   int       mCircleSize;
   int       mCirclePos;
   double   *mCircle;

   void Follow(float *buffer, float *env, int len, float *previous, int previous_len);
   float DoCompression(float x, double env);

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
   float	*mFollow1;
   float	*mFollow2;
   sampleCount mFollowLen;

   double    mMax;			//MJS

   friend class CompressorDialog;
};

class CompressorPanel: public wxPanel
{
public:
   CompressorPanel( wxWindow *parent, wxWindowID id,
                    const wxPoint& pos = wxDefaultPosition,
                    const wxSize& size = wxDefaultSize);

   void OnPaint(wxPaintEvent & event);

   double threshold;
   double noisefloor;
   double ratio;

private:

   wxBitmap *mBitmap;
   wxRect mEnvRect;
   int mWidth;
   int mHeight;

   DECLARE_EVENT_TABLE()
};

// WDR: class declarations

//----------------------------------------------------------------------------
// CompressorDialog
//----------------------------------------------------------------------------

class CompressorDialog: public EffectDialog
{
public:
   // constructors and destructors
   CompressorDialog( EffectCompressor *effect, wxWindow *parent);

   void PopulateOrExchange(ShuttleGui & S);
   bool TransferDataToWindow();
   bool TransferDataFromWindow();

   double threshold;
   double noisefloor;
   double ratio;
   double attack;
   double decay;  // "release"
   bool useGain;
   bool usePeak;

private:
   void OnSize( wxSizeEvent &event );
   void OnSlider( wxCommandEvent &event );
   void OnPreview( wxCommandEvent &event );

   EffectCompressor *mEffect;
   CompressorPanel *mPanel;

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

private:
   DECLARE_EVENT_TABLE()
};

#endif

