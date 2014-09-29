/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerToolbar.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_MIXER_TOOLBAR__
#define __AUDACITY_MIXER_TOOLBAR__

#include "ToolBar.h"

class wxImage;
class wxSize;
class wxPoint;
class wxChoice;
class wxStaticBitmap;

class ASlider;

class MixerToolBar:public ToolBar {

 public:

   MixerToolBar();
   virtual ~MixerToolBar();

   void Create(wxWindow * parent);

   void RecreateTipWindows();
   void UpdatePrefs();
   void UpdateControls();
   void SetMixer(wxCommandEvent &event);

   virtual void Populate();
   virtual void Repaint(wxDC * WXUNUSED(dc)) {};
   virtual void EnableDisableButtons() {};

   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);

   void OnSlider(wxCommandEvent & event);

   void ShowOutputGainDialog();
   void ShowInputGainDialog();

   void AdjustOutputGain(int adj);
   void AdjustInputGain(int adj);

 protected:
   float mInputSliderVolume;
   float mOutputSliderVolume;

 private:

   void InitializeMixerToolBar();
   void SetToolTips();

   wxBitmap *mPlayBitmap;
   wxBitmap *mRecordBitmap;

   ASlider *mInputSlider;
   ASlider *mOutputSlider;

 public:

   DECLARE_CLASS(MixerToolBar);
   DECLARE_EVENT_TABLE();
};

#endif

