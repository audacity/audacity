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
   virtual void Repaint(wxDC *dc) {};
   virtual void EnableDisableButtons() {};

   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);

   void OnSlider(wxCommandEvent & event);

   void ShowOutputGainDialog();
   void ShowInputGainDialog();
   void ShowInputSourceDialog();

   void AdjustOutputGain(int adj);
   void AdjustInputGain(int adj);

 private:

   void InitializeMixerToolBar();
   void SetToolTips();

   wxBitmap *mPlayBitmap;
   wxBitmap *mRecordBitmap;

   ASlider *mInputSlider;
   ASlider *mOutputSlider;

   wxChoice *mInputSourceChoice;

 public:

   DECLARE_CLASS(MixerToolBar);
   DECLARE_EVENT_TABLE();
};

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 3acba542-52ae-44eb-b0b3-e0645587b5c0

