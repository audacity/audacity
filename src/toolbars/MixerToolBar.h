/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerToolbar.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_MIXER_TOOLBAR__
#define __AUDACITY_MIXER_TOOLBAR__

#include "../MemoryX.h"
#include "ToolBar.h"

class wxImage;
class wxSize;
class wxPoint;
class wxChoice;
class wxStaticBitmap;

class ASlider;

class MixerToolBar final : public ToolBar {

 public:

   MixerToolBar();
   virtual ~MixerToolBar();

   void Create(wxWindow * parent);

   void UpdatePrefs();
   void UpdateControls();
   void SetMixer(wxCommandEvent &event);

   void Populate() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override {};

   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);

   void OnSlider(wxCommandEvent & event);

   void ShowOutputGainDialog();
   void ShowInputGainDialog();

   void AdjustOutputGain(int adj);
   void AdjustInputGain(int adj);

   void RegenerateTooltips() override {};

 protected:
   float mInputSliderVolume;
   float mOutputSliderVolume;

 private:

   void InitializeMixerToolBar();
   void SetToolTips();

   std::unique_ptr<wxBitmap> mPlayBitmap, mRecordBitmap;

   ASlider *mInputSlider;
   ASlider *mOutputSlider;

 public:

   DECLARE_CLASS(MixerToolBar)
   DECLARE_EVENT_TABLE()
};

#endif

