/**********************************************************************

  Audacity: A Digital Audio Editor

  MixerToolbar.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_MIXER_TOOLBAR__
#define __AUDACITY_MIXER_TOOLBAR__

#include "ToolBar.h"

class wxSize;
class wxPoint;

class ASlider;
class AudacityProject;

class MixerToolBar final : public ToolBar {

 public:

   MixerToolBar( AudacityProject &project );
   virtual ~MixerToolBar();

   static MixerToolBar &Get( AudacityProject &project );
   static const MixerToolBar &Get( const AudacityProject &project );

   void Create(wxWindow * parent) override;

   void UpdatePrefs() override;
   void UpdateControls();
   void SetMixer(wxCommandEvent &event);

   void Populate() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override {};
   int GetMinToolbarWidth() override { return 250; }
   int GetInitialWidth()  override { return 327; }

   void OnFocus(wxFocusEvent &event);
   void OnCaptureKey(wxCommandEvent &event);

   void OnSlider(wxCommandEvent & event);

   void ShowOutputGainDialog();
   void ShowInputGainDialog();

   void AdjustOutputGain(int adj);
   void AdjustInputGain(int adj);

   float GetInputGain();
   void SetInputGain(float vol);

   void RegenerateTooltips() override {};

   float beforeMutedInputVolume;

 protected:
   float mInputSliderVolume;
   float mOutputSliderVolume;

 private:

   void InitializeMixerToolBar();
   void SetToolTips();

   ASlider *mInputSlider;
   ASlider *mOutputSlider;

 public:

   DECLARE_CLASS(MixerToolBar)
   DECLARE_EVENT_TABLE()
};

#endif

