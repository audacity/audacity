/**********************************************************************

  Audacity: A Digital Audio Editor

  MeterToolbar.h

  Dominic Mazzoni
  Leland Lucius

  ToolBar to hold the VU Meter

**********************************************************************/

#ifndef __AUDACITY_METER_TOOLBAR__
#define __AUDACITY_METER_TOOLBAR__

#include <functional>
#include <vector>
#include "ToolBar.h"

class wxDC;
class wxGridBagSizer;
class wxSizeEvent;
class wxWindow;

class AudacityProject;
class MeterPanel;
class MeterToolBar;

using MeterToolBars = std::vector< std::reference_wrapper<MeterToolBar> >;
using ConstMeterToolBars = std::vector< std::reference_wrapper<const MeterToolBar> >;

// Constants used as bit pattern
const int kWithRecordMeter = 1;
const int kWithPlayMeter = 2;

class MeterToolBar final : public ToolBar {

 public:

   MeterToolBar(AudacityProject &project, int type);
   virtual ~MeterToolBar();

   static MeterToolBars GetToolBars(AudacityProject &project);
   static ConstMeterToolBars GetToolBars(const AudacityProject &project);

   static MeterToolBar & Get(AudacityProject &project, bool forPlayMeterToolBar);
   static const MeterToolBar & Get(const AudacityProject &project, bool forPlayMeterToolBar);

   void Create(wxWindow *parent) override;

   void Populate() override;
   void ReCreateButtons() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override {};
   void UpdatePrefs() override;
   void UpdateControls();

   void OnSize(wxSizeEvent & event);
   bool Expose(bool show) override;

   int GetInitialWidth() override;
   int GetMinToolbarWidth()  override { return 150; }
   wxSize GetDockedSize() override {
      return GetSmartDockedSize();
   };
   virtual void SetDocked(ToolDock *dock, bool pushed)override;

   void ShowOutputGainDialog();
   void ShowInputGainDialog();

   void AdjustOutputGain(int adj);
   void AdjustInputGain(int adj);

 private:
   void RegenerateTooltips() override;

   int mWhichMeters;
   wxGridBagSizer *mSizer;
   MeterPanel *mPlayMeter;
   MeterPanel *mRecordMeter;

 public:

   DECLARE_CLASS(MeterToolBar)
   DECLARE_EVENT_TABLE()

};

#endif

