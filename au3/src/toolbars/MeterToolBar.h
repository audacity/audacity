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
constexpr int kWithRecordMeter = 1;
constexpr int kWithPlayMeter = 2;
constexpr int kCombinedMeter = kWithPlayMeter | kWithRecordMeter;

class MeterToolBar final : public ToolBar
{
public:

    static Identifier ID();
    static Identifier PlayID();
    static Identifier RecordID();

    MeterToolBar(AudacityProject& project, unsigned whichMeters, const TranslatableString& label, Identifier ID);
    virtual ~MeterToolBar();

    static MeterToolBars GetToolBars(AudacityProject& project);
    static ConstMeterToolBars GetToolBars(const AudacityProject& project);

    static MeterToolBar& Get(AudacityProject& project, bool forPlayMeterToolBar);
    static const MeterToolBar& Get(const AudacityProject& project, bool forPlayMeterToolBar);

    bool ShownByDefault() const override;

    void Create(wxWindow* parent) override;

    void Populate() override;
    void ReCreateButtons() override;
    void Repaint(wxDC* WXUNUSED(dc)) override {}
    void EnableDisableButtons() override {}
    void UpdatePrefs() override;
    void UpdateControls();

    void OnSize(wxSizeEvent& event);
    bool Expose(bool show) override;

    int GetInitialWidth() override;
    int GetMinToolbarWidth()  override { return 150; }
    wxSize GetDockedSize() override
    {
        return GetSmartDockedSize();
    }

    void ShowOutputGainDialog();
    void ShowInputGainDialog();

    void AdjustOutputGain(int adj);
    void AdjustInputGain(int adj);

private:
    void RegenerateTooltips() override {}
    void RebuildLayout(bool force);

    unsigned mWhichMeters;
    wxBoxSizer* mRootSizer{ nullptr };
    AButton* mPlaySetupButton{ nullptr };
    MeterPanel* mPlayMeter{ nullptr };
    AButton* mRecordSetupButton{ nullptr };
    MeterPanel* mRecordMeter{ nullptr };

public:

    DECLARE_CLASS(MeterToolBar)
    DECLARE_EVENT_TABLE()
};

#endif
