/**********************************************************************

  Audacity: A Digital Audio Editor


  ToolsToolBar.h

  Dominic Mazzoni
  Shane T. Mueller
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_TOOLS_TOOLBAR__
#define __AUDACITY_TOOLS_TOOLBAR__

#include <wx/defs.h>

#include "Observer.h"
#include "ToolBar.h"

class wxCommandEvent;
class wxDC;
class wxGridSizer;
class wxImage;
class wxWindow;

class AButton;
class AudacityProject;
struct ProjectSettingsEvent;

// Code duplication warning: these apparently need to be in the
// same order as the enum in ToolsToolBar.cpp

const int FirstToolID = 11200;

class ToolsToolBar final : public ToolBar
{
public:
    static Identifier ID();

    ToolsToolBar(AudacityProject& project);
    virtual ~ToolsToolBar();

    static ToolsToolBar& Get(AudacityProject& project);
    static const ToolsToolBar& Get(const AudacityProject& project);

    void UpdatePrefs() override;

    void OnTool(wxCommandEvent& evt);
    void OnToolChanged(ProjectSettingsEvent);
    void DoToolChanged();

    void Populate() override;
    void Repaint(wxDC* WXUNUSED(dc)) override {}
    void EnableDisableButtons() override {}

private:

    void Create(wxWindow* parent) override;
    void RegenerateTooltips() override;
    wxImage* MakeToolImage(wxImage* tool, wxImage* mask, int style);

    enum {
        numTools = 4
    };

    Observer::Subscription mSubscription;
    AButton* mTool[numTools];
    wxGridSizer* mToolSizer;
    int mCurrentTool;

public:

    DECLARE_CLASS(ToolsToolBar)
    DECLARE_EVENT_TABLE()
};

#endif
