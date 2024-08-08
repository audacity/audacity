#ifndef __AUDACITY_SEARCH_BAR__
#define __AUDACITY_SEARCH_BAR__

#include <wx/defs.h>
#include <wx/sizer.h>
#include <wx/srchctrl.h>

#include "ToolBar.h"

class wxCommandEvent;
class wxDC;
class wxGridSizer;
class wxImage;
class wxWindow;

class TextFilterOverride;

class SearchToolBar final : public ToolBar {

public:
   SearchToolBar(AudacityProject& project);

public:
   static Identifier ID();

   void Create(wxWindow* parent) override;

   void Populate() override;
   void UpdatePrefs() override;

   void Repaint(wxDC* WXUNUSED(dc)) override {}
   void EnableDisableButtons() override {}
   void RegenerateTooltips() override {}

public:
   virtual ~SearchToolBar();


private:
   void OnSearch(wxCommandEvent& evt);
   void OnFocus(wxFocusEvent& evt);

private:
   wxSearchCtrl* mInputField;
   wxBoxSizer* mToolSizer;
   bool mFocused = false;

public:
   DECLARE_CLASS(SearchToolBar)
   DECLARE_EVENT_TABLE()
};

#endif // __AUDACITY_SEARCH_BAR__
