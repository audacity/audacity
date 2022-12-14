/**********************************************************************

  Audacity: A Digital Audio Editor


  CutCopyPaste.h

  ksoze95

**********************************************************************/

#ifndef __AUDACITY_CUT_COPY_PASTE_TOOLBAR__
#define __AUDACITY_CUT_COPY_PASTE_TOOLBAR__

#include <vector>
#include <wx/defs.h>

#include "ToolBar.h"
#include "ToolBarButtons.h"

class wxCommandEvent;
class wxDC;
class wxGridSizer;
class wxImage;
class wxWindow;

class CutCopyPasteToolBar final : public ToolBar {

 public:

   CutCopyPasteToolBar( AudacityProject &project );
   virtual ~CutCopyPasteToolBar();

   void Create(wxWindow *parent) override;

   void OnButton(wxCommandEvent & event);

   void Populate() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override;
   void UpdatePrefs() override;

 private:

   void AddButton(
      teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
      int firstToolBarId, int thisButtonId,
      const TranslatableString &label, bool toggle = false);

   void RegenerateTooltips() override;
   void ForAllButtons(int Action);

   std::vector<AButton*> mButtons;

   wxGridSizer* mToolSizer;

 public:

   DECLARE_CLASS(CutCopyPaste)
   DECLARE_EVENT_TABLE()
};

#endif

