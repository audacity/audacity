/**********************************************************************

  Audacity: A Digital Audio Editor


  CutCopyPaste.h

  ksoze95

**********************************************************************/

#ifndef __AUDACITY_CUT_COPY_PASTE_TOOLBAR__
#define __AUDACITY_CUT_COPY_PASTE_TOOLBAR__

#include <wx/defs.h>

#include "ToolBar.h"

class wxCommandEvent;
class wxDC;
class wxGridSizer;
class wxImage;
class wxWindow;

class AButton;

enum {
   TBCutID,
   TBCopyID,
   TBPasteID,
   TBNumButtons
};

const int first_TB_ID = 21300;

// flags so 1,2,4,8 etc.
enum {
   TBActTooltips = 1,
   TBActEnableDisable = 2,
};

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

   static AButton *AddButton(
      CutCopyPasteToolBar *pBar,
      teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
      int id, const TranslatableString &label, bool toggle = false);

   void RegenerateTooltips() override;
   void ForAllButtons(int Action);

   AButton *mButtons[TBNumButtons];

   wxGridSizer* mToolSizer;

 public:

   DECLARE_CLASS(CutCopyPaste)
   DECLARE_EVENT_TABLE()
};

#endif

