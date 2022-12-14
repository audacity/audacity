/**********************************************************************

  Audacity: A Digital Audio Editor

  ToolBarButtons.h

  ksoze95

**********************************************************************/

#ifndef __AUDACITY_TOOLBAR_BUTTONS__
#define __AUDACITY_TOOLBAR_BUTTONS__

#include "ToolBar.h"

class AButton;


// flags so 1,2,4,8 etc.
enum {
   TBActTooltips = 1,
   TBActEnableDisable = 2,
};

class ToolBarButtons {
public:
   static AButton *AddButton(
      ToolBar *pBar,
      teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
      int firstToolBarId, int thisButtonId,
      const TranslatableString &label, bool toggle = false);
};

#endif
