#include <wx/app.h>

#include "ToolBarButtons.h"

#include "AllThemeResources.h"
#include "../widgets/AButton.h"

/// This is a convenience function that allows for button creation in
/// MakeButtons() with fewer arguments
/// Very similar to code in ControlToolBar...
AButton *ToolBarButtons::AddButton(
   ToolBar *pBar,
   teBmps eEnabledUp, teBmps eEnabledDown, teBmps eDisabled,
   int firstToolBarId,
   int thisButtonId,
   const TranslatableString &label,
   bool toggle)
{
   auto r = ToolBar::MakeButton(pBar,
      bmpRecoloredUpSmall, bmpRecoloredDownSmall, bmpRecoloredUpHiliteSmall, bmpRecoloredHiliteSmall,
      eEnabledUp, eEnabledDown, eDisabled,
      wxWindowID(thisButtonId + firstToolBarId),
      wxDefaultPosition,
      toggle,
      theTheme.ImageSize( bmpRecoloredUpSmall ));

   r->SetLabel(label);

   return r;
}
