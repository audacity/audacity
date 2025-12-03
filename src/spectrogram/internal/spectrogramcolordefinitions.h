/**********************************************************************

  Audacity: A Digital Audio Editor

  AllThemeResources.h

  James Crook

  Audacity is free software.
  License: GPL v2 or later - see LICENSE.txt

********************************************************************//**

\file AllThemeResources.h

This file contains definitions of all images, cursors, colours, fonts
and grids used by Audacity.

This will be split up into separate include files to reduce the amount
of recompilation on a change.

Meantime, do NOT DELETE any of these declarations, even if they're
unused, as they're all offset by prior declarations.

To add an image, you give its size and name like so:

\code
\endcode

If you do this and run the program the image will be black to start
with, but you can go into ThemePrefs and load it (load components)
from there.  Audacity will look for a file called "Pause.png".

 - Now save into ImageCache.
 - From here on you can get the image by loading ImageCache.
 - To burn it into the program defaults, use the
 'Output Sourcery' button.

\see \ref Themability in DOxygen documentation for more details.

*//*******************************************************************/

// Note: No '#ifndef/#define' pair on this header file.
// we want to include it multiple times in Theme.cpp.

#include "macromagic.h"

SET_THEME_FLAGS(resFlagNewLine);

DEFINE_COLOUR(clrUnselected, QColor(30,  30,  30), wxT("Unselected"));
DEFINE_COLOUR(clrSelected,   QColor(93,  65,  93), wxT("Selected"));

DEFINE_COLOUR(clrSpectro1,            QColor(191,  191,  191),  wxT("Spectro1"));
DEFINE_COLOUR(clrSpectro1Sel,         QColor(143,  143,  143),  wxT("Spectro1Sel"));
