/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file HiContrastThemeAsCeeCode.cpp
 
 Paul Licameli split from Theme.cpp
 
 **********************************************************************/

#include <vector>
#include "Theme.h"

static const std::vector<unsigned char> ImageCacheAsData {
// Include the generated file full of numbers
#include "HiContrastThemeAsCeeCode.h"
};

static ThemeBase::RegisteredTheme theme{
   /* i18n-hint: greater difference between foreground and
      background colors */
   { "high-contrast", XO("High Contrast") }, ImageCacheAsData
};
