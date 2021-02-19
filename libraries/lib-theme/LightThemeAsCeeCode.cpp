/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file LightThemeAsCeeCode.cpp
 
 Paul Licameli split from Theme.cpp
 
 **********************************************************************/

#include <vector>
#include "Theme.h"

static const std::vector<unsigned char> ImageCacheAsData {
// Include the generated file full of numbers
#include "LightThemeAsCeeCode.h"
};

static ThemeBase::RegisteredTheme theme{
   /* i18n-hint: Light meaning opposite of dark */
   { "light", XO("Light") }, ImageCacheAsData
};
