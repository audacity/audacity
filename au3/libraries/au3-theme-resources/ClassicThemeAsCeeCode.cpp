/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file ClassicThemeAsCeeCode.cpp

 Paul Licameli split from Theme.cpp

 **********************************************************************/

#include <vector>
#include "Theme.h"

static const std::vector<unsigned char> ImageCacheAsData {
// Include the generated file full of numbers
#include "ClassicThemeAsCeeCode.h"
};

static ThemeBase::RegisteredTheme theme{
    /* i18n-hint: describing the "classic" or traditional
       appearance of older versions of Audacity */
    { "classic", XO("Classic") }, PreferredSystemAppearance::Light, ImageCacheAsData
};
