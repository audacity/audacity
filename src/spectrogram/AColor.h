/**********************************************************************

  Audacity: A Digital Audio Editor

  AColor.h

  Dominic Mazzoni

  Manages color brushes and pens and provides utility
  drawing functions

**********************************************************************/

#ifndef __AUDACITY_COLOR__
#define __AUDACITY_COLOR__

class THEME_API AColor
{
public:

    enum ColorGradientChoice {
        ColorGradientUnselected = 0,
        ColorGradientTimeSelected,
        ColorGradientTimeAndFrequencySelected,
        ColorGradientEdge,

        ColorGradientTotal // keep me last
    };

    static void PreComputeGradient();

    static bool gradient_inited;
    static const int colorSchemes = 4;
    static const int gradientSteps = 256;
    static unsigned char gradient_pre[ColorGradientTotal][colorSchemes][gradientSteps][3];
};

inline void GetColorGradient(float value,
                             AColor::ColorGradientChoice selected,
                             int colorScheme,
                             unsigned char* __restrict red,
                             unsigned char* __restrict green,
                             unsigned char* __restrict blue)
{
    int idx = value * (AColor::gradientSteps - 1);

    *red = AColor::gradient_pre[selected][colorScheme][idx][0];
    *green = AColor::gradient_pre[selected][colorScheme][idx][1];
    *blue = AColor::gradient_pre[selected][colorScheme][idx][2];
}

#endif
