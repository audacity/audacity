/**********************************************************************

  Audacity: A Digital Audio Editor

  AColor.cpp

  Dominic Mazzoni


********************************************************************//**

\class AColor
\brief AColor Manages color brushes and pens

It is also a place to document colour usage policy in Audacity

*//********************************************************************/

#include "AColor.h"
#include "AColorResources.h"

#include <wx/colour.h>

#include "AllThemeResources.h"
#include "Theme.h"

bool AColor::inited = false;
unsigned char AColor::gradient_pre[ColorGradientTotal][colorSchemes][gradientSteps][3];

bool AColor::gradient_inited = 0;

void AColor::PreComputeGradient()
{
    if (gradient_inited) {
        return;
    }
    gradient_inited = 1;
    theTheme.EnsureInitialised();

    // Keep in correspondence with enum SpectrogramSettings::ColorScheme

    // colorScheme 0: Color (New)
    std::copy_n(&specColormap[0][0], gradientSteps * 3, &gradient_pre[ColorGradientUnselected][0][0][0]);
    std::copy_n(&selColormap[0][0], gradientSteps * 3, &gradient_pre[ColorGradientTimeSelected][0][0][0]);
    std::copy_n(&freqSelColormap[0][0], gradientSteps * 3, &gradient_pre[ColorGradientTimeAndFrequencySelected][0][0][0]);
    std::fill_n(&gradient_pre[ColorGradientEdge][0][0][0], gradientSteps * 3, 0);

    for (int selected = 0; selected < ColorGradientTotal; selected++) {
        // Get color scheme from Theme
        const int gsteps = 4;
        float gradient[gsteps + 1][3];
        theTheme.Colour(clrSpectro1) = theTheme.Colour(clrUnselected);
        theTheme.Colour(clrSpectro1Sel) = theTheme.Colour(clrSelected);
        int clrFirst = (selected == ColorGradientUnselected) ? clrSpectro1 : clrSpectro1Sel;
        for (int j=0; j < (gsteps + 1); j++) {
            QColor c = theTheme.Colour(clrFirst + j);
            gradient[ j] [0] = c.red() / 255.0;
            gradient[ j] [1] = c.green() / 255.0;
            gradient[ j] [2] = c.blue() / 255.0;
        }

        // colorScheme 1: Color (from theme)
        for (int i = 0; i < gradientSteps; i++) {
            float r, g, b;
            float value = float(i) / gradientSteps;

            int left = (int)(value * gsteps);
            int right = (left == gsteps ? gsteps : left + 1);

            float rweight = (value * gsteps) - left;
            float lweight = 1.0 - rweight;

            r = (gradient[left][0] * lweight) + (gradient[right][0] * rweight);
            g = (gradient[left][1] * lweight) + (gradient[right][1] * rweight);
            b = (gradient[left][2] * lweight) + (gradient[right][2] * rweight);

            switch (selected) {
            case ColorGradientUnselected:
                // not dimmed
                break;

            case ColorGradientTimeAndFrequencySelected:
                float temp;
                temp = r;
                r = g;
                g = b;
                b = temp;
                break;

            case ColorGradientTimeSelected:
                // partly dimmed
                r *= 0.75f;
                g *= 0.75f;
                b *= 0.75f;
                break;

            // For now edge colour is just black (or white if grey-scale)
            // Later we might invert or something else funky.
            case ColorGradientEdge:
                // fully dimmed
                r = 0;
                g = 0;
                b = 0;
                break;
            }
            gradient_pre[selected][1][i][0] = (unsigned char)(255 * r);
            gradient_pre[selected][1][i][1] = (unsigned char)(255 * g);
            gradient_pre[selected][1][i][2] = (unsigned char)(255 * b);
        }

        // colorScheme 3: Inverse Grayscale
        for (int i = 0; i < gradientSteps; i++) {
            float r, g, b;
            float value = float(i) / gradientSteps;

            r = g = b = value;

            switch (selected) {
            case ColorGradientUnselected:
                // not dimmed
                break;

            case ColorGradientTimeAndFrequencySelected:
            // else fall through to SAME grayscale colour as normal selection.
            // The white lines show it up clearly enough.

            case ColorGradientTimeSelected:
                // partly dimmed
                r = r * 0.75f + 0.25f;
                g = g * 0.75f + 0.25f;
                b = b * 0.75f + 0.25f;
                break;

            case ColorGradientEdge:
                r = 1.0f;
                g = 1.0f;
                b = 1.0f;
                break;
            }
            gradient_pre[selected][3][i][0] = (unsigned char)(255 * r);
            gradient_pre[selected][3][i][1] = (unsigned char)(255 * g);
            gradient_pre[selected][3][i][2] = (unsigned char)(255 * b);
        }

        // colorScheme 2: Grayscale (=Old grayscale)
        for (int i = 0; i < gradientSteps; i++) {
            float r, g, b;
            float value = float(i) / gradientSteps;

            r = g = b = 0.84 - 0.84 * value;

            switch (selected) {
            case ColorGradientUnselected:
                // not dimmed
                break;

            case ColorGradientTimeAndFrequencySelected:
            // else fall through to SAME grayscale colour as normal selection.
            // The white lines show it up clearly enough.

            case ColorGradientTimeSelected:
                // partly dimmed
                r *= 0.75f;
                g *= 0.75f;
                b *= 0.75f;
                break;

            // For now edge colour is just black (or white if grey-scale)
            // Later we might invert or something else funky.
            case ColorGradientEdge:
                // fully dimmed
                r = 1.0f;
                g = 1.0f;
                b = 1.0f;
                break;
            }
            gradient_pre[selected][2][i][0] = (unsigned char)(255 * r);
            gradient_pre[selected][2][i][1] = (unsigned char)(255 * g);
            gradient_pre[selected][2][i][2] = (unsigned char)(255 * b);
        }
    }
}
