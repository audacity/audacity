/* sliderdata.c -- shared data for slider abstraction in nyquist
 *
 * Roger B. Dannenberg
 * Jan 2013
 */

#include "sliderdata.h"

float slider_array[SLIDERS_MAX];

void set_slider(int index, float value)
{
    if (index >= 0 && index < SLIDERS_MAX) {
        slider_array[index] = value;
    }
}


