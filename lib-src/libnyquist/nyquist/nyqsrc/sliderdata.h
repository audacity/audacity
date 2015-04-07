/* sliderdata.h -- shared data for slider abstraction in nyquist
 *
 * Roger B. Dannenberg
 * Jan 2013
 */

/* define the shared data */
#define SLIDERS_MAX 1024
extern float slider_array[SLIDERS_MAX];

/* access from the GUI to shared data */
void set_slider(int index, float value);
