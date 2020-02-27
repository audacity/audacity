#include <stdlib.h>
#include <string.h>
#ifndef WIN32
#include "config.h"
#endif

#ifdef ENABLE_NLS
#include <libintl.h>
#endif

#define         _ISOC9X_SOURCE  1
#define         _ISOC99_SOURCE  1
#define         __USE_ISOC99    1
#define         __USE_ISOC9X    1

#include <math.h>

#include "ladspa.h"

#ifdef WIN32
#define _WINDOWS_DLL_EXPORT_ __declspec(dllexport)
int bIsFirstTime = 1; 
void _init(); // forward declaration
#else
#define _WINDOWS_DLL_EXPORT_ 
#endif

#line 10 "sc4_1882.xml"

#include "util/db.h"
#include "util/rms.h"

#define A_TBL 256

#define SC4_RMS_PEAK                   0
#define SC4_ATTACK                     1
#define SC4_RELEASE                    2
#define SC4_THRESHOLD                  3
#define SC4_RATIO                      4
#define SC4_KNEE                       5
#define SC4_MAKEUP_GAIN                6
#define SC4_AMPLITUDE                  7
#define SC4_GAIN_RED                   8
#define SC4_LEFT_IN                    9
#define SC4_RIGHT_IN                   10
#define SC4_LEFT_OUT                   11
#define SC4_RIGHT_OUT                  12

static LADSPA_Descriptor *sc4Descriptor = NULL;

typedef struct {
	LADSPA_Data *rms_peak;
	LADSPA_Data *attack;
	LADSPA_Data *release;
	LADSPA_Data *threshold;
	LADSPA_Data *ratio;
	LADSPA_Data *knee;
	LADSPA_Data *makeup_gain;
	LADSPA_Data *amplitude;
	LADSPA_Data *gain_red;
	LADSPA_Data *left_in;
	LADSPA_Data *right_in;
	LADSPA_Data *left_out;
	LADSPA_Data *right_out;
	float        amp;
	float *      as;
	unsigned int count;
	float        env;
	float        env_peak;
	float        env_rms;
	float        gain;
	float        gain_t;
	rms_env *    rms;
	float        sum;
	LADSPA_Data run_adding_gain;
} Sc4;

_WINDOWS_DLL_EXPORT_
const LADSPA_Descriptor *ladspa_descriptor(unsigned long index) {

#ifdef WIN32
	if (bIsFirstTime) {
		_init();
		bIsFirstTime = 0;
	}
#endif
	switch (index) {
	case 0:
		return sc4Descriptor;
	default:
		return NULL;
	}
}

static void cleanupSc4(LADSPA_Handle instance) {
#line 46 "sc4_1882.xml"
	Sc4 *plugin_data = (Sc4 *)instance;
	rms_env_free(plugin_data->rms);
	free(plugin_data->as);
	free(instance);
}

static void connectPortSc4(
 LADSPA_Handle instance,
 unsigned long port,
 LADSPA_Data *data) {
	Sc4 *plugin;

	plugin = (Sc4 *)instance;
	switch (port) {
	case SC4_RMS_PEAK:
		plugin->rms_peak = data;
		break;
	case SC4_ATTACK:
		plugin->attack = data;
		break;
	case SC4_RELEASE:
		plugin->release = data;
		break;
	case SC4_THRESHOLD:
		plugin->threshold = data;
		break;
	case SC4_RATIO:
		plugin->ratio = data;
		break;
	case SC4_KNEE:
		plugin->knee = data;
		break;
	case SC4_MAKEUP_GAIN:
		plugin->makeup_gain = data;
		break;
	case SC4_AMPLITUDE:
		plugin->amplitude = data;
		break;
	case SC4_GAIN_RED:
		plugin->gain_red = data;
		break;
	case SC4_LEFT_IN:
		plugin->left_in = data;
		break;
	case SC4_RIGHT_IN:
		plugin->right_in = data;
		break;
	case SC4_LEFT_OUT:
		plugin->left_out = data;
		break;
	case SC4_RIGHT_OUT:
		plugin->right_out = data;
		break;
	}
}

static LADSPA_Handle instantiateSc4(
 const LADSPA_Descriptor *descriptor,
 unsigned long s_rate) {
	Sc4 *plugin_data = (Sc4 *)malloc(sizeof(Sc4));
	float amp;
	float *as = NULL;
	unsigned int count;
	float env;
	float env_peak;
	float env_rms;
	float gain;
	float gain_t;
	rms_env *rms = NULL;
	float sum;

#line 23 "sc4_1882.xml"
	unsigned int i;
	float sample_rate = (float)s_rate;

	rms = rms_env_new();
	sum = 0.0f;
	amp = 0.0f;
	gain = 0.0f;
	gain_t = 0.0f;
	env = 0.0f;
	env_rms = 0.0f;
	env_peak = 0.0f;
	count = 0;

	as = malloc(A_TBL * sizeof(float));
	as[0] = 1.0f;
	for (i=1; i<A_TBL; i++) {
	  as[i] = expf(-1.0f / (sample_rate * (float)i / (float)A_TBL));
	}

	db_init();

	plugin_data->amp = amp;
	plugin_data->as = as;
	plugin_data->count = count;
	plugin_data->env = env;
	plugin_data->env_peak = env_peak;
	plugin_data->env_rms = env_rms;
	plugin_data->gain = gain;
	plugin_data->gain_t = gain_t;
	plugin_data->rms = rms;
	plugin_data->sum = sum;

	return (LADSPA_Handle)plugin_data;
}

#undef buffer_write
#undef RUN_ADDING
#undef RUN_REPLACING

#define buffer_write(b, v) (b = v)
#define RUN_ADDING    0
#define RUN_REPLACING 1

static void runSc4(LADSPA_Handle instance, unsigned long sample_count) {
	Sc4 *plugin_data = (Sc4 *)instance;

	/* RMS/peak (float value) */
	const LADSPA_Data rms_peak = *(plugin_data->rms_peak);

	/* Attack time (ms) (float value) */
	const LADSPA_Data attack = *(plugin_data->attack);

	/* Release time (ms) (float value) */
	const LADSPA_Data release = *(plugin_data->release);

	/* Threshold level (dB) (float value) */
	const LADSPA_Data threshold = *(plugin_data->threshold);

	/* Ratio (1:n) (float value) */
	const LADSPA_Data ratio = *(plugin_data->ratio);

	/* Knee radius (dB) (float value) */
	const LADSPA_Data knee = *(plugin_data->knee);

	/* Makeup gain (dB) (float value) */
	const LADSPA_Data makeup_gain = *(plugin_data->makeup_gain);

	/* Left input (array of floats of length sample_count) */
	const LADSPA_Data * const left_in = plugin_data->left_in;

	/* Right input (array of floats of length sample_count) */
	const LADSPA_Data * const right_in = plugin_data->right_in;

	/* Left output (array of floats of length sample_count) */
	LADSPA_Data * const left_out = plugin_data->left_out;

	/* Right output (array of floats of length sample_count) */
	LADSPA_Data * const right_out = plugin_data->right_out;
	float amp = plugin_data->amp;
	float * as = plugin_data->as;
	unsigned int count = plugin_data->count;
	float env = plugin_data->env;
	float env_peak = plugin_data->env_peak;
	float env_rms = plugin_data->env_rms;
	float gain = plugin_data->gain;
	float gain_t = plugin_data->gain_t;
	rms_env * rms = plugin_data->rms;
	float sum = plugin_data->sum;

#line 51 "sc4_1882.xml"
	unsigned long pos;

	const float ga = attack < 2.0f ? 0.0f : as[f_round(attack * 0.001f * (float)(A_TBL-1))];
	const float gr = as[f_round(release * 0.001f * (float)(A_TBL-1))];
	const float rs = (ratio - 1.0f) / ratio;
	const float mug = db2lin(makeup_gain);
	const float knee_min = db2lin(threshold - knee);
	const float knee_max = db2lin(threshold + knee);
	const float ef_a = ga * 0.25f;
	const float ef_ai = 1.0f - ef_a;

	for (pos = 0; pos < sample_count; pos++) {
	  const float la = fabs(left_in[pos]);
	  const float ra = fabs(right_in[pos]);
	  const float lev_in = f_max(la, ra);
	  sum += lev_in * lev_in;

	  if (amp > env_rms) {
	    env_rms = env_rms * ga + amp * (1.0f - ga);
	  } else {
	    env_rms = env_rms * gr + amp * (1.0f - gr);
	  }
	  round_to_zero(&env_rms);
	  if (lev_in > env_peak) {
	    env_peak = env_peak * ga + lev_in * (1.0f - ga);
	  } else {
	    env_peak = env_peak * gr + lev_in * (1.0f - gr);
	  }
	  round_to_zero(&env_peak);
	  if ((count++ & 3) == 3) {
	    amp = rms_env_process(rms, sum * 0.25f);
	    sum = 0.0f;
	    if (isnan(env_rms)) {
	      // This can happen sometimes, but I don't know why
	      env_rms = 0.0f;
	    }

	    env = LIN_INTERP(rms_peak, env_rms, env_peak);

	    if (env <= knee_min) {
	      gain_t = 1.0f;
	    } else if (env < knee_max) {
	      const float x = -(threshold - knee - lin2db(env)) / knee;
	      gain_t = db2lin(-knee * rs * x * x * 0.25f);
	    } else {
	      gain_t = db2lin((threshold - lin2db(env)) * rs);
	    }
	  }
	  gain = gain * ef_a + gain_t * ef_ai;
	  buffer_write(left_out[pos], left_in[pos] * gain * mug);
	  buffer_write(right_out[pos], right_in[pos] * gain * mug);
	}
	plugin_data->sum = sum;
	plugin_data->amp = amp;
	plugin_data->gain = gain;
	plugin_data->gain_t = gain_t;
	plugin_data->env = env;
	plugin_data->env_rms = env_rms;
	plugin_data->env_peak = env_peak;
	plugin_data->count = count;

	*(plugin_data->amplitude) = lin2db(env);
	*(plugin_data->gain_red) = lin2db(gain);
}
#undef buffer_write
#undef RUN_ADDING
#undef RUN_REPLACING

#define buffer_write(b, v) (b += (v) * run_adding_gain)
#define RUN_ADDING    1
#define RUN_REPLACING 0

static void setRunAddingGainSc4(LADSPA_Handle instance, LADSPA_Data gain) {
	((Sc4 *)instance)->run_adding_gain = gain;
}

static void runAddingSc4(LADSPA_Handle instance, unsigned long sample_count) {
	Sc4 *plugin_data = (Sc4 *)instance;
	LADSPA_Data run_adding_gain = plugin_data->run_adding_gain;

	/* RMS/peak (float value) */
	const LADSPA_Data rms_peak = *(plugin_data->rms_peak);

	/* Attack time (ms) (float value) */
	const LADSPA_Data attack = *(plugin_data->attack);

	/* Release time (ms) (float value) */
	const LADSPA_Data release = *(plugin_data->release);

	/* Threshold level (dB) (float value) */
	const LADSPA_Data threshold = *(plugin_data->threshold);

	/* Ratio (1:n) (float value) */
	const LADSPA_Data ratio = *(plugin_data->ratio);

	/* Knee radius (dB) (float value) */
	const LADSPA_Data knee = *(plugin_data->knee);

	/* Makeup gain (dB) (float value) */
	const LADSPA_Data makeup_gain = *(plugin_data->makeup_gain);

	/* Left input (array of floats of length sample_count) */
	const LADSPA_Data * const left_in = plugin_data->left_in;

	/* Right input (array of floats of length sample_count) */
	const LADSPA_Data * const right_in = plugin_data->right_in;

	/* Left output (array of floats of length sample_count) */
	LADSPA_Data * const left_out = plugin_data->left_out;

	/* Right output (array of floats of length sample_count) */
	LADSPA_Data * const right_out = plugin_data->right_out;
	float amp = plugin_data->amp;
	float * as = plugin_data->as;
	unsigned int count = plugin_data->count;
	float env = plugin_data->env;
	float env_peak = plugin_data->env_peak;
	float env_rms = plugin_data->env_rms;
	float gain = plugin_data->gain;
	float gain_t = plugin_data->gain_t;
	rms_env * rms = plugin_data->rms;
	float sum = plugin_data->sum;

#line 51 "sc4_1882.xml"
	unsigned long pos;

	const float ga = attack < 2.0f ? 0.0f : as[f_round(attack * 0.001f * (float)(A_TBL-1))];
	const float gr = as[f_round(release * 0.001f * (float)(A_TBL-1))];
	const float rs = (ratio - 1.0f) / ratio;
	const float mug = db2lin(makeup_gain);
	const float knee_min = db2lin(threshold - knee);
	const float knee_max = db2lin(threshold + knee);
	const float ef_a = ga * 0.25f;
	const float ef_ai = 1.0f - ef_a;

	for (pos = 0; pos < sample_count; pos++) {
	  const float la = fabs(left_in[pos]);
	  const float ra = fabs(right_in[pos]);
	  const float lev_in = f_max(la, ra);
	  sum += lev_in * lev_in;

	  if (amp > env_rms) {
	    env_rms = env_rms * ga + amp * (1.0f - ga);
	  } else {
	    env_rms = env_rms * gr + amp * (1.0f - gr);
	  }
	  round_to_zero(&env_rms);
	  if (lev_in > env_peak) {
	    env_peak = env_peak * ga + lev_in * (1.0f - ga);
	  } else {
	    env_peak = env_peak * gr + lev_in * (1.0f - gr);
	  }
	  round_to_zero(&env_peak);
	  if ((count++ & 3) == 3) {
	    amp = rms_env_process(rms, sum * 0.25f);
	    sum = 0.0f;
	    if (isnan(env_rms)) {
	      // This can happen sometimes, but I don't know why
	      env_rms = 0.0f;
	    }

	    env = LIN_INTERP(rms_peak, env_rms, env_peak);

	    if (env <= knee_min) {
	      gain_t = 1.0f;
	    } else if (env < knee_max) {
	      const float x = -(threshold - knee - lin2db(env)) / knee;
	      gain_t = db2lin(-knee * rs * x * x * 0.25f);
	    } else {
	      gain_t = db2lin((threshold - lin2db(env)) * rs);
	    }
	  }
	  gain = gain * ef_a + gain_t * ef_ai;
	  buffer_write(left_out[pos], left_in[pos] * gain * mug);
	  buffer_write(right_out[pos], right_in[pos] * gain * mug);
	}
	plugin_data->sum = sum;
	plugin_data->amp = amp;
	plugin_data->gain = gain;
	plugin_data->gain_t = gain_t;
	plugin_data->env = env;
	plugin_data->env_rms = env_rms;
	plugin_data->env_peak = env_peak;
	plugin_data->count = count;

	*(plugin_data->amplitude) = lin2db(env);
	*(plugin_data->gain_red) = lin2db(gain);
}

void _init() {
	char **port_names;
	LADSPA_PortDescriptor *port_descriptors;
	LADSPA_PortRangeHint *port_range_hints;

#ifdef ENABLE_NLS
#define D_(s) dgettext(PACKAGE, s)
	setlocale(LC_ALL, "");
	bindtextdomain(PACKAGE, PACKAGE_LOCALE_DIR);
#else
#define D_(s) (s)
#endif


	sc4Descriptor =
	 (LADSPA_Descriptor *)malloc(sizeof(LADSPA_Descriptor));

	if (sc4Descriptor) {
		sc4Descriptor->UniqueID = 1882;
		sc4Descriptor->Label = "sc4";
		sc4Descriptor->Properties =
		 LADSPA_PROPERTY_HARD_RT_CAPABLE;
		sc4Descriptor->Name =
		 D_("SC4");
		sc4Descriptor->Maker =
		 "Steve Harris <steve@plugin.org.uk>";
		sc4Descriptor->Copyright =
		 "GPL";
		sc4Descriptor->PortCount = 13;

		port_descriptors = (LADSPA_PortDescriptor *)calloc(13,
		 sizeof(LADSPA_PortDescriptor));
		sc4Descriptor->PortDescriptors =
		 (const LADSPA_PortDescriptor *)port_descriptors;

		port_range_hints = (LADSPA_PortRangeHint *)calloc(13,
		 sizeof(LADSPA_PortRangeHint));
		sc4Descriptor->PortRangeHints =
		 (const LADSPA_PortRangeHint *)port_range_hints;

		port_names = (char **)calloc(13, sizeof(char*));
		sc4Descriptor->PortNames =
		 (const char **)port_names;

		/* Parameters for RMS/peak */
		port_descriptors[SC4_RMS_PEAK] =
		 LADSPA_PORT_INPUT | LADSPA_PORT_CONTROL;
		port_names[SC4_RMS_PEAK] =
		 D_("RMS/peak");
		port_range_hints[SC4_RMS_PEAK].HintDescriptor =
		 LADSPA_HINT_BOUNDED_BELOW | LADSPA_HINT_BOUNDED_ABOVE | LADSPA_HINT_DEFAULT_MINIMUM;
		port_range_hints[SC4_RMS_PEAK].LowerBound = 0;
		port_range_hints[SC4_RMS_PEAK].UpperBound = 1;

		/* Parameters for Attack time (ms) */
		port_descriptors[SC4_ATTACK] =
		 LADSPA_PORT_INPUT | LADSPA_PORT_CONTROL;
		port_names[SC4_ATTACK] =
		 D_("Attack time (ms)");
		port_range_hints[SC4_ATTACK].HintDescriptor =
		 LADSPA_HINT_BOUNDED_BELOW | LADSPA_HINT_BOUNDED_ABOVE | LADSPA_HINT_DEFAULT_LOW;
		port_range_hints[SC4_ATTACK].LowerBound = 1.5;
		port_range_hints[SC4_ATTACK].UpperBound = 400;

		/* Parameters for Release time (ms) */
		port_descriptors[SC4_RELEASE] =
		 LADSPA_PORT_INPUT | LADSPA_PORT_CONTROL;
		port_names[SC4_RELEASE] =
		 D_("Release time (ms)");
		port_range_hints[SC4_RELEASE].HintDescriptor =
		 LADSPA_HINT_BOUNDED_BELOW | LADSPA_HINT_BOUNDED_ABOVE | LADSPA_HINT_DEFAULT_MIDDLE;
		port_range_hints[SC4_RELEASE].LowerBound = 2;
		port_range_hints[SC4_RELEASE].UpperBound = 800;

		/* Parameters for Threshold level (dB) */
		port_descriptors[SC4_THRESHOLD] =
		 LADSPA_PORT_INPUT | LADSPA_PORT_CONTROL;
		port_names[SC4_THRESHOLD] =
		 D_("Threshold level (dB)");
		port_range_hints[SC4_THRESHOLD].HintDescriptor =
		 LADSPA_HINT_BOUNDED_BELOW | LADSPA_HINT_BOUNDED_ABOVE | LADSPA_HINT_DEFAULT_MAXIMUM;
		port_range_hints[SC4_THRESHOLD].LowerBound = -30;
		port_range_hints[SC4_THRESHOLD].UpperBound = 0;

		/* Parameters for Ratio (1:n) */
		port_descriptors[SC4_RATIO] =
		 LADSPA_PORT_INPUT | LADSPA_PORT_CONTROL;
		port_names[SC4_RATIO] =
		 D_("Ratio (1:n)");
		port_range_hints[SC4_RATIO].HintDescriptor =
		 LADSPA_HINT_BOUNDED_BELOW | LADSPA_HINT_BOUNDED_ABOVE | LADSPA_HINT_DEFAULT_1;
		port_range_hints[SC4_RATIO].LowerBound = 1;
		port_range_hints[SC4_RATIO].UpperBound = 20;

		/* Parameters for Knee radius (dB) */
		port_descriptors[SC4_KNEE] =
		 LADSPA_PORT_INPUT | LADSPA_PORT_CONTROL;
		port_names[SC4_KNEE] =
		 D_("Knee radius (dB)");
		port_range_hints[SC4_KNEE].HintDescriptor =
		 LADSPA_HINT_BOUNDED_BELOW | LADSPA_HINT_BOUNDED_ABOVE | LADSPA_HINT_DEFAULT_LOW;
		port_range_hints[SC4_KNEE].LowerBound = 1;
		port_range_hints[SC4_KNEE].UpperBound = 10;

		/* Parameters for Makeup gain (dB) */
		port_descriptors[SC4_MAKEUP_GAIN] =
		 LADSPA_PORT_INPUT | LADSPA_PORT_CONTROL;
		port_names[SC4_MAKEUP_GAIN] =
		 D_("Makeup gain (dB)");
		port_range_hints[SC4_MAKEUP_GAIN].HintDescriptor =
		 LADSPA_HINT_BOUNDED_BELOW | LADSPA_HINT_BOUNDED_ABOVE | LADSPA_HINT_DEFAULT_0;
		port_range_hints[SC4_MAKEUP_GAIN].LowerBound = 0;
		port_range_hints[SC4_MAKEUP_GAIN].UpperBound = +24;

		/* Parameters for Amplitude (dB) */
		port_descriptors[SC4_AMPLITUDE] =
		 LADSPA_PORT_OUTPUT | LADSPA_PORT_CONTROL;
		port_names[SC4_AMPLITUDE] =
		 D_("Amplitude (dB)");
		port_range_hints[SC4_AMPLITUDE].HintDescriptor =
		 LADSPA_HINT_BOUNDED_BELOW | LADSPA_HINT_BOUNDED_ABOVE;
		port_range_hints[SC4_AMPLITUDE].LowerBound = -40;
		port_range_hints[SC4_AMPLITUDE].UpperBound = +12;

		/* Parameters for Gain reduction (dB) */
		port_descriptors[SC4_GAIN_RED] =
		 LADSPA_PORT_OUTPUT | LADSPA_PORT_CONTROL;
		port_names[SC4_GAIN_RED] =
		 D_("Gain reduction (dB)");
		port_range_hints[SC4_GAIN_RED].HintDescriptor =
		 LADSPA_HINT_BOUNDED_BELOW | LADSPA_HINT_BOUNDED_ABOVE;
		port_range_hints[SC4_GAIN_RED].LowerBound = -24;
		port_range_hints[SC4_GAIN_RED].UpperBound = 0;

		/* Parameters for Left input */
		port_descriptors[SC4_LEFT_IN] =
		 LADSPA_PORT_INPUT | LADSPA_PORT_AUDIO;
		port_names[SC4_LEFT_IN] =
		 D_("Left input");
		port_range_hints[SC4_LEFT_IN].HintDescriptor = 0;

		/* Parameters for Right input */
		port_descriptors[SC4_RIGHT_IN] =
		 LADSPA_PORT_INPUT | LADSPA_PORT_AUDIO;
		port_names[SC4_RIGHT_IN] =
		 D_("Right input");
		port_range_hints[SC4_RIGHT_IN].HintDescriptor = 0;

		/* Parameters for Left output */
		port_descriptors[SC4_LEFT_OUT] =
		 LADSPA_PORT_OUTPUT | LADSPA_PORT_AUDIO;
		port_names[SC4_LEFT_OUT] =
		 D_("Left output");
		port_range_hints[SC4_LEFT_OUT].HintDescriptor = 0;

		/* Parameters for Right output */
		port_descriptors[SC4_RIGHT_OUT] =
		 LADSPA_PORT_OUTPUT | LADSPA_PORT_AUDIO;
		port_names[SC4_RIGHT_OUT] =
		 D_("Right output");
		port_range_hints[SC4_RIGHT_OUT].HintDescriptor = 0;

		sc4Descriptor->activate = NULL;
		sc4Descriptor->cleanup = cleanupSc4;
		sc4Descriptor->connect_port = connectPortSc4;
		sc4Descriptor->deactivate = NULL;
		sc4Descriptor->instantiate = instantiateSc4;
		sc4Descriptor->run = runSc4;
		sc4Descriptor->run_adding = runAddingSc4;
		sc4Descriptor->set_run_adding_gain = setRunAddingGainSc4;
	}
}

void _fini() {
	if (sc4Descriptor) {
		free((LADSPA_PortDescriptor *)sc4Descriptor->PortDescriptors);
		free((char **)sc4Descriptor->PortNames);
		free((LADSPA_PortRangeHint *)sc4Descriptor->PortRangeHints);
		free(sc4Descriptor);
	}

}
