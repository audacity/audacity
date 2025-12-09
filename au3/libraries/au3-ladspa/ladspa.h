/* ladspa.h

   Linux Audio Developer's Simple Plugin API Version 1.1[LGPL].
   Copyright (C) 2000-2002 Richard W.E. Furse, Paul Barton-Davis,
   Stefan Westerfeld.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   as published by the Free Software Foundation; either version 2.1 of
   the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA. */

#ifndef LADSPA_INCLUDED
#define LADSPA_INCLUDED

#define LADSPA_VERSION "1.1"
#define LADSPA_VERSION_MAJOR 1
#define LADSPA_VERSION_MINOR 1

#ifdef __cplusplus
extern "C" {
#endif

/*****************************************************************************/

/* Overview:

   There is a large number of synthesis packages in use or development
   on the Linux platform at this time. This API (`The Linux Audio
   Developer's Simple Plugin API') attempts to give programmers the
   ability to write simple `plugin' audio processors in C/C++ and link
   them dynamically (`plug') into a range of these packages (`hosts').
   It should be possible for any host and any plugin to communicate
   completely through this interface.

   This API is deliberately short and simple. To achieve compatibility
   with a range of promising Linux sound synthesis packages it
   attempts to find the `greatest common divisor' in their logical
   behaviour. Having said this, certain limiting decisions are
   implicit, notably the use of a fixed type (LADSPA_Data) for all
   data transfer and absence of a parameterised `initialisation'
   phase. See below for the LADSPA_Data typedef.

   Plugins are expected to distinguish between control and audio
   data. Plugins have `ports' that are inputs or outputs for audio or
   control data and each plugin is `run' for a `block' corresponding
   to a short time interval measured in samples. Audio data is
   communicated using arrays of LADSPA_Data, allowing a block of audio
   to be processed by the plugin in a single pass. Control data is
   communicated using single LADSPA_Data values. Control data has a
   single value at the start of a call to the `run()' or `run_adding()'
   function, and may be considered to remain this value for its
   duration. The plugin may assume that all its input and output ports
   have been connected to the relevant data location (see the
   `connect_port()' function below) before it is asked to run.

   Plugins will reside in shared object files suitable for dynamic
   linking by dlopen() and family. The file will provide a number of
   `plugin types' that can be used to instantiate actual plugins
   (sometimes known as `plugin instances') that can be connected
   together to perform tasks.

   This API contains very limited error-handling. */

/*****************************************************************************/

/* Fundamental data type passed in and out of plugin. This data type
   is used to communicate audio samples and control values. It is
   assumed that the plugin will work sensibly given any numeric input
   value although it may have a preferred range (see hints below).

   For audio it is generally assumed that 1.0f is the `0dB' reference
   amplitude and is a `normal' signal level. */

typedef float LADSPA_Data;

/*****************************************************************************/

/* Special Plugin Properties:

   Optional features of the plugin type are encapsulated in the
   LADSPA_Properties type. This is assembled by ORing individual
   properties together. */

typedef int LADSPA_Properties;

/* Property LADSPA_PROPERTY_REALTIME indicates that the plugin has a
   real-time dependency (e.g. listens to a MIDI device) and so its
   output must not be cached or subject to significant latency. */
#define LADSPA_PROPERTY_REALTIME        0x1

/* Property LADSPA_PROPERTY_INPLACE_BROKEN indicates that the plugin
   may cease to work correctly if the host elects to use the same data
   location for both input and output (see connect_port()). This
   should be avoided as enabling this flag makes it impossible for
   hosts to use the plugin to process audio `in-place.' */
#define LADSPA_PROPERTY_INPLACE_BROKEN  0x2

/* Property LADSPA_PROPERTY_HARD_RT_CAPABLE indicates that the plugin
   is capable of running not only in a conventional host but also in a
   `hard real-time' environment. To qualify for this the plugin must
   satisfy all of the following:

   (1) The plugin must not use malloc(), free() or other heap memory
   management within its run() or run_adding() functions. All NEW
   memory used in run() must be managed via the stack. These
   restrictions only apply to the run() function.

   (2) The plugin will not attempt to make use of any library
   functions with the exceptions of functions in the ANSI standard C
   and C maths libraries, which the host is expected to provide.

   (3) The plugin will not access files, devices, pipes, sockets, IPC
   or any other mechanism that might result in process or thread
   blocking.

   (4) The plugin will take an amount of time to execute a run() or
   run_adding() call approximately of form (A+B*SampleCount) where A
   and B depend on the machine and host in use. This amount of time
   may not depend on input signals or plugin state. The host is left
   the responsibility to perform timings to estimate upper bounds for
   A and B. */
#define LADSPA_PROPERTY_HARD_RT_CAPABLE 0x4

#define LADSPA_IS_REALTIME(x)        ((x) & LADSPA_PROPERTY_REALTIME)
#define LADSPA_IS_INPLACE_BROKEN(x)  ((x) & LADSPA_PROPERTY_INPLACE_BROKEN)
#define LADSPA_IS_HARD_RT_CAPABLE(x) ((x) & LADSPA_PROPERTY_HARD_RT_CAPABLE)

/*****************************************************************************/

/* Plugin Ports:

   Plugins have `ports' that are inputs or outputs for audio or
   data. Ports can communicate arrays of LADSPA_Data (for audio
   inputs/outputs) or single LADSPA_Data values (for control
   input/outputs). This information is encapsulated in the
   LADSPA_PortDescriptor type which is assembled by ORing individual
   properties together.

   Note that a port must be an input or an output port but not both
   and that a port must be a control or audio port but not both. */

typedef int LADSPA_PortDescriptor;

/* Property LADSPA_PORT_INPUT indicates that the port is an input. */
#define LADSPA_PORT_INPUT   0x1

/* Property LADSPA_PORT_OUTPUT indicates that the port is an output. */
#define LADSPA_PORT_OUTPUT  0x2

/* Property LADSPA_PORT_CONTROL indicates that the port is a control
   port. */
#define LADSPA_PORT_CONTROL 0x4

/* Property LADSPA_PORT_AUDIO indicates that the port is a audio
   port. */
#define LADSPA_PORT_AUDIO   0x8

#define LADSPA_IS_PORT_INPUT(x)   ((x) & LADSPA_PORT_INPUT)
#define LADSPA_IS_PORT_OUTPUT(x)  ((x) & LADSPA_PORT_OUTPUT)
#define LADSPA_IS_PORT_CONTROL(x) ((x) & LADSPA_PORT_CONTROL)
#define LADSPA_IS_PORT_AUDIO(x)   ((x) & LADSPA_PORT_AUDIO)

/*****************************************************************************/

/* Plugin Port Range Hints:

   The host may wish to provide a representation of data entering or
   leaving a plugin (e.g. to generate a GUI automatically). To make
   this more meaningful, the plugin should provide `hints' to the host
   describing the usual values taken by the data.

   Note that these are only hints. The host may ignore them and the
   plugin must not assume that data supplied to it is meaningful. If
   the plugin receives invalid input data it is expected to continue
   to run without failure and, where possible, produce a sensible
   output (e.g. a high-pass filter given a negative cutoff frequency
   might switch to an all-pass mode).

   Hints are meaningful for all input and output ports but hints for
   input control ports are expected to be particularly useful.

   More hint information is encapsulated in the
   LADSPA_PortRangeHintDescriptor type which is assembled by ORing
   individual hint types together. Hints may require further
   LowerBound and UpperBound information.

   All the hint information for a particular port is aggregated in the
   LADSPA_PortRangeHint structure. */

typedef int LADSPA_PortRangeHintDescriptor;

/* Hint LADSPA_HINT_BOUNDED_BELOW indicates that the LowerBound field
   of the LADSPA_PortRangeHint should be considered meaningful. The
   value in this field should be considered the (inclusive) lower
   bound of the valid range. If LADSPA_HINT_SAMPLE_RATE is also
   specified then the value of LowerBound should be multiplied by the
   sample rate. */
#define LADSPA_HINT_BOUNDED_BELOW   0x1

/* Hint LADSPA_HINT_BOUNDED_ABOVE indicates that the UpperBound field
   of the LADSPA_PortRangeHint should be considered meaningful. The
   value in this field should be considered the (inclusive) upper
   bound of the valid range. If LADSPA_HINT_SAMPLE_RATE is also
   specified then the value of UpperBound should be multiplied by the
   sample rate. */
#define LADSPA_HINT_BOUNDED_ABOVE   0x2

/* Hint LADSPA_HINT_TOGGLED indicates that the data item should be
   considered a Boolean toggle. Data less than or equal to zero should
   be considered `off' or `false,' and data above zero should be
   considered `on' or `true.' LADSPA_HINT_TOGGLED may not be used in
   conjunction with any other hint except LADSPA_HINT_DEFAULT_0 or
   LADSPA_HINT_DEFAULT_1. */
#define LADSPA_HINT_TOGGLED         0x4

/* Hint LADSPA_HINT_SAMPLE_RATE indicates that any bounds specified
   should be interpreted as multiples of the sample rate. For
   instance, a frequency range from 0Hz to the Nyquist frequency (half
   the sample rate) could be requested by this hint in conjunction
   with LowerBound = 0 and UpperBound = 0.5. Hosts that support bounds
   at all must support this hint to retain meaning. */
#define LADSPA_HINT_SAMPLE_RATE     0x8

/* Hint LADSPA_HINT_LOGARITHMIC indicates that it is likely that the
   user will find it more intuitive to view values using a logarithmic
   scale. This is particularly useful for frequencies and gains. */
#define LADSPA_HINT_LOGARITHMIC     0x10

/* Hint LADSPA_HINT_INTEGER indicates that a user interface would
   probably wish to provide a stepped control taking only integer
   values. Any bounds set should be slightly wider than the actual
   integer range required to avoid floating point rounding errors. For
   instance, the integer set {0,1,2,3} might be described as [-0.1,
   3.1]. */
#define LADSPA_HINT_INTEGER         0x20

/* The various LADSPA_HINT_HAS_DEFAULT_* hints indicate a `normal'
   value for the port that is sensible as a default. For instance,
   this value is suitable for use as an initial value in a user
   interface or as a value the host might assign to a control port
   when the user has not provided one. Defaults are encoded using a
   mask so only one default may be specified for a port. Some of the
   hints make use of lower and upper bounds, in which case the
   relevant bound or bounds must be available and
   LADSPA_HINT_SAMPLE_RATE must be applied as usual. The resulting
   default must be rounded if LADSPA_HINT_INTEGER is present. Default
   values were introduced in LADSPA v1.1. */
#define LADSPA_HINT_DEFAULT_MASK    0x3C0

/* This default values indicates that no default is provided. */
#define LADSPA_HINT_DEFAULT_NONE    0x0

/* This default hint indicates that the suggested lower bound for the
   port should be used. */
#define LADSPA_HINT_DEFAULT_MINIMUM 0x40

/* This default hint indicates that a low value between the suggested
   lower and upper bounds should be chosen. For ports with
   LADSPA_HINT_LOGARITHMIC, this should be exp(log(lower) * 0.75 +
   log(upper) * 0.25). Otherwise, this should be (lower * 0.75 + upper
   * 0.25). */
#define LADSPA_HINT_DEFAULT_LOW     0x80

/* This default hint indicates that a middle value between the
   suggested lower and upper bounds should be chosen. For ports with
   LADSPA_HINT_LOGARITHMIC, this should be exp(log(lower) * 0.5 +
   log(upper) * 0.5). Otherwise, this should be (lower * 0.5 + upper *
   0.5). */
#define LADSPA_HINT_DEFAULT_MIDDLE  0xC0

/* This default hint indicates that a high value between the suggested
   lower and upper bounds should be chosen. For ports with
   LADSPA_HINT_LOGARITHMIC, this should be exp(log(lower) * 0.25 +
   log(upper) * 0.75). Otherwise, this should be (lower * 0.25 + upper
   * 0.75). */
#define LADSPA_HINT_DEFAULT_HIGH    0x100

/* This default hint indicates that the suggested upper bound for the
   port should be used. */
#define LADSPA_HINT_DEFAULT_MAXIMUM 0x140

/* This default hint indicates that the number 0 should be used. Note
   that this default may be used in conjunction with
   LADSPA_HINT_TOGGLED. */
#define LADSPA_HINT_DEFAULT_0       0x200

/* This default hint indicates that the number 1 should be used. Note
   that this default may be used in conjunction with
   LADSPA_HINT_TOGGLED. */
#define LADSPA_HINT_DEFAULT_1       0x240

/* This default hint indicates that the number 100 should be used. */
#define LADSPA_HINT_DEFAULT_100     0x280

/* This default hint indicates that the Hz frequency of `concert A'
   should be used. This will be 440 unless the host uses an unusual
   tuning convention, in which case it may be within a few Hz. */
#define LADSPA_HINT_DEFAULT_440     0x2C0

#define LADSPA_IS_HINT_BOUNDED_BELOW(x)   ((x) & LADSPA_HINT_BOUNDED_BELOW)
#define LADSPA_IS_HINT_BOUNDED_ABOVE(x)   ((x) & LADSPA_HINT_BOUNDED_ABOVE)
#define LADSPA_IS_HINT_TOGGLED(x)         ((x) & LADSPA_HINT_TOGGLED)
#define LADSPA_IS_HINT_SAMPLE_RATE(x)     ((x) & LADSPA_HINT_SAMPLE_RATE)
#define LADSPA_IS_HINT_LOGARITHMIC(x)     ((x) & LADSPA_HINT_LOGARITHMIC)
#define LADSPA_IS_HINT_INTEGER(x)         ((x) & LADSPA_HINT_INTEGER)

#define LADSPA_IS_HINT_HAS_DEFAULT(x)     ((x) & LADSPA_HINT_DEFAULT_MASK)
#define LADSPA_IS_HINT_DEFAULT_MINIMUM(x) (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_MINIMUM)
#define LADSPA_IS_HINT_DEFAULT_LOW(x)     (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_LOW)
#define LADSPA_IS_HINT_DEFAULT_MIDDLE(x)  (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_MIDDLE)
#define LADSPA_IS_HINT_DEFAULT_HIGH(x)    (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_HIGH)
#define LADSPA_IS_HINT_DEFAULT_MAXIMUM(x) (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_MAXIMUM)
#define LADSPA_IS_HINT_DEFAULT_0(x)       (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_0)
#define LADSPA_IS_HINT_DEFAULT_1(x)       (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_1)
#define LADSPA_IS_HINT_DEFAULT_100(x)     (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_100)
#define LADSPA_IS_HINT_DEFAULT_440(x)     (((x) & LADSPA_HINT_DEFAULT_MASK)   \
                                           == LADSPA_HINT_DEFAULT_440)

typedef struct _LADSPA_PortRangeHint {
    /* Hints about the port. */
    LADSPA_PortRangeHintDescriptor HintDescriptor;

    /* Meaningful when hint LADSPA_HINT_BOUNDED_BELOW is active. When
       LADSPA_HINT_SAMPLE_RATE is also active then this value should be
       multiplied by the relevant sample rate. */
    LADSPA_Data LowerBound;

    /* Meaningful when hint LADSPA_HINT_BOUNDED_ABOVE is active. When
       LADSPA_HINT_SAMPLE_RATE is also active then this value should be
       multiplied by the relevant sample rate. */
    LADSPA_Data UpperBound;
} LADSPA_PortRangeHint;

/*****************************************************************************/

/* Plugin Handles:

   This plugin handle indicates a particular instance of the plugin
   concerned. It is valid to compare this to NULL (0 for C++) but
   otherwise the host should not attempt to interpret it. The plugin
   may use it to reference internal instance data. */

typedef void* LADSPA_Handle;

/*****************************************************************************/

/* Descriptor for a Type of Plugin:

   This structure is used to describe a plugin type. It provides a
   number of functions to examine the type, instantiate it, link it to
   buffers and workspaces and to run it. */

typedef struct _LADSPA_Descriptor {
    /* This numeric identifier indicates the plugin type
       uniquely. Plugin programmers may reserve ranges of IDs from a
       central body to avoid clashes. Hosts may assume that IDs are
       below 0x1000000. */
    unsigned long UniqueID;

    /* This identifier can be used as a unique, case-sensitive
       identifier for the plugin type within the plugin file. Plugin
       types should be identified by file and label rather than by index
       or plugin name, which may be changed in NEW plugin
       versions. Labels must not contain white-space characters. */
    const char* Label;

    /* This indicates a number of properties of the plugin. */
    LADSPA_Properties Properties;

    /* This member points to the null-terminated name of the plugin
       (e.g. "Sine Oscillator"). */
    const char* Name;

    /* This member points to the null-terminated string indicating the
       maker of the plugin. This can be an empty string but not NULL. */
    const char* Maker;

    /* This member points to the null-terminated string indicating any
       copyright applying to the plugin. If no Copyright applies the
       string "None" should be used. */
    const char* Copyright;

    /* This indicates the number of ports (input AND output) present on
       the plugin. */
    unsigned long PortCount;

    /* This member indicates an array of port descriptors. Valid indices
       vary from 0 to PortCount-1. */
    const LADSPA_PortDescriptor* PortDescriptors;

    /* This member indicates an array of null-terminated strings
       describing ports (e.g. "Frequency (Hz)"). Valid indices vary from
       0 to PortCount-1. */
    const char* const* PortNames;

    /* This member indicates an array of range hints for each port (see
       above). Valid indices vary from 0 to PortCount-1. */
    const LADSPA_PortRangeHint* PortRangeHints;

    /* This may be used by the plugin developer to pass any custom
       implementation data into an instantiate call. It must not be used
       or interpreted by the host. It is expected that most plugin
       writers will not use this facility as LADSPA_Handle should be
       used to hold instance data. */
    void* ImplementationData;

    /* This member is a function pointer that instantiates a plugin. A
       handle is returned indicating the NEW plugin instance. The
       instantiation function accepts a sample rate as a parameter. The
       plugin descriptor from which this instantiate function was found
       must also be passed. This function must return NULL if
       instantiation fails.

       Note that instance initialisation should generally occur in
       activate() rather than here. */
    LADSPA_Handle (* instantiate)(const struct _LADSPA_Descriptor* Descriptor, unsigned long                     SampleRate);

    /* This member is a function pointer that connects a port on an
       instantiated plugin to a memory location at which a block of data
       for the port will be read/written. The data location is expected
       to be an array of LADSPA_Data for audio ports or a single
       LADSPA_Data value for control ports. Memory issues will be
       managed by the host. The plugin must read/write the data at these
       locations every time run() or run_adding() is called and the data
       present at the time of this connection call should not be
       considered meaningful.

       connect_port() may be called more than once for a plugin instance
       to allow the host to change the buffers that the plugin is
       reading or writing. These calls may be made before or after
       activate() or deactivate() calls.

       connect_port() must be called at least once for each port before
       run() or run_adding() is called. When working with blocks of
       LADSPA_Data the plugin should pay careful attention to the block
       size passed to the run function as the block allocated may only
       just be large enough to contain the block of samples.

       Plugin writers should be aware that the host may elect to use the
       same buffer for more than one port and even use the same buffer
       for both input and output (see LADSPA_PROPERTY_INPLACE_BROKEN).
       However, overlapped buffers or use of a single buffer for both
       audio and control data may result in unexpected behaviour. */
    void (* connect_port)(LADSPA_Handle Instance, unsigned long Port, LADSPA_Data* DataLocation);

    /* This member is a function pointer that initialises a plugin
       instance and activates it for use. This is separated from
       instantiate() to aid real-time support and so that hosts can
       reinitialise a plugin instance by calling deactivate() and then
       activate(). In this case the plugin instance must reset all state
       information dependent on the history of the plugin instance
       except for any data locations provided by connect_port() and any
       gain set by set_run_adding_gain(). If there is nothing for
       activate() to do then the plugin writer may provide a NULL rather
       than an empty function.

       When present, hosts must call this function once before run() (or
       run_adding()) is called for the first time. This call should be
       made as close to the run() call as possible and indicates to
       real-time plugins that they are now live. Plugins should not rely
       on a prompt call to run() after activate(). activate() may not be
       called again unless deactivate() is called first. Note that
       connect_port() may be called before or after a call to
       activate(). */
    void (* activate)(LADSPA_Handle Instance);

    /* This method is a function pointer that runs an instance of a
       plugin for a block. Two parameters are required: the first is a
       handle to the particular instance to be run and the second
       indicates the block size (in samples) for which the plugin
       instance may run.

       Note that if an activate() function exists then it must be called
       before run() or run_adding(). If deactivate() is called for a
       plugin instance then the plugin instance may not be reused until
       activate() has been called again.

       If the plugin has the property LADSPA_PROPERTY_HARD_RT_CAPABLE
       then there are various things that the plugin should not do
       within the run() or run_adding() functions (see above). */
    void (* run)(LADSPA_Handle Instance, unsigned long SampleCount);

    /* This method is a function pointer that runs an instance of a
       plugin for a block. This has identical behaviour to run() except
       in the way data is output from the plugin. When run() is used,
       values are written directly to the memory areas associated with
       the output ports. However when run_adding() is called, values
       must be added to the values already present in the memory
       areas. Furthermore, output values written must be scaled by the
       current gain set by set_run_adding_gain() (see below) before
       addition.

       run_adding() is optional. When it is not provided by a plugin,
       this function pointer must be set to NULL. When it is provided,
       the function set_run_adding_gain() must be provided also. */
    void (* run_adding)(LADSPA_Handle Instance, unsigned long SampleCount);

    /* This method is a function pointer that sets the output gain for
       use when run_adding() is called (see above). If this function is
       never called the gain is assumed to default to 1. Gain
       information should be retained when activate() or deactivate()
       are called.

       This function should be provided by the plugin if and only if the
       run_adding() function is provided. When it is absent this
       function pointer must be set to NULL. */
    void (* set_run_adding_gain)(LADSPA_Handle Instance, LADSPA_Data Gain);

    /* This is the counterpart to activate() (see above). If there is
       nothing for deactivate() to do then the plugin writer may provide
       a NULL rather than an empty function.

       Hosts must deactivate all activated units after they have been
       run() (or run_adding()) for the last time. This call should be
       made as close to the last run() call as possible and indicates to
       real-time plugins that they are no longer live. Plugins should
       not rely on prompt deactivation. Note that connect_port() may be
       called before or after a call to deactivate().

       Deactivation is not similar to pausing as the plugin instance
       will be reinitialised when activate() is called to reuse it. */
    void (* deactivate)(LADSPA_Handle Instance);

    /* Once an instance of a plugin has been finished with it can be
       deleted using the following function. The instance handle passed
       ceases to be valid after this call.

       If activate() was called for a plugin instance then a
       corresponding call to deactivate() must be made before cleanup()
       is called. */
    void (* cleanup)(LADSPA_Handle Instance);
} LADSPA_Descriptor;

/**********************************************************************/

/* Accessing a Plugin: */

/* The exact mechanism by which plugins are loaded is host-dependent,
   however all most hosts will need to know is the name of shared
   object file containing the plugin types. To allow multiple hosts to
   share plugin types, hosts may wish to check for environment
   variable LADSPA_PATH. If present, this should contain a
   colon-separated path indicating directories that should be searched
   (in order) when loading plugin types.

   A plugin programmer must include a function called
   "ladspa_descriptor" with the following function prototype within
   the shared object file. This function will have C-style linkage (if
   you are using C++ this is taken care of by the `extern "C"' clause
   at the top of the file).

   A host will find the plugin shared object file by one means or
   another, find the ladspa_descriptor() function, call it, and
   proceed from there.

   Plugin types are accessed by index (not ID) using values from 0
   upwards. Out of range indexes must result in this function
   returning NULL, so the plugin count can be determined by checking
   for the least index that results in NULL being returned. */

const LADSPA_Descriptor* ladspa_descriptor(unsigned long Index);

/* Datatype corresponding to the ladspa_descriptor() function. */
typedef const LADSPA_Descriptor*
(* LADSPA_Descriptor_Function)(unsigned long Index);

/**********************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* LADSPA_INCLUDED */

/* EOF */
