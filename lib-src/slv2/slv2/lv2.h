/* LV2 - LADSPA (Linux Audio Developer's Simple Plugin API) Version 2
 * Revision 1
 *
 * Copyright (C) 2000-2002 Richard W.E. Furse, Paul Barton-Davis,
 *                         Stefan Westerfeld.
 * Copyright (C) 2006-2008 Steve Harris, Dave Robillard.
 *
 * This header is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License,
 * or (at your option) any later version.
 *
 * This header is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
 * USA.
 */

#ifndef LV2_H_INCLUDED
#define LV2_H_INCLUDED

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif


/* ************************************************************************* */


/** @file lv2.h
 *
 * Revision: 1
 *
 * == Overview ==
 *
 * There are a large number of open source and free software synthesis
 * packages in use or development at this time. This API ('LV2')
 * attempts to give programmers the ability to write simple 'plugin'
 * audio processors in C/C++ and link them dynamically ('plug') into
 * a range of these packages ('hosts').  It should be possible for any
 * host and any plugin to communicate completely through this interface.
 *
 * This API is deliberately as short and simple as possible.
 * The information required to use a plugin is in a companion data
 * (RDF) file.  The shared library portion of the API (defined in this
 * header) does not contain enough information to make use of the plugin
 * possible - the data file is mandatory.
 *
 * Plugins are expected to distinguish between control rate and audio
 * rate data (or other types of data defined by extensions). Plugins have
 * 'ports' that are inputs or outputs and each plugin is 'run' for a 'block'
 * corresponding to a short time interval measured in samples. Audio rate
 * data is communicated using arrays with one element per sample processed,
 * allowing a block of audio to be processed by the plugin in a single
 * pass. Control rate data is communicated using single values. Control
 * rate data has a single value at the start of a call to the 'run()'
 * function, and may be considered to remain this value for its duration.
 * Thus the 'control rate' is determined by the block size, controlled by
 * the host.  The plugin may assume that all its input and output ports have
 * been connected to the relevant data location (see the 'connect_port()'
 * function below) before it is asked to run, unless the port has been set
 * 'connection optional' in the plugin's data file.
 *
 * Plugins will reside in shared object files suitable for dynamic linking
 * by dlopen() and family. The file will provide a number of 'plugin
 * types' that can be used to instantiate actual plugins (sometimes known
 * as 'plugin instances') that can be connected together to perform tasks.
 * The host can access these plugin types using the lv2_descriptor() 
 * function.
 *
 * This API contains very limited error-handling.
 *
 * == Threading rules ==
 *
 * Certain hosts may need to call the functions provided by a plugin from
 * multiple threads. For this to be safe, the plugin must be written so that
 * those functions can be executed simultaneously without problems.
 * To facilitate this, the functions provided by a plugin are divided into
 * classes:
 *
 *  - Discovery class:     lv2_descriptor(), extension_data()
 *  - Instantiation class: instantiate(), cleanup(), activate(), deactivate()
 *  - Audio class:         run(), connect_port()
 *
 * Extensions to this specification which add new functions MUST declare in
 * which of these classes the functions belong, or define new classes for them.
 * The rules that hosts must follow are these:
 * 
 *  - When a function from the Discovery class is running, no other 
 *    functions in the same shared object file may run.
 *  - When a function from the Instantiation class is running for a plugin 
 *    instance, no other functions for that instance may run.
 *  - When a function is running for a plugin instance, no other 
 *    function in the same class may run for that instance.
 *
 * Any simultaneous calls that are not explicitly forbidden by these rules
 * are allowed. For example, a host may call run() for two different plugin 
 * instances simultaneously.
 */


/* ************************************************************************* */

    
/** Plugin Handle.
 *
 * This plugin handle indicates a particular instance of the plugin
 * concerned. It is valid to compare this to NULL (0 for C++) but
 * otherwise the host MUST NOT attempt to interpret it. The plugin
 * may use it to reference internal instance data. */
typedef void * LV2_Handle;


/* ************************************************************************* */


/** Feature data.
 * 
 * These are passed to a plugin's instantiate method to represent a special
 * feature the host has which the plugin may depend on.  This is to allow
 * extensions to the LV2 specification without causing any breakage.
 * Extensions may specify what data needs to be passed here.  The base
 * LV2 specification does not define any features; hosts are not required
 * to use this facility. */
typedef struct _LV2_Feature {
	/** A globally unique, case-sensitive identifier for this feature.
	 *
	 * This MUST be defined in the specification of any LV2 extension which
	 * defines a host feature. */
	const char * URI;

	/** Pointer to arbitrary data.
	 *
	 * This is to allow hosts to pass data to a plugin (simple values, data
	 * structures, function pointers, etc) as part of a 'feature'.  The LV2
	 * specification makes no restrictions on the contents of this data.
	 * The data here MUST be cleary defined by the LV2 extension which defines
	 * this feature.
	 * If no data is required, this may be set to NULL. */
	void * data;
} LV2_Feature;


/* ************************************************************************* */


/** Descriptor for a Type of Plugin.
 * 
 * This structure is used to describe a plugin type. It provides a number
 * of functions to instantiate it, link it to buffers and run it. */
typedef struct _LV2_Descriptor { 

	/** A globally unique, case-sensitive identifier for this plugin type.
	 *
	 * All plugins with the same URI MUST be compatible in terms of 'port
	 * signature', meaning they have the same number of ports, same port
	 * shortnames, and roughly the same functionality.  URIs should
	 * probably contain a version number (or similar) for this reason.
	 *
	 * Rationale:  When serializing session/patch/etc files, hosts MUST
	 * refer to a loaded plugin by the plugin URI only.  In the future
	 * loading a plugin with this URI MUST yield a plugin with the
	 * same ports (etc) which is 100% compatible. */
	const char * URI;

	/** Function pointer that instantiates a plugin.
	 *
	 * A handle is returned indicating the new plugin instance. The
	 * instantiation function accepts a sample rate as a parameter as well
	 * as the plugin descriptor from which this instantiate function was
	 * found. This function must return NULL if instantiation fails.
	 *
	 * bundle_path is a string of the path to the LV2 bundle which contains
	 * this plugin binary.  It MUST include the trailing directory separator
	 * (e.g. '/') so that BundlePath + filename gives the path to a file
	 * in the bundle.
	 *
	 * features is a NULL terminated array of LV2_Feature structs which
	 * represent the features the host supports. Plugins may refuse to
	 * instantiate if required features are not found here (however hosts
	 * SHOULD NOT use this as a discovery mechanism, instead reading the
	 * data file before attempting to instantiate the plugin).  This array
	 * must always exist; if a host has no features, it MUST pass a single
	 * element array containing NULL (to simplify plugins).
	 *
	 * Note that instance initialisation should generally occur in
	 * activate() rather than here.  If a host calls instantiate, it MUST
	 * call cleanup() at some point in the future. */
	LV2_Handle (*instantiate)(const struct _LV2_Descriptor * descriptor,
	                          double                         sample_rate,
	                          const char *                   bundle_path,
	                          const LV2_Feature *const *     features);

	/** Function pointer that connects a port on a plugin instance to a memory
	 * location where the block of data for the port will be read/written.
	 *
	 * The data location is expected to be of the type defined in the
	 * plugin's data file (e.g. an array of float for an lv2:AudioPort).
	 * Memory issues are managed by the host. The plugin must read/write
	 * the data at these locations every time run() is called, data
	 * present at the time of this connection call MUST NOT be
	 * considered meaningful. 
	 *
	 * The host MUST NOT try to connect a data buffer to a port index 
	 * that is not defined in the RDF data for the plugin. If it does, 
	 * the plugin's behaviour is undefined.
	 *
	 * connect_port() may be called more than once for a plugin instance
	 * to allow the host to change the buffers that the plugin is reading
	 * or writing. These calls may be made before or after activate()
	 * or deactivate() calls.  Note that there may be realtime constraints
	 * on connect_port (see lv2:hardRTCapable in lv2.ttl).
	 *
	 * connect_port() MUST be called at least once for each port before
	 * run() is called.  The plugin must pay careful attention to the block
	 * size passed to the run function as the block allocated may only just
	 * be large enough to contain the block of data (typically samples), and
	 * is not guaranteed to be constant.
	 *
	 * Plugin writers should be aware that the host may elect to use the
	 * same buffer for more than one port and even use the same buffer for
	 * both input and output (see lv2:inPlaceBroken in lv2.ttl).
	 * However, overlapped buffers or use of a single buffer for both
	 * audio and control data may result in unexpected behaviour.
	 *
	 * If the plugin has the feature lv2:hardRTCapable then there are 
	 * various things that the plugin MUST NOT do within the connect_port()
	 * function (see lv2.ttl). */
	void (*connect_port)(LV2_Handle instance,
	                     uint32_t   port,
	                     void *     data_location);

	/** Function pointer that initialises a plugin instance and activates
	 * it for use.
	 * 
	 * This is separated from instantiate() to aid real-time support and so
	 * that hosts can reinitialise a plugin instance by calling deactivate()
	 * and then activate(). In this case the plugin instance must reset all
	 * state information dependent on the history of the plugin instance
	 * except for any data locations provided by connect_port(). If there
	 * is nothing for activate() to do then the plugin writer may provide
	 * a NULL rather than an empty function.
	 *
	 * When present, hosts MUST call this function once before run()
	 * is called for the first time. This call SHOULD be made as close
	 * to the run() call as possible and indicates to real-time plugins
	 * that they are now live, however plugins MUST NOT rely on a prompt
	 * call to run() after activate().  activate() may not be called again
	 * unless deactivate() is called first (after which activate() may be
	 * called again, followed by deactivate, etc. etc.).  If a host calls
	 * activate, it MUST call deactivate at some point in the future.
	 *
	 * Note that connect_port() may be called before or after a call to
	 * activate(). */
	void (*activate)(LV2_Handle instance);

	/** Function pointer that runs a plugin instance for a block.
	 *
	 * Two parameters are required: the first is a handle to the particular
	 * instance to be run and the second indicates the block size (in
	 * samples) for which the plugin instance may run.
	 *
	 * Note that if an activate() function exists then it must be called
	 * before run(). If deactivate() is called for a plugin instance then
	 * the plugin instance may not be reused until activate() has been
	 * called again.
	 *
	 * If the plugin has the feature lv2:hardRTCapable then there are 
	 * various things that the plugin MUST NOT do within the run()
	 * function (see lv2.ttl). */
	void (*run)(LV2_Handle instance,
	            uint32_t   sample_count);

	/** This is the counterpart to activate() (see above). If there is
	 * nothing for deactivate() to do then the plugin writer may provide
	 * a NULL rather than an empty function.
	 *
	 * Hosts must deactivate all activated units after they have been run()
	 * for the last time. This call SHOULD be made as close to the last
	 * run() call as possible and indicates to real-time plugins that
	 * they are no longer live, however plugins MUST NOT rely on prompt
	 * deactivation. Note that connect_port() may be called before or
	 * after a call to deactivate().
	 *
	 * Note that deactivation is not similar to pausing as the plugin
	 * instance will be reinitialised when activate() is called to reuse it.
	 * Hosts MUST NOT call deactivate() unless activate() was previously
	 * called. */
	void (*deactivate)(LV2_Handle instance);

	/** This is the counterpart to instantiate() (see above).  Once an instance
	 * of a plugin has been finished with it can be deleted using this
	 * function. The instance handle passed ceases to be valid after
	 * this call.
	 * 
	 * If activate() was called for a plugin instance then a corresponding
	 * call to deactivate() MUST be made before cleanup() is called.
	 * Hosts MUST NOT call cleanup() unless instantiate() was previously
	 * called. */
	void (*cleanup)(LV2_Handle instance);

	/** Function pointer that can be used to return additional instance data for
	 * a plugin defined by some extenion (e.g. a struct containing additional
	 * function pointers).
	 *
	 * The actual type and meaning of the returned object MUST be specified 
	 * precisely by the extension if it defines any extra data.  If a particular
	 * extension does not define extra instance data, this function MUST return
	 * NULL for that extension's URI.  If a plugin does not support any
	 * extensions that define extra instance data, this function pointer may be
	 * set to NULL rather than providing an empty function.
	 * 
	 * The only parameter is the URI of the extension. The plugin MUST return
	 * NULL if it does not support the extension, but hosts SHOULD NOT use this
	 * as a discovery method (e.g. hosts should only call this function for
	 * extensions known to be supported by the plugin from the data file).
	 *
	 * The host is never responsible for freeing the returned value.
	 * 
	 * NOTE: This function should return a struct (likely containing function
	 * pointers) and NOT a direct function pointer.  Standard C and C++ do not
	 * allow type casts from void* to a function pointer type.  To provide
	 * additional functions a struct should be returned containing the extra
	 * function pointers (which is valid standard code, and a much better idea
	 * for extensibility anyway). */
	const void* (*extension_data)(const char * uri); 

} LV2_Descriptor;


/* ****************************************************************** */


/** Accessing Plugin Types.
 *
 * The exact mechanism by which plugins are loaded is host-dependent,
 * however all most hosts will need to know is the URI of the plugin they
 * wish to load.  The environment variable LV2_PATH, if present, should
 * contain a colon-separated path indicating directories (containing
 * plugin bundle subdirectories) that should be searched (in order)
 * for plugins.  It is expected that hosts will use a library to provide
 * this functionality.
 * 
 * A plugin programmer must include a function called "lv2_descriptor"
 * with the following function prototype within the shared object
 * file. This function will have C-style linkage (if you are using
 * C++ this is taken care of by the 'extern "C"' clause at the top of
 * the file).
 *
 * A host will find the plugin shared object file by one means or another,
 * find the lv2_descriptor() function, call it, and proceed from there.
 *
 * Plugin types are accessed by index (not ID) using values from 0
 * upwards. Out of range indexes must result in this function returning
 * NULL, so the plugin count can be determined by checking for the least
 * index that results in NULL being returned.  Index has no meaning,
 * hosts MUST NOT depend on it remaining constant (ie when serialising)
 * in any way. */
const LV2_Descriptor * lv2_descriptor(uint32_t index);


/** Datatype corresponding to the lv2_descriptor() function. */
typedef const LV2_Descriptor * 
(*LV2_Descriptor_Function)(uint32_t index);


/* ******************************************************************** */


/* Put this (LV2_SYMBOL_EXPORT) before any functions that are to be loaded
 * by the host as a symbol from the dynamic library.
 */
#ifdef WIN32
#define LV2_SYMBOL_EXPORT __declspec(dllexport)
#else
#define LV2_SYMBOL_EXPORT
#endif


#ifdef __cplusplus
}
#endif

#endif /* LV2_H_INCLUDED */

