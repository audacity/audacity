/*
  LV2 - An audio plugin interface specification.
  Copyright 2006-2012 Steve Harris, David Robillard.

  Based on LADSPA, Copyright 2000-2002 Richard W.E. Furse,
  Paul Barton-Davis, Stefan Westerfeld.

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

/**
   @file lv2.h
   API for the LV2 specification <http://lv2plug.in/ns/lv2core>.
   Revision: 6.5
*/

#ifndef LV2_H_INCLUDED
#define LV2_H_INCLUDED

#include <stdint.h>

#define LV2_CORE_URI    "http://lv2plug.in/ns/lv2core"
#define LV2_CORE_PREFIX LV2_CORE_URI "#"

#define LV2_CORE__AllpassPlugin      LV2_CORE_PREFIX "AllpassPlugin"
#define LV2_CORE__AmplifierPlugin    LV2_CORE_PREFIX "AmplifierPlugin"
#define LV2_CORE__AnalyserPlugin     LV2_CORE_PREFIX "AnalyserPlugin"
#define LV2_CORE__AudioPort          LV2_CORE_PREFIX "AudioPort"
#define LV2_CORE__BandpassPlugin     LV2_CORE_PREFIX "BandpassPlugin"
#define LV2_CORE__CVPort             LV2_CORE_PREFIX "CVPort"
#define LV2_CORE__ChorusPlugin       LV2_CORE_PREFIX "ChorusPlugin"
#define LV2_CORE__CombPlugin         LV2_CORE_PREFIX "CombPlugin"
#define LV2_CORE__CompressorPlugin   LV2_CORE_PREFIX "CompressorPlugin"
#define LV2_CORE__ConstantPlugin     LV2_CORE_PREFIX "ConstantPlugin"
#define LV2_CORE__ControlPort        LV2_CORE_PREFIX "ControlPort"
#define LV2_CORE__ConverterPlugin    LV2_CORE_PREFIX "ConverterPlugin"
#define LV2_CORE__DelayPlugin        LV2_CORE_PREFIX "DelayPlugin"
#define LV2_CORE__DistortionPlugin   LV2_CORE_PREFIX "DistortionPlugin"
#define LV2_CORE__DynamicsPlugin     LV2_CORE_PREFIX "DynamicsPlugin"
#define LV2_CORE__EQPlugin           LV2_CORE_PREFIX "EQPlugin"
#define LV2_CORE__EnvelopePlugin     LV2_CORE_PREFIX "EnvelopePlugin"
#define LV2_CORE__ExpanderPlugin     LV2_CORE_PREFIX "ExpanderPlugin"
#define LV2_CORE__ExtensionData      LV2_CORE_PREFIX "ExtensionData"
#define LV2_CORE__Feature            LV2_CORE_PREFIX "Feature"
#define LV2_CORE__FilterPlugin       LV2_CORE_PREFIX "FilterPlugin"
#define LV2_CORE__FlangerPlugin      LV2_CORE_PREFIX "FlangerPlugin"
#define LV2_CORE__FunctionPlugin     LV2_CORE_PREFIX "FunctionPlugin"
#define LV2_CORE__GatePlugin         LV2_CORE_PREFIX "GatePlugin"
#define LV2_CORE__GeneratorPlugin    LV2_CORE_PREFIX "GeneratorPlugin"
#define LV2_CORE__HighpassPlugin     LV2_CORE_PREFIX "HighpassPlugin"
#define LV2_CORE__InputPort          LV2_CORE_PREFIX "InputPort"
#define LV2_CORE__InstrumentPlugin   LV2_CORE_PREFIX "InstrumentPlugin"
#define LV2_CORE__LimiterPlugin      LV2_CORE_PREFIX "LimiterPlugin"
#define LV2_CORE__LowpassPlugin      LV2_CORE_PREFIX "LowpassPlugin"
#define LV2_CORE__MixerPlugin        LV2_CORE_PREFIX "MixerPlugin"
#define LV2_CORE__ModulatorPlugin    LV2_CORE_PREFIX "ModulatorPlugin"
#define LV2_CORE__MultiEQPlugin      LV2_CORE_PREFIX "MultiEQPlugin"
#define LV2_CORE__OscillatorPlugin   LV2_CORE_PREFIX "OscillatorPlugin"
#define LV2_CORE__OutputPort         LV2_CORE_PREFIX "OutputPort"
#define LV2_CORE__ParaEQPlugin       LV2_CORE_PREFIX "ParaEQPlugin"
#define LV2_CORE__PhaserPlugin       LV2_CORE_PREFIX "PhaserPlugin"
#define LV2_CORE__PitchPlugin        LV2_CORE_PREFIX "PitchPlugin"
#define LV2_CORE__Plugin             LV2_CORE_PREFIX "Plugin"
#define LV2_CORE__PluginBase         LV2_CORE_PREFIX "PluginBase"
#define LV2_CORE__Point              LV2_CORE_PREFIX "Point"
#define LV2_CORE__Port               LV2_CORE_PREFIX "Port"
#define LV2_CORE__PortProperty       LV2_CORE_PREFIX "PortProperty"
#define LV2_CORE__Resource           LV2_CORE_PREFIX "Resource"
#define LV2_CORE__ReverbPlugin       LV2_CORE_PREFIX "ReverbPlugin"
#define LV2_CORE__ScalePoint         LV2_CORE_PREFIX "ScalePoint"
#define LV2_CORE__SimulatorPlugin    LV2_CORE_PREFIX "SimulatorPlugin"
#define LV2_CORE__SpatialPlugin      LV2_CORE_PREFIX "SpatialPlugin"
#define LV2_CORE__Specification      LV2_CORE_PREFIX "Specification"
#define LV2_CORE__SpectralPlugin     LV2_CORE_PREFIX "SpectralPlugin"
#define LV2_CORE__UtilityPlugin      LV2_CORE_PREFIX "UtilityPlugin"
#define LV2_CORE__WaveshaperPlugin   LV2_CORE_PREFIX "WaveshaperPlugin"
#define LV2_CORE__appliesTo          LV2_CORE_PREFIX "appliesTo"
#define LV2_CORE__binary             LV2_CORE_PREFIX "binary"
#define LV2_CORE__connectionOptional LV2_CORE_PREFIX "connectionOptional"
#define LV2_CORE__control            LV2_CORE_PREFIX "control"
#define LV2_CORE__default            LV2_CORE_PREFIX "default"
#define LV2_CORE__designation        LV2_CORE_PREFIX "designation"
#define LV2_CORE__documentation      LV2_CORE_PREFIX "documentation"
#define LV2_CORE__enumeration        LV2_CORE_PREFIX "enumeration"
#define LV2_CORE__extensionData      LV2_CORE_PREFIX "extensionData"
#define LV2_CORE__freeWheeling       LV2_CORE_PREFIX "freeWheeling"
#define LV2_CORE__hardRTCapable      LV2_CORE_PREFIX "hardRTCapable"
#define LV2_CORE__inPlaceBroken      LV2_CORE_PREFIX "inPlaceBroken"
#define LV2_CORE__index              LV2_CORE_PREFIX "index"
#define LV2_CORE__integer            LV2_CORE_PREFIX "integer"
#define LV2_CORE__isLive             LV2_CORE_PREFIX "isLive"
#define LV2_CORE__latency            LV2_CORE_PREFIX "latency"
#define LV2_CORE__maximum            LV2_CORE_PREFIX "maximum"
#define LV2_CORE__microVersion       LV2_CORE_PREFIX "microVersion"
#define LV2_CORE__minimum            LV2_CORE_PREFIX "minimum"
#define LV2_CORE__minorVersion       LV2_CORE_PREFIX "minorVersion"
#define LV2_CORE__name               LV2_CORE_PREFIX "name"
#define LV2_CORE__optionalFeature    LV2_CORE_PREFIX "optionalFeature"
#define LV2_CORE__port               LV2_CORE_PREFIX "port"
#define LV2_CORE__portProperty       LV2_CORE_PREFIX "portProperty"
#define LV2_CORE__project            LV2_CORE_PREFIX "project"
#define LV2_CORE__reportsLatency     LV2_CORE_PREFIX "reportsLatency"
#define LV2_CORE__requiredFeature    LV2_CORE_PREFIX "requiredFeature"
#define LV2_CORE__sampleRate         LV2_CORE_PREFIX "sampleRate"
#define LV2_CORE__scalePoint         LV2_CORE_PREFIX "scalePoint"
#define LV2_CORE__symbol             LV2_CORE_PREFIX "symbol"
#define LV2_CORE__toggled            LV2_CORE_PREFIX "toggled"

#ifdef __cplusplus
extern "C" {
#endif

/**
   Plugin Instance Handle.

   This is a handle for one particular instance of a plugin.  It is valid to
   compare to NULL (or 0 for C++) but otherwise the host MUST NOT attempt to
   interpret it.
*/
typedef void * LV2_Handle;

/**
   Feature.

   Features allow hosts to make additional functionality available to plugins
   without requiring modification to the LV2 API.  Extensions may define new
   features and specify the @ref URI and @ref data to be used if necessary.
   Some features, such as lv2:isLive, do not require the host to pass data.
*/
typedef struct _LV2_Feature {
	/**
	   A globally unique, case-sensitive identifier (URI) for this feature.

	   This MUST be a valid URI string as defined by RFC 3986.
	*/
	const char * URI;

	/**
	   Pointer to arbitrary data.

	   The format of this data is defined by the extension which describes the
	   feature with the given @ref URI.
	*/
	void * data;
} LV2_Feature;

/**
   Plugin Descriptor.

   This structure provides the core functions necessary to instantiate and use
   a plugin.
*/
typedef struct _LV2_Descriptor {
	/**
	   A globally unique, case-sensitive identifier for this plugin.

	   This MUST be a valid URI string as defined by RFC 3986.  All plugins with
	   the same URI MUST be compatible to some degree, see
	   http://lv2plug.in/ns/lv2core for details.
	*/
	const char * URI;

	/**
	   Instantiate the plugin.

	   Note that instance initialisation should generally occur in activate()
	   rather than here. If a host calls instantiate(), it MUST call cleanup()
	   at some point in the future.

	   @param descriptor Descriptor of the plugin to instantiate.

	   @param sample_rate Sample rate, in Hz, for the new plugin instance.

	   @param bundle_path Path to the LV2 bundle which contains this plugin
	   binary. It MUST include the trailing directory separator (e.g. '/') so
	   that simply appending a filename will yield the path to that file in the
	   bundle.

	   @param features A NULL terminated array of LV2_Feature structs which
	   represent the features the host supports. Plugins may refuse to
	   instantiate if required features are not found here. However, hosts MUST
	   NOT use this as a discovery mechanism: instead, use the RDF data to
	   determine which features are required and do not attempt to instantiate
	   unsupported plugins at all. This parameter MUST NOT be NULL, i.e. a host
	   that supports no features MUST pass a single element array containing
	   NULL.

	   @return A handle for the new plugin instance, or NULL if instantiation
	   has failed.
	*/
	LV2_Handle (*instantiate)(const struct _LV2_Descriptor * descriptor,
	                          double                         sample_rate,
	                          const char *                   bundle_path,
	                          const LV2_Feature *const *     features);

	/**
	   Connect a port on a plugin instance to a memory location.

	   Plugin writers should be aware that the host may elect to use the same
	   buffer for more than one port and even use the same buffer for both
	   input and output (see lv2:inPlaceBroken in lv2.ttl).

	   If the plugin has the feature lv2:hardRTCapable then there are various
	   things that the plugin MUST NOT do within the connect_port() function;
	   see lv2core.ttl for details.

	   connect_port() MUST be called at least once for each port before run()
	   is called, unless that port is lv2:connectionOptional. The plugin must
	   pay careful attention to the block size passed to run() since the block
	   allocated may only just be large enough to contain the data, and is not
	   guaranteed to remain constant between run() calls.

	   connect_port() may be called more than once for a plugin instance to
	   allow the host to change the buffers that the plugin is reading or
	   writing. These calls may be made before or after activate() or
	   deactivate() calls.

	   @param instance Plugin instance containing the port.

	   @param port Index of the port to connect. The host MUST NOT try to
	   connect a port index that is not defined in the plugin's RDF data. If
	   it does, the plugin's behaviour is undefined (a crash is likely).

	   @param data_location Pointer to data of the type defined by the port
	   type in the plugin's RDF data (e.g. an array of float for an
	   lv2:AudioPort). This pointer must be stored by the plugin instance and
	   used to read/write data when run() is called. Data present at the time
	   of the connect_port() call MUST NOT be considered meaningful.
	*/
	void (*connect_port)(LV2_Handle instance,
	                     uint32_t   port,
	                     void *     data_location);

	/**
	   Initialise a plugin instance and activate it for use.

	   This is separated from instantiate() to aid real-time support and so
	   that hosts can reinitialise a plugin instance by calling deactivate()
	   and then activate(). In this case the plugin instance MUST reset all
	   state information dependent on the history of the plugin instance except
	   for any data locations provided by connect_port(). If there is nothing
	   for activate() to do then this field may be NULL.

	   When present, hosts MUST call this function once before run() is called
	   for the first time. This call SHOULD be made as close to the run() call
	   as possible and indicates to real-time plugins that they are now live,
	   however plugins MUST NOT rely on a prompt call to run() after
	   activate().

	   The host MUST NOT call activate() again until deactivate() has been
	   called first. If a host calls activate(), it MUST call deactivate() at
	   some point in the future. Note that connect_port() may be called before
	   or after activate().
	*/
	void (*activate)(LV2_Handle instance);

	/**
	   Run a plugin instance for a block.

	   Note that if an activate() function exists then it must be called before
	   run(). If deactivate() is called for a plugin instance then run() may
	   not be called until activate() has been called again.

	   If the plugin has the feature lv2:hardRTCapable then there are various
	   things that the plugin MUST NOT do within the run() function (see
	   lv2core.ttl for details).

	   As a special case, when @p sample_count == 0, the plugin should update
	   any output ports that represent a single instant in time (e.g. control
	   ports, but not audio ports). This is particularly useful for latent
	   plugins, which should update their latency output port so hosts can
	   pre-roll plugins to compute latency. Plugins MUST NOT crash when
	   @p sample_count == 0.

	   @param instance Instance to be run.

	   @param sample_count The block size (in samples) for which the plugin
	   instance must run.
	*/
	void (*run)(LV2_Handle instance,
	            uint32_t   sample_count);

	/**
	   Deactivate a plugin instance (counterpart to activate()).

	   Hosts MUST deactivate all activated instances after they have been run()
	   for the last time. This call SHOULD be made as close to the last run()
	   call as possible and indicates to real-time plugins that they are no
	   longer live, however plugins MUST NOT rely on prompt deactivation. If
	   there is nothing for deactivate() to do then this field may be NULL

	   Deactivation is not similar to pausing since the plugin instance will be
	   reinitialised by activate(). However, deactivate() itself MUST NOT fully
	   reset plugin state. For example, the host may deactivate a plugin, then
	   store its state (using some extension to do so).

	   Hosts MUST NOT call deactivate() unless activate() was previously
	   called. Note that connect_port() may be called before or after
	   deactivate().
	*/
	void (*deactivate)(LV2_Handle instance);

	/**
	   Clean up a plugin instance (counterpart to instantiate()).

	   Once an instance of a plugin has been finished with it must be deleted
	   using this function. The instance handle passed ceases to be valid after
	   this call.

	   If activate() was called for a plugin instance then a corresponding call
	   to deactivate() MUST be made before cleanup() is called. Hosts MUST NOT
	   call cleanup() unless instantiate() was previously called.
	*/
	void (*cleanup)(LV2_Handle instance);

	/**
	   Return additional plugin data defined by some extenion.

	   A typical use of this facility is to return a struct containing function
	   pointers to extend the LV2_Descriptor API.

	   The actual type and meaning of the returned object MUST be specified
	   precisely by the extension. This function MUST return NULL for any
	   unsupported URI. If a plugin does not support any extension data, this
	   field may be NULL.

	   The host is never responsible for freeing the returned value.
	*/
	const void * (*extension_data)(const char * uri);
} LV2_Descriptor;

/**
   Put this (LV2_SYMBOL_EXPORT) before any functions that are to be loaded
   by the host as a symbol from the dynamic library.
*/
#ifdef _WIN32
#    define LV2_SYMBOL_EXPORT __declspec(dllexport)
#else
#    define LV2_SYMBOL_EXPORT
#endif

/**
   Prototype for plugin accessor function.

   This is part of the old discovery API, which has been replaced due to being
   inadequate for some plugins.  It is limited because the bundle path is not
   available during discovery, and it relies on non-portable shared library
   constructors/destructors.  However, this API is still supported and plugins
   are not required to migrate.

   Plugins are discovered by hosts using RDF data (not by loading libraries).
   See http://lv2plug.in for details on the discovery process, though most
   hosts should use an existing library to implement this functionality.

   A plugin library MUST include a function called "lv2_descriptor" with this
   prototype.  This function MUST have C-style linkage (if you are using C++
   this is taken care of by the 'extern "C"' clause at the top of this file).

   When it is time to load a plugin (designated by its URI), the host loads the
   plugin's library, gets the lv2_descriptor() function from it, and uses this
   function to find the LV2_Descriptor for the desired plugin.  Plugins are
   accessed by index using values from 0 upwards.  This function MUST return
   NULL for out of range indices, so the host can enumerate plugins by
   increasing @p index until NULL is returned.

   Note that @p index has no meaning, hosts MUST NOT depend on it remaining
   consistent between loads of the plugin library.
*/
LV2_SYMBOL_EXPORT
const LV2_Descriptor * lv2_descriptor(uint32_t index);

/**
   Type of the lv2_descriptor() function in a library (old discovery API).
*/
typedef const LV2_Descriptor *
(*LV2_Descriptor_Function)(uint32_t index);

/**
   Handle for a library descriptor.
*/
typedef void* LV2_Lib_Handle;

/**
   Descriptor for a plugin library.

   To access a plugin library, the host creates an LV2_Lib_Descriptor via the
   lv2_lib_descriptor() function in the shared object.
*/
typedef struct {
	/**
	   Opaque library data which must be passed as the first parameter to all
	   the methods of this struct.
	*/
	LV2_Lib_Handle handle;

	/**
	   The total size of this struct.  This allows for this struct to be
	   expanded in the future if necessary.  This MUST be set by the library to
	   sizeof(LV2_Lib_Descriptor).  The host MUST NOT access any fields of this
	   struct beyond get_plugin() unless this field indicates they are present.
	*/
	uint32_t size;

	/**
	   Destroy this library descriptor and free all related resources.
	*/
	void (*cleanup)(LV2_Lib_Handle handle);

	/**
	   Plugin accessor.

	   Plugins are accessed by index using values from 0 upwards.  Out of range
	   indices MUST result in this function returning NULL, so the host can
	   enumerate plugins by increasing @a index until NULL is returned.
	*/
	const LV2_Descriptor * (*get_plugin)(LV2_Lib_Handle handle,
	                                     uint32_t       index);
} LV2_Lib_Descriptor;

/**
   Prototype for library accessor function.

   This is the entry point for a plugin library.  Hosts load this symbol from
   the library and call this function to obtain a library descriptor which can
   be used to access all the contained plugins.  The returned object must not
   be destroyed (using LV2_Lib_Descriptor::cleanup()) until all plugins loaded
   from that library have been destroyed.
*/
const LV2_Lib_Descriptor *
lv2_lib_descriptor(const char *               bundle_path,
                   const LV2_Feature *const * features);

/**
   Type of the lv2_lib_descriptor() function in an LV2 library.
*/
typedef const LV2_Lib_Descriptor *
(*LV2_Lib_Descriptor_Function)(const char *               bundle_path,
                               const LV2_Feature *const * features);

#ifdef __cplusplus
}
#endif

#endif /* LV2_H_INCLUDED */
