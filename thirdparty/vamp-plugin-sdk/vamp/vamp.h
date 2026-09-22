/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    Copyright 2006 Chris Cannam.
  
    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR
    ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
    CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

    Except as contained in this notice, the names of the Centre for
    Digital Music; Queen Mary, University of London; and Chris Cannam
    shall not be used in advertising or otherwise to promote the sale,
    use or other dealings in this Software without prior written
    authorization.
*/

#ifndef VAMP_HEADER_INCLUDED
#define VAMP_HEADER_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

/** 
 * Plugin API version.  This is incremented when a change is made that
 * changes the binary layout of the descriptor records.  When this
 * happens, there should be a mechanism for retaining compatibility
 * with older hosts and/or plugins.
 *
 * See also the vampApiVersion field in the plugin descriptor, and the
 * hostApiVersion argument to the vampGetPluginDescriptor function.
 */
#define VAMP_API_VERSION 2

/**
 * C language API for Vamp plugins.
 * 
 * This is the formal plugin API for Vamp.  Plugin authors may prefer
 * to use the C++ classes provided in the Vamp plugin SDK, instead of
 * using this API directly.  There is an adapter class provided that
 * makes C++ plugins available using this C API with relatively little
 * work, and the C++ headers are more thoroughly documented.
 *
 * IMPORTANT: The comments in this file summarise the purpose of each
 * of the declared fields and functions, but do not provide a complete
 * guide to their permitted values and expected usage.  Please refer
 * to the C++ headers in the Vamp plugin SDK for further details and
 * plugin lifecycle documentation.
 */

typedef struct _VampParameterDescriptor
{
    /** Computer-usable name of the parameter. Must not change. [a-zA-Z0-9_-] */
    const char *identifier;

    /** Human-readable name of the parameter. May be translatable. */
    const char *name;

    /** Human-readable short text about the parameter.  May be translatable. */
    const char *description;

    /** Human-readable unit of the parameter. */
    const char *unit;

    /** Minimum value. */
    float minValue;

    /** Maximum value. */
    float maxValue;

    /** Default value. Plugin is responsible for setting this on initialise. */
    float defaultValue;

    /** 1 if parameter values are quantized to a particular resolution. */
    int isQuantized;

    /** Quantization resolution, if isQuantized. */
    float quantizeStep;

    /** Human-readable names of the values, if isQuantized.  May be NULL. */
    const char **valueNames;

} VampParameterDescriptor;

typedef enum
{
    /** Each process call returns results aligned with call's block start. */
    vampOneSamplePerStep,

    /** Returned results are evenly spaced at samplerate specified below. */
    vampFixedSampleRate,

    /** Returned results have their own individual timestamps. */
    vampVariableSampleRate

} VampSampleType;

typedef struct _VampOutputDescriptor
{
    /** Computer-usable name of the output. Must not change. [a-zA-Z0-9_-] */
    const char *identifier;

    /** Human-readable name of the output. May be translatable. */
    const char *name;

    /** Human-readable short text about the output. May be translatable. */
    const char *description;

    /** Human-readable name of the unit of the output. */
    const char *unit;

    /** 1 if output has equal number of values for each returned result. */
    int hasFixedBinCount;

    /** Number of values per result, if hasFixedBinCount. */
    unsigned int binCount;

    /** Names of returned value bins, if hasFixedBinCount.  May be NULL. */
    const char **binNames;

    /** 1 if each returned value falls within the same fixed min/max range. */
    int hasKnownExtents;
    
    /** Minimum value for a returned result in any bin, if hasKnownExtents. */
    float minValue;

    /** Maximum value for a returned result in any bin, if hasKnownExtents. */
    float maxValue;

    /** 1 if returned results are quantized to a particular resolution. */
    int isQuantized;

    /** Quantization resolution for returned results, if isQuantized. */
    float quantizeStep;

    /** Time positioning method for returned results (see VampSampleType). */
    VampSampleType sampleType;

    /** Sample rate of returned results, if sampleType is vampFixedSampleRate.
       "Resolution" of result, if sampleType is vampVariableSampleRate. */
    float sampleRate;

    /** 1 if the returned results for this output are known to have a
        duration field.

        This field is new in Vamp API version 2; it must not be tested
        for plugins that report an older API version in their plugin
        descriptor.
    */
    int hasDuration;

} VampOutputDescriptor;

typedef struct _VampFeature
{
    /** 1 if the feature has a timestamp (i.e. if vampVariableSampleRate). */
    int hasTimestamp;

    /** Seconds component of timestamp. */
    int sec;

    /** Nanoseconds component of timestamp. */
    int nsec;

    /** Number of values.  Must be binCount if hasFixedBinCount. */
    unsigned int valueCount;

    /** Values for this returned sample. */
    float *values;

    /** Label for this returned sample.  May be NULL. */
    char *label;

} VampFeature;

typedef struct _VampFeatureV2
{
    /** 1 if the feature has a duration. */
    int hasDuration;

    /** Seconds component of duratiion. */
    int durationSec;

    /** Nanoseconds component of duration. */
    int durationNsec;

} VampFeatureV2;

typedef union _VampFeatureUnion
{
    // sizeof(featureV1) >= sizeof(featureV2) for backward compatibility
    VampFeature   v1;
    VampFeatureV2 v2;

} VampFeatureUnion;

typedef struct _VampFeatureList
{
    /** Number of features in this feature list. */
    unsigned int featureCount;

    /** Features in this feature list.  May be NULL if featureCount is
        zero.

        If present, this array must contain featureCount feature
        structures for a Vamp API version 1 plugin, or 2*featureCount
        feature unions for a Vamp API version 2 plugin.

        The features returned by an API version 2 plugin must consist
        of the same feature structures as in API version 1 for the
        first featureCount array elements, followed by featureCount
        unions that contain VampFeatureV2 structures (or NULL pointers
        if no V2 feature structures are present).
     */
    VampFeatureUnion *features;

} VampFeatureList;

typedef enum
{
    vampTimeDomain,
    vampFrequencyDomain

} VampInputDomain;

typedef void *VampPluginHandle;

typedef struct _VampPluginDescriptor
{
    /** API version with which this descriptor is compatible. */
    unsigned int vampApiVersion;

    /** Computer-usable name of the plugin. Must not change. [a-zA-Z0-9_-] */
    const char *identifier;

    /** Human-readable name of the plugin. May be translatable. */
    const char *name;

    /** Human-readable short text about the plugin. May be translatable. */
    const char *description;

    /** Human-readable name of plugin's author or vendor. */
    const char *maker;

    /** Version number of the plugin. */
    int pluginVersion;

    /** Human-readable summary of copyright or licensing for plugin. */
    const char *copyright;

    /** Number of parameter inputs. */
    unsigned int parameterCount;

    /** Fixed descriptors for parameter inputs. */
    const VampParameterDescriptor **parameters;

    /** Number of programs. */
    unsigned int programCount;

    /** Fixed names for programs. */
    const char **programs;

    /** Preferred input domain for audio input (time or frequency). */
    VampInputDomain inputDomain;

    /** Create and return a new instance of this plugin. */
    VampPluginHandle (*instantiate)(const struct _VampPluginDescriptor *,
                                   float inputSampleRate);

    /** Destroy an instance of this plugin. */
    void (*cleanup)(VampPluginHandle);

    /** Initialise an instance following parameter configuration. */
    int (*initialise)(VampPluginHandle,
                      unsigned int inputChannels,
                      unsigned int stepSize, 
                      unsigned int blockSize);

    /** Reset an instance, ready to use again on new input data. */
    void (*reset)(VampPluginHandle);

    /** Get a parameter value. */
    float (*getParameter)(VampPluginHandle, int);

    /** Set a parameter value. May only be called before initialise. */
    void  (*setParameter)(VampPluginHandle, int, float);

    /** Get the current program (if programCount > 0). */
    unsigned int (*getCurrentProgram)(VampPluginHandle);

    /** Set the current program. May only be called before initialise. */
    void  (*selectProgram)(VampPluginHandle, unsigned int);
    
    /** Get the plugin's preferred processing window increment in samples. */
    unsigned int (*getPreferredStepSize)(VampPluginHandle);

    /** Get the plugin's preferred processing window size in samples. */
    unsigned int (*getPreferredBlockSize)(VampPluginHandle);

    /** Get the minimum number of input channels this plugin can handle. */
    unsigned int (*getMinChannelCount)(VampPluginHandle);

    /** Get the maximum number of input channels this plugin can handle. */
    unsigned int (*getMaxChannelCount)(VampPluginHandle);

    /** Get the number of feature outputs (distinct sets of results). */
    unsigned int (*getOutputCount)(VampPluginHandle);

    /** Get a descriptor for a given feature output. Returned pointer
        is valid only until next call to getOutputDescriptor for this
        handle, or releaseOutputDescriptor for this descriptor. Host
        must call releaseOutputDescriptor after use. */
    VampOutputDescriptor *(*getOutputDescriptor)(VampPluginHandle,
                                                 unsigned int);

    /** Destroy a descriptor for a feature output. */
    void (*releaseOutputDescriptor)(VampOutputDescriptor *);

    /** Process an input block and return a set of features. Returned
        pointer is valid only until next call to process,
        getRemainingFeatures, or cleanup for this handle, or
        releaseFeatureSet for this feature set. Host must call
        releaseFeatureSet after use. */
    VampFeatureList *(*process)(VampPluginHandle,
                                const float *const *inputBuffers,
                                int sec,
                                int nsec);

    /** Return any remaining features at the end of processing. */
    VampFeatureList *(*getRemainingFeatures)(VampPluginHandle);

    /** Release a feature set returned from process or getRemainingFeatures. */
    void (*releaseFeatureSet)(VampFeatureList *);

} VampPluginDescriptor;


/** Get the descriptor for a given plugin index in this library.
    Return NULL if the index is outside the range of valid indices for
    this plugin library.

    The hostApiVersion argument tells the library code the highest
    Vamp API version supported by the host.  The function should
    return a plugin descriptor compatible with the highest API version
    supported by the library that is no higher than that supported by
    the host.  Provided the descriptor has the correct vampApiVersion
    field for its actual compatibility level, the host should be able
    to do the right thing with it: use it if possible, discard it
    otherwise.

    This is the only symbol that a Vamp plugin actually needs to
    export from its shared object; all others can be hidden.  See the
    accompanying documentation for notes on how to achieve this with
    certain compilers.
*/
const VampPluginDescriptor *vampGetPluginDescriptor
    (unsigned int hostApiVersion, unsigned int index);


/** Function pointer type for vampGetPluginDescriptor. */
typedef const VampPluginDescriptor *(*VampGetPluginDescriptorFunction)
    (unsigned int, unsigned int);

#ifdef __cplusplus
}
#endif

#endif
