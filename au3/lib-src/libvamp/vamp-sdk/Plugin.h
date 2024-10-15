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

#ifndef _VAMP_SDK_PLUGIN_H_
#define _VAMP_SDK_PLUGIN_H_

#include <string>
#include <vector>
#include <map>

#include "PluginBase.h"
#include "RealTime.h"

#include "plugguard.h"
_VAMP_SDK_PLUGSPACE_BEGIN(Plugin.h)

namespace Vamp {

/**
 * \class Plugin Plugin.h <vamp-sdk/Plugin.h>
 * 
 * Vamp::Plugin is a base class for plugin instance classes
 * that provide feature extraction from audio or related data.
 *
 * In most cases, the input will be audio and the output will be a
 * stream of derived data at a lower sampling resolution than the
 * input.
 *
 * Note that this class inherits several abstract methods from
 * PluginBase.  These must be implemented by the subclass.
 * 
 * 
 * PLUGIN LIFECYCLE
 *
 * Feature extraction plugins are managed differently from real-time
 * plugins (such as VST effects).  The main difference is that the
 * parameters for a feature extraction plugin are configured before
 * the plugin is used, and do not change during use.
 *
 * 1. Host constructs the plugin, passing it the input sample rate.
 * The plugin may do basic initialisation, but should not do anything
 * computationally expensive at this point.  You must make sure your
 * plugin is cheap to construct, otherwise you'll seriously affect the
 * startup performance of almost all hosts.  If you have serious
 * initialisation to do, the proper place is in initialise() (step 5).
 *
 * 2. Host may query the plugin's available outputs.
 *
 * 3. Host queries programs and parameter descriptors, and may set
 * some or all of them.  Parameters that are not explicitly set should
 * take their default values as specified in the parameter descriptor.
 * When a program is set, the parameter values may change and the host
 * will re-query them to check.
 *
 * 4. Host queries the preferred step size, block size and number of
 * channels.  These may all vary depending on the parameter values.
 * (Note however that you cannot make the number of distinct outputs
 * dependent on parameter values.)
 *
 * 5. Plugin is properly initialised with a call to initialise.  This
 * fixes the step size, block size, and number of channels, as well as
 * all of the parameter and program settings.  If the values passed in
 * to initialise do not match the plugin's advertised preferred values
 * from step 4, the plugin may refuse to initialise and return false
 * (although if possible it should accept the new values).  Any
 * computationally expensive setup code should take place here.
 *
 * 6. Host finally checks the number of values, resolution, extents
 * etc per output (which may vary depending on the number of channels,
 * step size and block size as well as the parameter values).
 *
 * 7. Host will repeatedly call the process method to pass in blocks
 * of input data.  This method may return features extracted from that
 * data (if the plugin is causal).
 *
 * 8. Host will call getRemainingFeatures exactly once, after all the
 * input data has been processed.  This may return any non-causal or
 * leftover features.
 *
 * 9. At any point after initialise was called, the host may
 * optionally call the reset method and restart processing.  (This
 * does not mean it can change the parameters, which are fixed from
 * initialise until destruction.)
 *
 * A plugin does not need to handle the case where setParameter or
 * selectProgram is called after initialise has been called.  It's the
 * host's responsibility not to do that.  Similarly, the plugin may
 * safely assume that initialise is called no more than once.
 */

class Plugin : public PluginBase
{
public:
    virtual ~Plugin() { }

    /**
     * Initialise a plugin to prepare it for use with the given number
     * of input channels, step size (window increment, in sample
     * frames) and block size (window size, in sample frames).
     *
     * The input sample rate should have been already specified at
     * construction time.
     * 
     * Return true for successful initialisation, false if the number
     * of input channels, step size and/or block size cannot be
     * supported.
     */
    virtual bool initialise(size_t inputChannels,
			    size_t stepSize,
			    size_t blockSize) = 0;

    /**
     * Reset the plugin after use, to prepare it for another clean
     * run.  Not called for the first initialisation (i.e. initialise
     * must also do a reset).
     */
    virtual void reset() = 0;

    enum InputDomain { TimeDomain, FrequencyDomain };
    
    /**
     * Get the plugin's required input domain.
     *
     * If this is TimeDomain, the samples provided to the process()
     * function (below) will be in the time domain, as for a
     * traditional audio processing plugin.
     *
     * If this is FrequencyDomain, the host will carry out a windowed
     * FFT of size equal to the negotiated block size on the data
     * before passing the frequency bin data in to process().  The
     * input data for the FFT will be rotated so as to place the
     * origin in the centre of the block.
     * The plugin does not get to choose the window type -- the host
     * will either let the user do so, or will use a Hanning window.
     */
    virtual InputDomain getInputDomain() const = 0;

    /**
     * Get the preferred block size (window size -- the number of
     * sample frames passed in each block to the process() function).
     * This should be called before initialise().
     *
     * A plugin that can handle any block size may return 0.  The
     * final block size will be set in the initialise() call.
     */
    virtual size_t getPreferredBlockSize() const { return 0; }

    /**
     * Get the preferred step size (window increment -- the distance
     * in sample frames between the start frames of consecutive blocks
     * passed to the process() function) for the plugin.  This should
     * be called before initialise().
     *
     * A plugin may return 0 if it has no particular interest in the
     * step size.  In this case, the host should make the step size
     * equal to the block size if the plugin is accepting input in the
     * time domain.  If the plugin is accepting input in the frequency
     * domain, the host may use any step size.  The final step size
     * will be set in the initialise() call.
     */
    virtual size_t getPreferredStepSize() const { return 0; }

    /**
     * Get the minimum supported number of input channels.
     */
    virtual size_t getMinChannelCount() const { return 1; }

    /**
     * Get the maximum supported number of input channels.
     */
    virtual size_t getMaxChannelCount() const { return 1; }

    struct OutputDescriptor
    {
	/**
	 * The name of the output, in computer-usable form.  Should be
	 * reasonably short and without whitespace or punctuation, using
         * the characters [a-zA-Z0-9_-] only.
         * Example: "zero_crossing_count"
	 */
	std::string identifier;

	/**
	 * The human-readable name of the output.
         * Example: "Zero Crossing Counts"
	 */
	std::string name;

	/**
	 * A human-readable short text describing the output.  May be
         * empty if the name has said it all already.
         * Example: "The number of zero crossing points per processing block"
	 */
	std::string description;

	/**
	 * The unit of the output, in human-readable form.
	 */
	std::string unit;

	/**
	 * True if the output has the same number of values per sample
	 * for every output sample.  Outputs for which this is false
	 * are unlikely to be very useful in a general-purpose host.
	 */
	bool hasFixedBinCount;

	/**
	 * The number of values per result of the output.  Undefined
	 * if hasFixedBinCount is false.  If this is zero, the output
	 * is point data (i.e. only the time of each output is of
	 * interest, the value list will be empty).
	 */
	size_t binCount;

	/**
	 * The (human-readable) names of each of the bins, if
	 * appropriate.  This is always optional.
	 */
	std::vector<std::string> binNames;

	/**
	 * True if the results in each output bin fall within a fixed
	 * numeric range (minimum and maximum values).  Undefined if
	 * binCount is zero.
	 */
	bool hasKnownExtents;

	/**
	 * Minimum value of the results in the output.  Undefined if
	 * hasKnownExtents is false or binCount is zero.
	 */
	float minValue;

	/**
	 * Maximum value of the results in the output.  Undefined if
	 * hasKnownExtents is false or binCount is zero.
	 */
	float maxValue;

	/**
	 * True if the output values are quantized to a particular
	 * resolution.  Undefined if binCount is zero.
	 */
	bool isQuantized;

	/**
	 * Quantization resolution of the output values (e.g. 1.0 if
	 * they are all integers).  Undefined if isQuantized is false
	 * or binCount is zero.
	 */
	float quantizeStep;

	enum SampleType {

	    /// Results from each process() align with that call's block start
	    OneSamplePerStep,

	    /// Results are evenly spaced in time (sampleRate specified below)
	    FixedSampleRate,

	    /// Results are unevenly spaced and have individual timestamps
	    VariableSampleRate
	};

	/**
	 * Positioning in time of the output results.
	 */
	SampleType sampleType;

	/**
	 * Sample rate of the output results, as samples per second.
	 * Undefined if sampleType is OneSamplePerStep.
	 *
	 * If sampleType is VariableSampleRate and this value is
	 * non-zero, then it may be used to calculate a resolution for
	 * the output (i.e. the "duration" of each sample, in time,
	 * will be 1/sampleRate seconds).  It's recommended to set
	 * this to zero if that behaviour is not desired.
	 */
	float sampleRate;

        /**
         * True if the returned results for this output are known to
         * have a duration field.
         */
        bool hasDuration;

        OutputDescriptor() : // defaults for mandatory non-class-type members
            hasFixedBinCount(false), hasKnownExtents(false), isQuantized(false),
            sampleType(OneSamplePerStep), sampleRate(0), hasDuration(false) { }
    };

    typedef std::vector<OutputDescriptor> OutputList;

    /**
     * Get the outputs of this plugin.  An output's index in this list
     * is used as its numeric index when looking it up in the
     * FeatureSet returned from the process() call.
     */
    virtual OutputList getOutputDescriptors() const = 0;

    struct Feature
    {
	/**
	 * True if an output feature has its own timestamp.  This is
	 * mandatory if the output has VariableSampleRate, optional if
	 * the output has FixedSampleRate, and unused if the output
	 * has OneSamplePerStep.
	 */
	bool hasTimestamp;

	/**
	 * Timestamp of the output feature.  This is mandatory if the
	 * output has VariableSampleRate or if the output has
	 * FixedSampleRate and hasTimestamp is true, and unused
	 * otherwise.
	 */
	RealTime timestamp;

        /**
         * True if an output feature has a specified duration.  This
         * is optional if the output has VariableSampleRate or
         * FixedSampleRate, and and unused if the output has
         * OneSamplePerStep.
         */
        bool hasDuration;

        /**
         * Duration of the output feature.  This is mandatory if the
         * output has VariableSampleRate or FixedSampleRate and
         * hasDuration is true, and unused otherwise.
         */
        RealTime duration;
	
	/**
	 * Results for a single sample of this feature.  If the output
	 * hasFixedBinCount, there must be the same number of values
	 * as the output's binCount count.
	 */
	std::vector<float> values;

	/**
	 * Label for the sample of this feature.
	 */
	std::string label;

        Feature() : // defaults for mandatory non-class-type members
            hasTimestamp(false), hasDuration(false) { }
    };

    typedef std::vector<Feature> FeatureList;

    typedef std::map<int, FeatureList> FeatureSet; // key is output no

    /**
     * Process a single block of input data.
     * 
     * If the plugin's inputDomain is TimeDomain, inputBuffers will
     * point to one array of floats per input channel, and each of
     * these arrays will contain blockSize consecutive audio samples
     * (the host will zero-pad as necessary).  The timestamp in this
     * case will be the real time in seconds of the start of the
     * supplied block of samples.
     *
     * If the plugin's inputDomain is FrequencyDomain, inputBuffers
     * will point to one array of floats per input channel, and each
     * of these arrays will contain blockSize/2+1 consecutive pairs of
     * real and imaginary component floats corresponding to bins
     * 0..(blockSize/2) of the FFT output.  That is, bin 0 (the first
     * pair of floats) contains the DC output, up to bin blockSize/2
     * which contains the Nyquist-frequency output.  There will
     * therefore be blockSize+2 floats per channel in total.  The
     * timestamp will be the real time in seconds of the centre of the
     * FFT input window (i.e. the very first block passed to process
     * might contain the FFT of half a block of zero samples and the
     * first half-block of the actual data, with a timestamp of zero).
     *
     * Return any features that have become available after this
     * process call.  (These do not necessarily have to fall within
     * the process block, except for OneSamplePerStep outputs.)
     */
    virtual FeatureSet process(const float *const *inputBuffers,
			       RealTime timestamp) = 0;

    /**
     * After all blocks have been processed, calculate and return any
     * remaining features derived from the complete input.
     */
    virtual FeatureSet getRemainingFeatures() = 0;

    /**
     * Used to distinguish between Vamp::Plugin and other potential
     * sibling subclasses of PluginBase.  Do not reimplement this
     * function in your subclass.
     */
    virtual std::string getType() const { return "Feature Extraction Plugin"; }

protected:
    Plugin(float inputSampleRate) :
	m_inputSampleRate(inputSampleRate) { }

    float m_inputSampleRate;
};

}

_VAMP_SDK_PLUGSPACE_END(Plugin.h)

#endif



