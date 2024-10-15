/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    Copyright 2006-2009 Chris Cannam and QMUL.
    This file by Mark Levy and Chris Cannam, Copyright 2007-2008 QMUL.
  
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

#ifndef _VAMP_PLUGIN_BUFFERING_ADAPTER_H_
#define _VAMP_PLUGIN_BUFFERING_ADAPTER_H_

#include "hostguard.h"
#include "PluginWrapper.h"

_VAMP_SDK_HOSTSPACE_BEGIN(PluginBufferingAdapter.h)

namespace Vamp {
	
namespace HostExt {
		
/**
 * \class PluginBufferingAdapter PluginBufferingAdapter.h <vamp-hostsdk/PluginBufferingAdapter.h>
 *
 * PluginBufferingAdapter is a Vamp plugin adapter that allows plugins
 * to be used by a host supplying an audio stream in non-overlapping
 * buffers of arbitrary size.
 *
 * A host using PluginBufferingAdapter may ignore the preferred step
 * and block size reported by the plugin, and still expect the plugin
 * to run.  The value of blockSize and stepSize passed to initialise
 * should be the size of the buffer which the host will supply; the
 * stepSize should be equal to the blockSize.
 *
 * If the internal step size used for the plugin differs from that
 * supplied by the host, the adapter will modify the sample type and
 * rate specifications for the plugin outputs appropriately, and set
 * timestamps on the output features for outputs that formerly used a
 * different sample rate specification.  This is necessary in order to
 * obtain correct time stamping.
 * 
 * In other respects, the PluginBufferingAdapter behaves identically
 * to the plugin that it wraps. The wrapped plugin will be deleted
 * when the wrapper is deleted.
 */
		
class PluginBufferingAdapter : public PluginWrapper
{
public:
    /**
     * Construct a PluginBufferingAdapter wrapping the given plugin.
     * The adapter takes ownership of the plugin, which will be
     * deleted when the adapter is deleted.
     */
    PluginBufferingAdapter(Plugin *plugin);
    virtual ~PluginBufferingAdapter();

    /**
     * Return the preferred step size for this adapter.
     * 
     * Because of the way this adapter works, its preferred step size
     * will always be the same as its preferred block size.  This may
     * or may not be the same as the preferred step size of the
     * underlying plugin, which may be obtained by calling
     * getPluginPreferredStepSize().
     */
    size_t getPreferredStepSize() const;

    /**
     * Return the preferred block size for this adapter.
     * 
     * This may or may not be the same as the preferred block size of
     * the underlying plugin, which may be obtained by calling
     * getPluginPreferredBlockSize().
     *
     * Note that this adapter may be initialised with any block size,
     * not just its supposedly preferred one.
     */
    size_t getPreferredBlockSize() const;

    /**
     * Initialise the adapter (and therefore the plugin) for the given
     * number of channels.  Initialise the adapter for the given step
     * and block size, which must be equal.
     *
     * The step and block size used for the underlying plugin will
     * depend on its preferences, or any values previously passed to
     * setPluginStepSize and setPluginBlockSize.
     */
    bool initialise(size_t channels, size_t stepSize, size_t blockSize);

    /**
     * Return the preferred step size of the plugin wrapped by this
     * adapter.
     *
     * This is included mainly for informational purposes.  This value
     * is not likely to be a valid step size for the adapter itself,
     * and it is not usually of any use in interpreting the results
     * (because the adapter re-writes OneSamplePerStep outputs to
     * FixedSampleRate so that the hop size no longer needs to be
     * known beforehand in order to interpret them).
     */
    size_t getPluginPreferredStepSize() const;

    /** 
     * Return the preferred block size of the plugin wrapped by this
     * adapter.
     *
     * This is included mainly for informational purposes.
     */
    size_t getPluginPreferredBlockSize() const;

    /**
     * Set the step size that will be used for the underlying plugin
     * when initialise() is called.  If this is not set, the plugin's
     * own preferred step size will be used.  You will not usually
     * need to call this function.  If you do call it, it must be
     * before the first call to initialise().
     */
    void setPluginStepSize(size_t stepSize);

    /**
     * Set the block size that will be used for the underlying plugin
     * when initialise() is called.  If this is not set, the plugin's
     * own preferred block size will be used.  You will not usually
     * need to call this function.  If you do call it, it must be
     * before the first call to initialise().
     */
    void setPluginBlockSize(size_t blockSize);

    /**
     * Return the step and block sizes that were actually used when
     * initialising the underlying plugin.
     *
     * This is included mainly for informational purposes.  You will
     * not usually need to call this function.  If this is called
     * before initialise(), it will return 0 for both values.  If it
     * is called after a failed call to initialise(), it will return
     * the values that were used in the failed call to the plugin's
     * initialise() function.
     */
    void getActualStepAndBlockSizes(size_t &stepSize, size_t &blockSize);

    void setParameter(std::string, float);
    void selectProgram(std::string);

    OutputList getOutputDescriptors() const;

    void reset();

    FeatureSet process(const float *const *inputBuffers, RealTime timestamp);
    
    FeatureSet getRemainingFeatures();
    
protected:
    class Impl;
    Impl *m_impl;
};
    
}

}

_VAMP_SDK_HOSTSPACE_END(PluginBufferingAdapter.h)

#endif
