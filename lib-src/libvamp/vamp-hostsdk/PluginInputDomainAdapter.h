/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    Copyright 2006-2009 Chris Cannam and QMUL.
  
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

#ifndef _VAMP_PLUGIN_INPUT_DOMAIN_ADAPTER_H_
#define _VAMP_PLUGIN_INPUT_DOMAIN_ADAPTER_H_

#include "hostguard.h"
#include "PluginWrapper.h"

_VAMP_SDK_HOSTSPACE_BEGIN(PluginInputDomainAdapter.h)

namespace Vamp {

namespace HostExt {

/**
 * \class PluginInputDomainAdapter PluginInputDomainAdapter.h <vamp-hostsdk/PluginInputDomainAdapter.h>
 * 
 * PluginInputDomainAdapter is a Vamp plugin adapter that converts
 * time-domain input into frequency-domain input for plugins that need
 * it.  This permits a host to use time- and frequency-domain plugins
 * interchangeably without needing to handle the conversion itself.
 *
 * This adapter uses a basic windowed FFT (using Hann window by
 * default) that supports power-of-two block sizes only.  If a
 * frequency domain plugin requests a non-power-of-two blocksize, the
 * adapter will adjust it to a nearby power of two instead.  Thus,
 * getPreferredBlockSize() will always return a power of two if the
 * wrapped plugin is a frequency domain one.  If the plugin doesn't
 * accept the adjusted power of two block size, initialise() will
 * fail.
 *
 * The adapter provides no way for the host to discover whether the
 * underlying plugin is actually a time or frequency domain plugin
 * (except that if the preferred block size is not a power of two, it
 * must be a time domain plugin).
 *
 * The FFT implementation is simple and self-contained, but unlikely
 * to be the fastest available: a host can usually do better if it
 * cares enough.
 *
 * The window shape for the FFT frame can be set using setWindowType
 * and the current shape retrieved using getWindowType.  (This was
 * added in v2.3 of the SDK.)
 *
 * In every respect other than its input domain handling, the
 * PluginInputDomainAdapter behaves identically to the plugin that it
 * wraps.  The wrapped plugin will be deleted when the wrapper is
 * deleted.
 *
 * \note This class was introduced in version 1.1 of the Vamp plugin SDK.
 */

class PluginInputDomainAdapter : public PluginWrapper
{
public:
    /**
     * Construct a PluginInputDomainAdapter wrapping the given plugin.
     * The adapter takes ownership of the plugin, which will be
     * deleted when the adapter is deleted.
     */
    PluginInputDomainAdapter(Plugin *plugin);
    virtual ~PluginInputDomainAdapter();
    
    bool initialise(size_t channels, size_t stepSize, size_t blockSize);
    void reset();

    InputDomain getInputDomain() const;

    size_t getPreferredStepSize() const;
    size_t getPreferredBlockSize() const;

    FeatureSet process(const float *const *inputBuffers, RealTime timestamp);

    /**
     * ProcessTimestampMethod determines how the
     * PluginInputDomainAdapter handles timestamps for the data passed
     * to the process() function of the plugin it wraps, in the case
     * where the plugin is expecting frequency-domain data.
     *
     * The Vamp specification requires that the timestamp passed to
     * the plugin for frequency-domain input should be that of the
     * centre of the processing block, rather than the start as is the
     * case for time-domain input.
     *
     * Since PluginInputDomainAdapter aims to be transparent in use,
     * it needs to handle this timestamp adjustment itself.  However,
     * some control is available over the method used for adjustment,
     * by means of the ProcessTimestampMethod setting.
     *
     * If ProcessTimestampMethod is set to ShiftTimestamp (the
     * default), then the data passed to the wrapped plugin will be
     * calculated from the same input data block as passed to the
     * wrapper, but the timestamp passed to the plugin will be
     * advanced by half of the window size.
     * 
     * If ProcessTimestampMethod is set to ShiftData, then the
     * timestamp passed to the wrapped plugin will be the same as that
     * passed to the process call of the wrapper, but the data block
     * used to calculate the input will be shifted back (earlier) by
     * half of the window size, with half a block of zero padding at
     * the start of the first process call.  This has the advantage of
     * preserving the first half block of audio without any
     * deterioration from window shaping.
     * 
     * If ProcessTimestampMethod is set to NoShift, then no adjustment
     * will be made and the timestamps will be incorrect.
     */
    enum ProcessTimestampMethod {
        ShiftTimestamp,
        ShiftData,
        NoShift
    };

    /**
     * Set the method used for timestamp adjustment in plugins taking
     * frequency-domain input.  See the ProcessTimestampMethod
     * documentation for details.
     *
     * This function must be called before the first call to
     * process().
     */
    void setProcessTimestampMethod(ProcessTimestampMethod);

    /**
     * Retrieve the method used for timestamp adjustment in plugins
     * taking frequency-domain input.  See the ProcessTimestampMethod
     * documentation for details.
     */
    ProcessTimestampMethod getProcessTimestampMethod() const;

    /**
     * Return the amount by which the timestamps supplied to process()
     * are being incremented when they are passed to the plugin's own
     * process() implementation.
     *
     * The Vamp API mandates that the timestamp passed to the plugin
     * for time-domain input should be the time of the first sample in
     * the block, but the timestamp passed for frequency-domain input
     * should be the timestamp of the centre of the block.
     *
     * The PluginInputDomainAdapter adjusts its timestamps properly so
     * that the plugin receives correct times, but in some
     * circumstances (such as for establishing the correct timing of
     * implicitly-timed features, i.e. features without their own
     * timestamps) the host may need to be aware that this adjustment
     * is taking place.
     *
     * If the plugin requires time-domain input or the
     * PluginInputDomainAdapter is configured with its
     * ProcessTimestampMethod set to ShiftData instead of
     * ShiftTimestamp, then this function will return zero.
     *
     * The result of calling this function before initialise() has
     * been called is undefined.
     */
    RealTime getTimestampAdjustment() const;

    /**
     * The set of supported window shapes.
     */
    enum WindowType {

        RectangularWindow    = 0,

        BartlettWindow       = 1, /// synonym for RectangularWindow
        TriangularWindow     = 1, /// synonym for BartlettWindow

        HammingWindow        = 2,

        HanningWindow        = 3, /// synonym for HannWindow
        HannWindow           = 3, /// synonym for HanningWindow

        BlackmanWindow       = 4,

        NuttallWindow        = 7,

        BlackmanHarrisWindow = 8
    };

    /**
     * Return the current window shape.  The default is HanningWindow.
     */
    WindowType getWindowType() const;
    
    /**
     * Set the current window shape.
     */
    void setWindowType(WindowType type);


protected:
    class Impl;
    Impl *m_impl;
};

}

}

_VAMP_SDK_HOSTSPACE_END(PluginInputDomainAdapter.h)

#endif
