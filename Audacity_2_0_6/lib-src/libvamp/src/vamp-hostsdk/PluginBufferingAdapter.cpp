/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    Vamp

    An API for audio analysis and feature extraction plugins.

    Centre for Digital Music, Queen Mary, University of London.
    Copyright 2006-2009 Chris Cannam and QMUL.
    This file by Mark Levy and Chris Cannam, Copyright 2007-2009 QMUL.
  
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

#include <vector>
#include <map>

#include <vamp-hostsdk/PluginBufferingAdapter.h>
#include <vamp-hostsdk/PluginInputDomainAdapter.h>

using std::vector;
using std::map;

_VAMP_SDK_HOSTSPACE_BEGIN(PluginBufferingAdapter.cpp)

namespace Vamp {
	
namespace HostExt {
		
class PluginBufferingAdapter::Impl
{
public:
    Impl(Plugin *plugin, float inputSampleRate);
    ~Impl();
	
    void setPluginStepSize(size_t stepSize);	
    void setPluginBlockSize(size_t blockSize);

    bool initialise(size_t channels, size_t stepSize, size_t blockSize);

    void getActualStepAndBlockSizes(size_t &stepSize, size_t &blockSize);

    OutputList getOutputDescriptors() const;

    void setParameter(std::string, float);
    void selectProgram(std::string);

    void reset();

    FeatureSet process(const float *const *inputBuffers, RealTime timestamp);
		
    FeatureSet getRemainingFeatures();
		
protected:
    class RingBuffer
    {
    public:
        RingBuffer(int n) :
            m_buffer(new float[n+1]), m_writer(0), m_reader(0), m_size(n+1) { }
        virtual ~RingBuffer() { delete[] m_buffer; }

        int getSize() const { return m_size-1; }
        void reset() { m_writer = 0; m_reader = 0; }

        int getReadSpace() const {
            int writer = m_writer, reader = m_reader, space;
            if (writer > reader) space = writer - reader;
            else if (writer < reader) space = (writer + m_size) - reader;
            else space = 0;
            return space;
        }

        int getWriteSpace() const {
            int writer = m_writer;
            int reader = m_reader;
            int space = (reader + m_size - writer - 1);
            if (space >= m_size) space -= m_size;
            return space;
        }
        
        int peek(float *destination, int n) const {

            int available = getReadSpace();

            if (n > available) {
                for (int i = available; i < n; ++i) {
                    destination[i] = 0.f;
                }
                n = available;
            }
            if (n == 0) return n;

            int reader = m_reader;
            int here = m_size - reader;
            const float *const bufbase = m_buffer + reader;

            if (here >= n) {
                for (int i = 0; i < n; ++i) {
                    destination[i] = bufbase[i];
                }
            } else {
                for (int i = 0; i < here; ++i) {
                    destination[i] = bufbase[i];
                }
                float *const destbase = destination + here;
                const int nh = n - here;
                for (int i = 0; i < nh; ++i) {
                    destbase[i] = m_buffer[i];
                }
            }

            return n;
        }

        int skip(int n) {
            
            int available = getReadSpace();
            if (n > available) {
                n = available;
            }
            if (n == 0) return n;

            int reader = m_reader;
            reader += n;
            while (reader >= m_size) reader -= m_size;
            m_reader = reader;
            return n;
        }
        
        int write(const float *source, int n) {

            int available = getWriteSpace();
            if (n > available) {
                n = available;
            }
            if (n == 0) return n;

            int writer = m_writer;
            int here = m_size - writer;
            float *const bufbase = m_buffer + writer;
            
            if (here >= n) {
                for (int i = 0; i < n; ++i) {
                    bufbase[i] = source[i];
                }
            } else {
                for (int i = 0; i < here; ++i) {
                    bufbase[i] = source[i];
                }
                const int nh = n - here;
                const float *const srcbase = source + here;
                float *const buf = m_buffer;
                for (int i = 0; i < nh; ++i) {
                    buf[i] = srcbase[i];
                }
            }

            writer += n;
            while (writer >= m_size) writer -= m_size;
            m_writer = writer;

            return n;
        }

        int zero(int n) {
            
            int available = getWriteSpace();
            if (n > available) {
                n = available;
            }
            if (n == 0) return n;

            int writer = m_writer;
            int here = m_size - writer;
            float *const bufbase = m_buffer + writer;

            if (here >= n) {
                for (int i = 0; i < n; ++i) {
                    bufbase[i] = 0.f;
                }
            } else {
                for (int i = 0; i < here; ++i) {
                    bufbase[i] = 0.f;
                }
                const int nh = n - here;
                for (int i = 0; i < nh; ++i) {
                    m_buffer[i] = 0.f;
                }
            }
            
            writer += n;
            while (writer >= m_size) writer -= m_size;
            m_writer = writer;

            return n;
        }

    protected:
        float *m_buffer;
        int    m_writer;
        int    m_reader;
        int    m_size;

    private:
        RingBuffer(const RingBuffer &); // not provided
        RingBuffer &operator=(const RingBuffer &); // not provided
    };

    Plugin *m_plugin;
    size_t m_inputStepSize;  // value passed to wrapper initialise()
    size_t m_inputBlockSize; // value passed to wrapper initialise()
    size_t m_setStepSize;    // value passed to setPluginStepSize()
    size_t m_setBlockSize;   // value passed to setPluginBlockSize()
    size_t m_stepSize;       // value actually used to initialise plugin
    size_t m_blockSize;      // value actually used to initialise plugin
    size_t m_channels;
    vector<RingBuffer *> m_queue;
    float **m_buffers;
    float m_inputSampleRate;
    long m_frame;
    bool m_unrun;
    mutable OutputList m_outputs;
    mutable std::map<int, bool> m_rewriteOutputTimes;
    std::map<int, int> m_fixedRateFeatureNos; // output no -> feature no
		
    void processBlock(FeatureSet& allFeatureSets);
    void adjustFixedRateFeatureTime(int outputNo, Feature &);
};
		
PluginBufferingAdapter::PluginBufferingAdapter(Plugin *plugin) :
    PluginWrapper(plugin)
{
    m_impl = new Impl(plugin, m_inputSampleRate);
}
		
PluginBufferingAdapter::~PluginBufferingAdapter()
{
    delete m_impl;
}

size_t
PluginBufferingAdapter::getPreferredStepSize() const
{
    return getPreferredBlockSize();
}

size_t
PluginBufferingAdapter::getPreferredBlockSize() const
{
    return PluginWrapper::getPreferredBlockSize();
}

size_t
PluginBufferingAdapter::getPluginPreferredStepSize() const
{
    return PluginWrapper::getPreferredStepSize();
}

size_t
PluginBufferingAdapter::getPluginPreferredBlockSize() const
{
    return PluginWrapper::getPreferredBlockSize();
}

void
PluginBufferingAdapter::setPluginStepSize(size_t stepSize)
{
    m_impl->setPluginStepSize(stepSize);
}

void
PluginBufferingAdapter::setPluginBlockSize(size_t blockSize)
{
    m_impl->setPluginBlockSize(blockSize);
}

void
PluginBufferingAdapter::getActualStepAndBlockSizes(size_t &stepSize,
                                                   size_t &blockSize)
{
    m_impl->getActualStepAndBlockSizes(stepSize, blockSize);
}
		
bool
PluginBufferingAdapter::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    return m_impl->initialise(channels, stepSize, blockSize);
}

PluginBufferingAdapter::OutputList
PluginBufferingAdapter::getOutputDescriptors() const
{
    return m_impl->getOutputDescriptors();
}

void
PluginBufferingAdapter::setParameter(std::string name, float value)
{
    m_impl->setParameter(name, value);
}

void
PluginBufferingAdapter::selectProgram(std::string name)
{
    m_impl->selectProgram(name);
}

void
PluginBufferingAdapter::reset()
{
    m_impl->reset();
}
		
PluginBufferingAdapter::FeatureSet
PluginBufferingAdapter::process(const float *const *inputBuffers,
                                RealTime timestamp)
{
    return m_impl->process(inputBuffers, timestamp);
}
		
PluginBufferingAdapter::FeatureSet
PluginBufferingAdapter::getRemainingFeatures()
{
    return m_impl->getRemainingFeatures();
}
		
PluginBufferingAdapter::Impl::Impl(Plugin *plugin, float inputSampleRate) :
    m_plugin(plugin),
    m_inputStepSize(0),
    m_inputBlockSize(0),
    m_setStepSize(0),
    m_setBlockSize(0),
    m_stepSize(0),
    m_blockSize(0),
    m_channels(0), 
    m_queue(0),
    m_buffers(0),
    m_inputSampleRate(inputSampleRate),
    m_frame(0),
    m_unrun(true)
{
    (void)getOutputDescriptors(); // set up m_outputs and m_rewriteOutputTimes
}
		
PluginBufferingAdapter::Impl::~Impl()
{
    // the adapter will delete the plugin

    for (size_t i = 0; i < m_channels; ++i) {
        delete m_queue[i];
        delete[] m_buffers[i];
    }
    delete[] m_buffers;
}
		
void
PluginBufferingAdapter::Impl::setPluginStepSize(size_t stepSize)
{
    if (m_inputStepSize != 0) {
        std::cerr << "PluginBufferingAdapter::setPluginStepSize: ERROR: Cannot be called after initialise()" << std::endl;
        return;
    }
    m_setStepSize = stepSize;
}
		
void
PluginBufferingAdapter::Impl::setPluginBlockSize(size_t blockSize)
{
    if (m_inputBlockSize != 0) {
        std::cerr << "PluginBufferingAdapter::setPluginBlockSize: ERROR: Cannot be called after initialise()" << std::endl;
        return;
    }
    m_setBlockSize = blockSize;
}

void
PluginBufferingAdapter::Impl::getActualStepAndBlockSizes(size_t &stepSize,
                                                         size_t &blockSize)
{
    stepSize = m_stepSize;
    blockSize = m_blockSize;
}

bool
PluginBufferingAdapter::Impl::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (stepSize != blockSize) {
        std::cerr << "PluginBufferingAdapter::initialise: input stepSize must be equal to blockSize for this adapter (stepSize = " << stepSize << ", blockSize = " << blockSize << ")" << std::endl;
        return false;
    }

    m_channels = channels;	
    m_inputStepSize = stepSize;
    m_inputBlockSize = blockSize;

    // if the user has requested particular step or block sizes, use
    // those; otherwise use the step and block sizes which the plugin
    // prefers

    m_stepSize = 0;
    m_blockSize = 0;

    if (m_setStepSize > 0) {
        m_stepSize = m_setStepSize;
    }
    if (m_setBlockSize > 0) {
        m_blockSize = m_setBlockSize;
    }

    if (m_stepSize == 0 && m_blockSize == 0) {
        m_stepSize = m_plugin->getPreferredStepSize();
        m_blockSize = m_plugin->getPreferredBlockSize();
    }
    
    bool freq = (m_plugin->getInputDomain() == Vamp::Plugin::FrequencyDomain);
    
    // or sensible defaults if it has no preference
    if (m_blockSize == 0) {
        if (m_stepSize == 0) {
            m_blockSize = 1024;
            if (freq) {
                m_stepSize = m_blockSize / 2;
            } else {
                m_stepSize = m_blockSize;
            }
        } else if (freq) {
            m_blockSize = m_stepSize * 2;
        } else {
            m_blockSize = m_stepSize;
        }
    } else if (m_stepSize == 0) { // m_blockSize != 0 (that was handled above)
        if (freq) {
            m_stepSize = m_blockSize/2;
        } else {
            m_stepSize = m_blockSize;
        }
    }
    
    // current implementation breaks if step is greater than block
    if (m_stepSize > m_blockSize) {
        size_t newBlockSize;
        if (freq) {
            newBlockSize = m_stepSize * 2;
        } else {
            newBlockSize = m_stepSize;
        }
        std::cerr << "PluginBufferingAdapter::initialise: WARNING: step size " << m_stepSize << " is greater than block size " << m_blockSize << ": cannot handle this in adapter; adjusting block size to " << newBlockSize << std::endl;
        m_blockSize = newBlockSize;
    }
    
//    std::cerr << "PluginBufferingAdapter::initialise: NOTE: stepSize " << m_inputStepSize << " -> " << m_stepSize 
//              << ", blockSize " << m_inputBlockSize << " -> " << m_blockSize << std::endl;			

    m_buffers = new float *[m_channels];

    for (size_t i = 0; i < m_channels; ++i) {
        m_queue.push_back(new RingBuffer(m_blockSize + m_inputBlockSize));
        m_buffers[i] = new float[m_blockSize];
    }
    
    bool success = m_plugin->initialise(m_channels, m_stepSize, m_blockSize);

//    std::cerr << "PluginBufferingAdapter::initialise: success = " << success << std::endl;

    if (success) {
        // Re-query outputs; properties such as bin count may have
        // changed on initialise
        m_outputs.clear();
        (void)getOutputDescriptors();
    }

    return success;
}
		
PluginBufferingAdapter::OutputList
PluginBufferingAdapter::Impl::getOutputDescriptors() const
{
    if (m_outputs.empty()) {
//    std::cerr << "PluginBufferingAdapter::getOutputDescriptors: querying anew" << std::endl;

        m_outputs = m_plugin->getOutputDescriptors();
    }

    PluginBufferingAdapter::OutputList outs = m_outputs;

    for (size_t i = 0; i < outs.size(); ++i) {

        switch (outs[i].sampleType) {

        case OutputDescriptor::OneSamplePerStep:
            outs[i].sampleType = OutputDescriptor::FixedSampleRate;
            outs[i].sampleRate = (1.f / m_inputSampleRate) * m_stepSize;
            m_rewriteOutputTimes[i] = true;
            break;
            
        case OutputDescriptor::FixedSampleRate:
            if (outs[i].sampleRate == 0.f) {
                outs[i].sampleRate = (1.f / m_inputSampleRate) * m_stepSize;
            }
            // We actually only need to rewrite output times for
            // features that don't have timestamps already, but we
            // can't tell from here whether our features will have
            // timestamps or not
            m_rewriteOutputTimes[i] = true;
            break;

        case OutputDescriptor::VariableSampleRate:
            m_rewriteOutputTimes[i] = false;
            break;
        }
    }

    return outs;
}

void
PluginBufferingAdapter::Impl::setParameter(std::string name, float value)
{
    m_plugin->setParameter(name, value);

    // Re-query outputs; properties such as bin count may have changed
    m_outputs.clear();
    (void)getOutputDescriptors();
}

void
PluginBufferingAdapter::Impl::selectProgram(std::string name)
{
    m_plugin->selectProgram(name);

    // Re-query outputs; properties such as bin count may have changed
    m_outputs.clear();
    (void)getOutputDescriptors();
}

void
PluginBufferingAdapter::Impl::reset()
{
    m_frame = 0;
    m_unrun = true;

    for (size_t i = 0; i < m_queue.size(); ++i) {
        m_queue[i]->reset();
    }

    m_plugin->reset();
}

PluginBufferingAdapter::FeatureSet
PluginBufferingAdapter::Impl::process(const float *const *inputBuffers,
                                      RealTime timestamp)
{
    if (m_inputStepSize == 0) {
        std::cerr << "PluginBufferingAdapter::process: ERROR: Plugin has not been initialised" << std::endl;
        return FeatureSet();
    }

    FeatureSet allFeatureSets;

    if (m_unrun) {
        m_frame = RealTime::realTime2Frame(timestamp,
                                           int(m_inputSampleRate + 0.5));
        m_unrun = false;
    }
			
    // queue the new input
    
    for (size_t i = 0; i < m_channels; ++i) {
        int written = m_queue[i]->write(inputBuffers[i], m_inputBlockSize);
        if (written < int(m_inputBlockSize) && i == 0) {
            std::cerr << "WARNING: PluginBufferingAdapter::Impl::process: "
                      << "Buffer overflow: wrote " << written 
                      << " of " << m_inputBlockSize 
                      << " input samples (for plugin step size "
                      << m_stepSize << ", block size " << m_blockSize << ")"
                      << std::endl;
        }
    }    
    
    // process as much as we can

    while (m_queue[0]->getReadSpace() >= int(m_blockSize)) {
        processBlock(allFeatureSets);
    }	
    
    return allFeatureSets;
}
    
void
PluginBufferingAdapter::Impl::adjustFixedRateFeatureTime(int outputNo,
                                                         Feature &feature)
{
    if (feature.hasTimestamp) {
        double secs = feature.timestamp.sec;
        secs += feature.timestamp.nsec / 1e9;
        m_fixedRateFeatureNos[outputNo] =
            int(secs * double(m_outputs[outputNo].sampleRate) + 0.5);
    }

    feature.timestamp = RealTime::fromSeconds
        (m_fixedRateFeatureNos[outputNo] / double(m_outputs[outputNo].sampleRate));

    feature.hasTimestamp = true;
    
    m_fixedRateFeatureNos[outputNo] = m_fixedRateFeatureNos[outputNo] + 1;
}    

PluginBufferingAdapter::FeatureSet
PluginBufferingAdapter::Impl::getRemainingFeatures() 
{
    FeatureSet allFeatureSets;
    
    // process remaining samples in queue
    while (m_queue[0]->getReadSpace() >= int(m_blockSize)) {
        processBlock(allFeatureSets);
    }
    
    // pad any last samples remaining and process
    if (m_queue[0]->getReadSpace() > 0) {
        for (size_t i = 0; i < m_channels; ++i) {
            m_queue[i]->zero(m_blockSize - m_queue[i]->getReadSpace());
        }
        processBlock(allFeatureSets);
    }			
    
    // get remaining features			

    FeatureSet featureSet = m_plugin->getRemainingFeatures();

    for (map<int, FeatureList>::iterator iter = featureSet.begin();
         iter != featureSet.end(); ++iter) {

        int outputNo = iter->first;
        FeatureList featureList = iter->second;

        for (size_t i = 0; i < featureList.size(); ++i) {

            if (m_outputs[outputNo].sampleType ==
                OutputDescriptor::FixedSampleRate) {
                adjustFixedRateFeatureTime(outputNo, featureList[i]);
            }

            allFeatureSets[outputNo].push_back(featureList[i]);
        }
    }
    
    return allFeatureSets;
}
    
void
PluginBufferingAdapter::Impl::processBlock(FeatureSet& allFeatureSets)
{
    for (size_t i = 0; i < m_channels; ++i) {
        m_queue[i]->peek(m_buffers[i], m_blockSize);
    }

    long frame = m_frame;
    RealTime timestamp = RealTime::frame2RealTime
        (frame, int(m_inputSampleRate + 0.5));

    FeatureSet featureSet = m_plugin->process(m_buffers, timestamp);
    
    PluginWrapper *wrapper = dynamic_cast<PluginWrapper *>(m_plugin);
    RealTime adjustment;
    if (wrapper) {
        PluginInputDomainAdapter *ida =
            wrapper->getWrapper<PluginInputDomainAdapter>();
        if (ida) adjustment = ida->getTimestampAdjustment();
    }

    for (FeatureSet::iterator iter = featureSet.begin();
         iter != featureSet.end(); ++iter) {

        int outputNo = iter->first;

        if (m_rewriteOutputTimes[outputNo]) {
            
            FeatureList featureList = iter->second;
	
            for (size_t i = 0; i < featureList.size(); ++i) {

                switch (m_outputs[outputNo].sampleType) {

                case OutputDescriptor::OneSamplePerStep:
                    // use our internal timestamp, always
                    featureList[i].timestamp = timestamp + adjustment;
                    featureList[i].hasTimestamp = true;
                    break;

                case OutputDescriptor::FixedSampleRate:
                    adjustFixedRateFeatureTime(outputNo, featureList[i]);
                    break;

                case OutputDescriptor::VariableSampleRate:
                    // plugin must set timestamp
                    break;

                default:
                    break;
                }
            
                allFeatureSets[outputNo].push_back(featureList[i]);
            }
        } else {
            for (size_t i = 0; i < iter->second.size(); ++i) {
                allFeatureSets[outputNo].push_back(iter->second[i]);
            }
        }
    }
    
    // step forward

    for (size_t i = 0; i < m_channels; ++i) {
        m_queue[i]->skip(m_stepSize);
    }
    
    // increment internal frame counter each time we step forward
    m_frame += m_stepSize;
}

}
	
}

_VAMP_SDK_HOSTSPACE_END(PluginBufferingAdapter.cpp)
