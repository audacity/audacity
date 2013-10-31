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

#include <vamp-hostsdk/PluginHostAdapter.h>
#include <cstdlib>

#if ( VAMP_SDK_MAJOR_VERSION != 2 || VAMP_SDK_MINOR_VERSION != 5 )
#error Unexpected version of Vamp SDK header included
#endif

_VAMP_SDK_HOSTSPACE_BEGIN(PluginHostAdapter.cpp)

namespace Vamp
{

PluginHostAdapter::PluginHostAdapter(const VampPluginDescriptor *descriptor,
                                     float inputSampleRate) :
    Plugin(inputSampleRate),
    m_descriptor(descriptor)
{
//    std::cerr << "PluginHostAdapter::PluginHostAdapter (plugin = " << descriptor->name << ")" << std::endl;
    m_handle = m_descriptor->instantiate(m_descriptor, inputSampleRate);
    if (!m_handle) {
//        std::cerr << "WARNING: PluginHostAdapter: Plugin instantiation failed for plugin " << m_descriptor->name << std::endl;
    }
}

PluginHostAdapter::~PluginHostAdapter()
{
//    std::cerr << "PluginHostAdapter::~PluginHostAdapter (plugin = " << m_descriptor->name << ")" << std::endl;
    if (m_handle) m_descriptor->cleanup(m_handle);
}

std::vector<std::string>
PluginHostAdapter::getPluginPath()
{
    std::vector<std::string> path;
    std::string envPath;

    char *cpath = getenv("VAMP_PATH");
    if (cpath) envPath = cpath;

#ifdef _WIN32
#define PATH_SEPARATOR ';'
#define DEFAULT_VAMP_PATH "%ProgramFiles%\\Vamp Plugins"
#else
#define PATH_SEPARATOR ':'
#ifdef __APPLE__
#define DEFAULT_VAMP_PATH "$HOME/Library/Audio/Plug-Ins/Vamp:/Library/Audio/Plug-Ins/Vamp"
#else
#define DEFAULT_VAMP_PATH "$HOME/vamp:$HOME/.vamp:/usr/local/lib/vamp:/usr/lib/vamp"
#endif
#endif

    if (envPath == "") {
        envPath = DEFAULT_VAMP_PATH;
        char *chome = getenv("HOME");
        if (chome) {
            std::string home(chome);
            std::string::size_type f;
            while ((f = envPath.find("$HOME")) != std::string::npos &&
                    f < envPath.length()) {
                envPath.replace(f, 5, home);
            }
        }
#ifdef _WIN32
        char *cpfiles = getenv("ProgramFiles");
        if (!cpfiles) cpfiles = (char *)"C:\\Program Files";
        std::string pfiles(cpfiles);
        std::string::size_type f;
        while ((f = envPath.find("%ProgramFiles%")) != std::string::npos &&
               f < envPath.length()) {
            envPath.replace(f, 14, pfiles);
        }
#endif
    }

    std::string::size_type index = 0, newindex = 0;

    while ((newindex = envPath.find(PATH_SEPARATOR, index)) < envPath.size()) {
	path.push_back(envPath.substr(index, newindex - index));
	index = newindex + 1;
    }
    
    path.push_back(envPath.substr(index));

    return path;
}

bool
PluginHostAdapter::initialise(size_t channels,
                              size_t stepSize,
                              size_t blockSize)
{
    if (!m_handle) return false;
    return m_descriptor->initialise(m_handle, channels, stepSize, blockSize) ?
        true : false;
}

void
PluginHostAdapter::reset()
{
    if (!m_handle) {
//        std::cerr << "PluginHostAdapter::reset: no handle" << std::endl;
        return;
    }
//    std::cerr << "PluginHostAdapter::reset(" << m_handle << ")" << std::endl;
    m_descriptor->reset(m_handle);
}

PluginHostAdapter::InputDomain
PluginHostAdapter::getInputDomain() const
{
    if (m_descriptor->inputDomain == vampFrequencyDomain) {
        return FrequencyDomain;
    } else {
        return TimeDomain;
    }
}

unsigned int
PluginHostAdapter::getVampApiVersion() const
{
    return m_descriptor->vampApiVersion;
}

std::string
PluginHostAdapter::getIdentifier() const
{
    return m_descriptor->identifier;
}

std::string
PluginHostAdapter::getName() const
{
    return m_descriptor->name;
}

std::string
PluginHostAdapter::getDescription() const
{
    return m_descriptor->description;
}

std::string
PluginHostAdapter::getMaker() const
{
    return m_descriptor->maker;
}

int
PluginHostAdapter::getPluginVersion() const
{
    return m_descriptor->pluginVersion;
}

std::string
PluginHostAdapter::getCopyright() const
{
    return m_descriptor->copyright;
}

PluginHostAdapter::ParameterList
PluginHostAdapter::getParameterDescriptors() const
{
    ParameterList list;
    for (unsigned int i = 0; i < m_descriptor->parameterCount; ++i) {
        const VampParameterDescriptor *spd = m_descriptor->parameters[i];
        ParameterDescriptor pd;
        pd.identifier = spd->identifier;
        pd.name = spd->name;
        pd.description = spd->description;
        pd.unit = spd->unit;
        pd.minValue = spd->minValue;
        pd.maxValue = spd->maxValue;
        pd.defaultValue = spd->defaultValue;
        pd.isQuantized = spd->isQuantized;
        pd.quantizeStep = spd->quantizeStep;
        if (pd.isQuantized && spd->valueNames) {
            for (unsigned int j = 0; spd->valueNames[j]; ++j) {
                pd.valueNames.push_back(spd->valueNames[j]);
            }
        }
        list.push_back(pd);
    }
    return list;
}

float
PluginHostAdapter::getParameter(std::string param) const
{
    if (!m_handle) return 0.0;

    for (unsigned int i = 0; i < m_descriptor->parameterCount; ++i) {
        if (param == m_descriptor->parameters[i]->identifier) {
            return m_descriptor->getParameter(m_handle, i);
        }
    }

    return 0.0;
}

void
PluginHostAdapter::setParameter(std::string param, 
                                float value)
{
    if (!m_handle) return;

    for (unsigned int i = 0; i < m_descriptor->parameterCount; ++i) {
        if (param == m_descriptor->parameters[i]->identifier) {
            m_descriptor->setParameter(m_handle, i, value);
            return;
        }
    }
}

PluginHostAdapter::ProgramList
PluginHostAdapter::getPrograms() const
{
    ProgramList list;
    
    for (unsigned int i = 0; i < m_descriptor->programCount; ++i) {
        list.push_back(m_descriptor->programs[i]);
    }
    
    return list;
}

std::string
PluginHostAdapter::getCurrentProgram() const
{
    if (!m_handle) return "";

    int pn = m_descriptor->getCurrentProgram(m_handle);
    if (pn < (int)m_descriptor->programCount) {
        return m_descriptor->programs[pn];
    } else {
        return "";
    }
}

void
PluginHostAdapter::selectProgram(std::string program)
{
    if (!m_handle) return;

    for (unsigned int i = 0; i < m_descriptor->programCount; ++i) {
        if (program == m_descriptor->programs[i]) {
            m_descriptor->selectProgram(m_handle, i);
            return;
        }
    }
}

size_t
PluginHostAdapter::getPreferredStepSize() const
{
    if (!m_handle) return 0;
    return m_descriptor->getPreferredStepSize(m_handle);
}

size_t
PluginHostAdapter::getPreferredBlockSize() const
{
    if (!m_handle) return 0;
    return m_descriptor->getPreferredBlockSize(m_handle);
}

size_t
PluginHostAdapter::getMinChannelCount() const
{
    if (!m_handle) return 0;
    return m_descriptor->getMinChannelCount(m_handle);
}

size_t
PluginHostAdapter::getMaxChannelCount() const
{
    if (!m_handle) return 0;
    return m_descriptor->getMaxChannelCount(m_handle);
}

PluginHostAdapter::OutputList
PluginHostAdapter::getOutputDescriptors() const
{
    OutputList list;
    if (!m_handle) {
//        std::cerr << "PluginHostAdapter::getOutputDescriptors: no handle " << std::endl;
        return list;
    }

    unsigned int count = m_descriptor->getOutputCount(m_handle);

    for (unsigned int i = 0; i < count; ++i) {
        VampOutputDescriptor *sd = m_descriptor->getOutputDescriptor(m_handle, i);
        OutputDescriptor d;
        d.identifier = sd->identifier;
        d.name = sd->name;
        d.description = sd->description;
        d.unit = sd->unit;
        d.hasFixedBinCount = sd->hasFixedBinCount;
        d.binCount = sd->binCount;
        if (d.hasFixedBinCount && sd->binNames) {
            for (unsigned int j = 0; j < sd->binCount; ++j) {
                d.binNames.push_back(sd->binNames[j] ? sd->binNames[j] : "");
            }
        }
        d.hasKnownExtents = sd->hasKnownExtents;
        d.minValue = sd->minValue;
        d.maxValue = sd->maxValue;
        d.isQuantized = sd->isQuantized;
        d.quantizeStep = sd->quantizeStep;

        switch (sd->sampleType) {
        case vampOneSamplePerStep:
            d.sampleType = OutputDescriptor::OneSamplePerStep; break;
        case vampFixedSampleRate:
            d.sampleType = OutputDescriptor::FixedSampleRate; break;
        case vampVariableSampleRate:
            d.sampleType = OutputDescriptor::VariableSampleRate; break;
        }

        d.sampleRate = sd->sampleRate;

        if (m_descriptor->vampApiVersion >= 2) {
            d.hasDuration = sd->hasDuration;
        } else {
            d.hasDuration = false;
        }

        list.push_back(d);

        m_descriptor->releaseOutputDescriptor(sd);
    }

    return list;
}

PluginHostAdapter::FeatureSet
PluginHostAdapter::process(const float *const *inputBuffers,
                           RealTime timestamp)
{
    FeatureSet fs;
    if (!m_handle) return fs;

    int sec = timestamp.sec;
    int nsec = timestamp.nsec;
    
    VampFeatureList *features = m_descriptor->process(m_handle,
                                                      inputBuffers,
                                                      sec, nsec);
    
    convertFeatures(features, fs);
    m_descriptor->releaseFeatureSet(features);
    return fs;
}

PluginHostAdapter::FeatureSet
PluginHostAdapter::getRemainingFeatures()
{
    FeatureSet fs;
    if (!m_handle) return fs;
    
    VampFeatureList *features = m_descriptor->getRemainingFeatures(m_handle); 

    convertFeatures(features, fs);
    m_descriptor->releaseFeatureSet(features);
    return fs;
}

void
PluginHostAdapter::convertFeatures(VampFeatureList *features,
                                   FeatureSet &fs)
{
    if (!features) return;

    unsigned int outputs = m_descriptor->getOutputCount(m_handle);

    for (unsigned int i = 0; i < outputs; ++i) {
        
        VampFeatureList &list = features[i];

        if (list.featureCount > 0) {

            Feature feature;
            feature.values.reserve(list.features[0].v1.valueCount);

            for (unsigned int j = 0; j < list.featureCount; ++j) {

                feature.hasTimestamp = list.features[j].v1.hasTimestamp;
                feature.timestamp = RealTime(list.features[j].v1.sec,
                                             list.features[j].v1.nsec);
                feature.hasDuration = false;

                if (m_descriptor->vampApiVersion >= 2) {
                    unsigned int j2 = j + list.featureCount;
                    feature.hasDuration = list.features[j2].v2.hasDuration;
                    feature.duration = RealTime(list.features[j2].v2.durationSec,
                                                list.features[j2].v2.durationNsec);
                }

                for (unsigned int k = 0; k < list.features[j].v1.valueCount; ++k) {
                    feature.values.push_back(list.features[j].v1.values[k]);
                }

                if (list.features[j].v1.label) {
                    feature.label = list.features[j].v1.label;
                }

                fs[i].push_back(feature);

                if (list.features[j].v1.valueCount > 0) {
                    feature.values.clear();
                }

                if (list.features[j].v1.label) {
                    feature.label = "";
                }
            }
        }
    }
}

}

_VAMP_SDK_HOSTSPACE_END(PluginHostAdapter.cpp)

