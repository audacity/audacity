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

#include <vamp-hostsdk/PluginWrapper.h>

_VAMP_SDK_HOSTSPACE_BEGIN(PluginWrapper.cpp)

namespace Vamp {

namespace HostExt {

class PluginRateExtractor : public Plugin
{
public:
    PluginRateExtractor() : Plugin(0) { }
    float getRate() const { return m_inputSampleRate; }
};

PluginWrapper::PluginWrapper(Plugin *plugin) :
    Plugin(((PluginRateExtractor *)plugin)->getRate()),
    m_plugin(plugin)
{
}

PluginWrapper::~PluginWrapper()
{
    delete m_plugin;
}

bool
PluginWrapper::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    return m_plugin->initialise(channels, stepSize, blockSize);
}

void
PluginWrapper::reset()
{
    m_plugin->reset();
}

Plugin::InputDomain
PluginWrapper::getInputDomain() const
{
    return m_plugin->getInputDomain();
}

unsigned int
PluginWrapper::getVampApiVersion() const
{
    return m_plugin->getVampApiVersion();
}

std::string
PluginWrapper::getIdentifier() const
{
    return m_plugin->getIdentifier();
}

std::string
PluginWrapper::getName() const
{
    return m_plugin->getName();
}

std::string
PluginWrapper::getDescription() const
{
    return m_plugin->getDescription();
}

std::string
PluginWrapper::getMaker() const
{
    return m_plugin->getMaker();
}

int
PluginWrapper::getPluginVersion() const
{
    return m_plugin->getPluginVersion();
}

std::string
PluginWrapper::getCopyright() const
{
    return m_plugin->getCopyright();
}

PluginBase::ParameterList
PluginWrapper::getParameterDescriptors() const
{
    return m_plugin->getParameterDescriptors();
}

float
PluginWrapper::getParameter(std::string parameter) const
{
    return m_plugin->getParameter(parameter);
}

void
PluginWrapper::setParameter(std::string parameter, float value)
{
    m_plugin->setParameter(parameter, value);
}

PluginBase::ProgramList
PluginWrapper::getPrograms() const
{
    return m_plugin->getPrograms();
}

std::string
PluginWrapper::getCurrentProgram() const
{
    return m_plugin->getCurrentProgram();
}

void
PluginWrapper::selectProgram(std::string program)
{
    m_plugin->selectProgram(program);
}

size_t
PluginWrapper::getPreferredStepSize() const
{
    return m_plugin->getPreferredStepSize();
}

size_t
PluginWrapper::getPreferredBlockSize() const
{
    return m_plugin->getPreferredBlockSize();
}

size_t
PluginWrapper::getMinChannelCount() const
{
    return m_plugin->getMinChannelCount();
}

size_t PluginWrapper::getMaxChannelCount() const
{
    return m_plugin->getMaxChannelCount();
}

Plugin::OutputList
PluginWrapper::getOutputDescriptors() const
{
    return m_plugin->getOutputDescriptors();
}

Plugin::FeatureSet
PluginWrapper::process(const float *const *inputBuffers, RealTime timestamp)
{
    return m_plugin->process(inputBuffers, timestamp);
}

Plugin::FeatureSet
PluginWrapper::getRemainingFeatures()
{
    return m_plugin->getRemainingFeatures();
}

}

}

_VAMP_SDK_HOSTSPACE_END(PluginWrapper.cpp)
