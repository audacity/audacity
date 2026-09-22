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

#include <vamp-sdk/PluginAdapter.h>

#include <cstring>
#include <cstdlib>

#include <mutex>

#if ( VAMP_SDK_MAJOR_VERSION != 2 || VAMP_SDK_MINOR_VERSION != 10 )
#error Unexpected version of Vamp SDK header included
#endif

using std::map;
using std::vector;
using std::string;
using std::cerr;
using std::endl;
using std::mutex;
using std::lock_guard;

//#define DEBUG_PLUGIN_ADAPTER 1

_VAMP_SDK_PLUGSPACE_BEGIN(PluginAdapter.cpp)

namespace Vamp {

class PluginAdapterBase::Impl
{
public:
    Impl(PluginAdapterBase *);
    ~Impl();

    const VampPluginDescriptor *getDescriptor();

protected:
    PluginAdapterBase *m_base;

    static VampPluginHandle vampInstantiate(const VampPluginDescriptor *desc,
                                            float inputSampleRate);

    static void vampCleanup(VampPluginHandle handle);

    static int vampInitialise(VampPluginHandle handle, unsigned int channels,
                             unsigned int stepSize, unsigned int blockSize);

    static void vampReset(VampPluginHandle handle);

    static float vampGetParameter(VampPluginHandle handle, int param);
    static void vampSetParameter(VampPluginHandle handle, int param, float value);

    static unsigned int vampGetCurrentProgram(VampPluginHandle handle);
    static void vampSelectProgram(VampPluginHandle handle, unsigned int program);

    static unsigned int vampGetPreferredStepSize(VampPluginHandle handle);
    static unsigned int vampGetPreferredBlockSize(VampPluginHandle handle);
    static unsigned int vampGetMinChannelCount(VampPluginHandle handle);
    static unsigned int vampGetMaxChannelCount(VampPluginHandle handle);

    static unsigned int vampGetOutputCount(VampPluginHandle handle);

    static VampOutputDescriptor *vampGetOutputDescriptor(VampPluginHandle handle,
                                                       unsigned int i);

    static void vampReleaseOutputDescriptor(VampOutputDescriptor *desc);

    static VampFeatureList *vampProcess(VampPluginHandle handle,
                                        const float *const *inputBuffers,
                                        int sec,
                                        int nsec);

    static VampFeatureList *vampGetRemainingFeatures(VampPluginHandle handle);

    static void vampReleaseFeatureSet(VampFeatureList *fs);

    void checkOutputMap(Plugin *plugin);
    void markOutputsChanged(Plugin *plugin);

    void cleanup(Plugin *plugin);
    unsigned int getOutputCount(Plugin *plugin);
    VampOutputDescriptor *getOutputDescriptor(Plugin *plugin, unsigned int i);
    VampFeatureList *process(Plugin *plugin,
                             const float *const *inputBuffers,
                             int sec, int nsec);
    VampFeatureList *getRemainingFeatures(Plugin *plugin);
    VampFeatureList *convertFeatures(Plugin *plugin,
                                     const Plugin::FeatureSet &features);
    
    // maps both plugins and descriptors to adapters
    typedef map<const void *, Impl *> AdapterMap;

    static AdapterMap *m_adapterMap;

    static mutex &adapterMapMutex() {
        // If this mutex was a global static, then it might be
        // destroyed before the last adapter, and we would end up
        // trying to lock an invalid mutex when removing an adapter
        // from the adapter map. To ensure it outlasts the adapters,
        // we need to ensure it is constructed before the construction
        // of any of them is complete, since destruction order is
        // reverse of construction. So we have to make sure this is
        // called from the PluginAdapterBase::Impl constructor below.
        static mutex m;
        return m;
    }
        
    static Impl *lookupAdapter(VampPluginHandle);

    mutex m_mutex; // guards all of the below
    
    bool m_populated;
    VampPluginDescriptor m_descriptor;
    Plugin::ParameterList m_parameters;
    Plugin::ProgramList m_programs;
    
    typedef map<Plugin *, Plugin::OutputList *> OutputMap;
    OutputMap m_pluginOutputs;

    map<Plugin *, VampFeatureList *> m_fs;
    map<Plugin *, vector<size_t> > m_fsizes;
    map<Plugin *, vector<vector<size_t> > > m_fvsizes;
    void resizeFS(Plugin *plugin, int n);
    void resizeFL(Plugin *plugin, int n, size_t sz);
    void resizeFV(Plugin *plugin, int n, int j, size_t sz);
};

PluginAdapterBase::PluginAdapterBase()
{
    m_impl = new Impl(this);
}

PluginAdapterBase::~PluginAdapterBase()
{
    delete m_impl;
}

const VampPluginDescriptor *
PluginAdapterBase::getDescriptor()
{
    return m_impl->getDescriptor();
}

PluginAdapterBase::Impl::Impl(PluginAdapterBase *base) :
    m_base(base),
    m_populated(false)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl[" << this << "]::Impl" << endl;
#endif

    (void)adapterMapMutex(); // see comment in adapterMapMutex function above
}

const VampPluginDescriptor *
PluginAdapterBase::Impl::getDescriptor()
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl[" << this << "]::getDescriptor" << endl;
#endif

    lock_guard<mutex> guard(m_mutex);
    
    if (m_populated) return &m_descriptor;
    
    Plugin *plugin = m_base->createPlugin(48000);
  
    if (!plugin) {
        cerr << "PluginAdapterBase::Impl::getDescriptor: Failed to create plugin" << endl;
        return 0;
    }

    if (plugin->getVampApiVersion() != VAMP_API_VERSION) {
        cerr << "Vamp::PluginAdapterBase::Impl::getDescriptor: ERROR: "
             << "API version " << plugin->getVampApiVersion()
             << " for\nplugin \"" << plugin->getIdentifier() << "\" "
             << "differs from version "
             << VAMP_API_VERSION << " for adapter.\n"
             << "This plugin is probably linked against a different version of the Vamp SDK\n"
             << "from the version it was compiled with.  It will need to be re-linked correctly\n"
             << "before it can be used." << endl;
        delete plugin;
        return 0;
    }

    m_parameters = plugin->getParameterDescriptors();
    m_programs = plugin->getPrograms();

    m_descriptor.vampApiVersion = plugin->getVampApiVersion();
    m_descriptor.identifier = strdup(plugin->getIdentifier().c_str());
    m_descriptor.name = strdup(plugin->getName().c_str());
    m_descriptor.description = strdup(plugin->getDescription().c_str());
    m_descriptor.maker = strdup(plugin->getMaker().c_str());
    m_descriptor.pluginVersion = plugin->getPluginVersion();
    m_descriptor.copyright = strdup(plugin->getCopyright().c_str());
    
    m_descriptor.parameterCount = m_parameters.size();
    m_descriptor.parameters = (const VampParameterDescriptor **)
        malloc(m_parameters.size() * sizeof(VampParameterDescriptor));

    unsigned int i;
    
    for (i = 0; i < m_parameters.size(); ++i) {
        VampParameterDescriptor *desc = (VampParameterDescriptor *)
            malloc(sizeof(VampParameterDescriptor));
        desc->identifier = strdup(m_parameters[i].identifier.c_str());
        desc->name = strdup(m_parameters[i].name.c_str());
        desc->description = strdup(m_parameters[i].description.c_str());
        desc->unit = strdup(m_parameters[i].unit.c_str());
        desc->minValue = m_parameters[i].minValue;
        desc->maxValue = m_parameters[i].maxValue;
        desc->defaultValue = m_parameters[i].defaultValue;
        desc->isQuantized = m_parameters[i].isQuantized;
        desc->quantizeStep = m_parameters[i].quantizeStep;
        desc->valueNames = 0;
        if (desc->isQuantized && !m_parameters[i].valueNames.empty()) {
            desc->valueNames = (const char **)
                malloc((m_parameters[i].valueNames.size()+1) * sizeof(char *));
            for (unsigned int j = 0; j < m_parameters[i].valueNames.size(); ++j) {
                desc->valueNames[j] = strdup(m_parameters[i].valueNames[j].c_str());
            }
            desc->valueNames[m_parameters[i].valueNames.size()] = 0;
        }
        m_descriptor.parameters[i] = desc;
    }
    
    m_descriptor.programCount = m_programs.size();
    m_descriptor.programs = (const char **)
        malloc(m_programs.size() * sizeof(const char *));
    
    for (i = 0; i < m_programs.size(); ++i) {
        m_descriptor.programs[i] = strdup(m_programs[i].c_str());
    }
    
    if (plugin->getInputDomain() == Plugin::FrequencyDomain) {
        m_descriptor.inputDomain = vampFrequencyDomain;
    } else {
        m_descriptor.inputDomain = vampTimeDomain;
    }

    m_descriptor.instantiate = vampInstantiate;
    m_descriptor.cleanup = vampCleanup;
    m_descriptor.initialise = vampInitialise;
    m_descriptor.reset = vampReset;
    m_descriptor.getParameter = vampGetParameter;
    m_descriptor.setParameter = vampSetParameter;
    m_descriptor.getCurrentProgram = vampGetCurrentProgram;
    m_descriptor.selectProgram = vampSelectProgram;
    m_descriptor.getPreferredStepSize = vampGetPreferredStepSize;
    m_descriptor.getPreferredBlockSize = vampGetPreferredBlockSize;
    m_descriptor.getMinChannelCount = vampGetMinChannelCount;
    m_descriptor.getMaxChannelCount = vampGetMaxChannelCount;
    m_descriptor.getOutputCount = vampGetOutputCount;
    m_descriptor.getOutputDescriptor = vampGetOutputDescriptor;
    m_descriptor.releaseOutputDescriptor = vampReleaseOutputDescriptor;
    m_descriptor.process = vampProcess;
    m_descriptor.getRemainingFeatures = vampGetRemainingFeatures;
    m_descriptor.releaseFeatureSet = vampReleaseFeatureSet;

    lock_guard<mutex> adapterMapGuard(adapterMapMutex());
    
    if (!m_adapterMap) {
        m_adapterMap = new AdapterMap;
    }
    (*m_adapterMap)[&m_descriptor] = this;

    delete plugin;

    m_populated = true;
    return &m_descriptor;
}

PluginAdapterBase::Impl::~Impl()
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl[" << this << "]::~Impl" << endl;
#endif

    lock_guard<mutex> guard(m_mutex);

    if (!m_populated) return;

    free((void *)m_descriptor.identifier);
    free((void *)m_descriptor.name);
    free((void *)m_descriptor.description);
    free((void *)m_descriptor.maker);
    free((void *)m_descriptor.copyright);
        
    for (unsigned int i = 0; i < m_descriptor.parameterCount; ++i) {
        const VampParameterDescriptor *desc = m_descriptor.parameters[i];
        free((void *)desc->identifier);
        free((void *)desc->name);
        free((void *)desc->description);
        free((void *)desc->unit);
        if (desc->valueNames) {
            for (unsigned int j = 0; desc->valueNames[j]; ++j) {
                free((void *)desc->valueNames[j]);
            }
            free((void *)desc->valueNames);
        }
        free((void *)desc);
    }
    free((void *)m_descriptor.parameters);

    for (unsigned int i = 0; i < m_descriptor.programCount; ++i) {
        free((void *)m_descriptor.programs[i]);
    }
    free((void *)m_descriptor.programs);

    lock_guard<mutex> adapterMapGuard(adapterMapMutex());
    
    if (m_adapterMap) {
        
        m_adapterMap->erase(&m_descriptor);
        
        if (m_adapterMap->empty()) {
            delete m_adapterMap;
            m_adapterMap = 0;
        }
    }
}

PluginAdapterBase::Impl *
PluginAdapterBase::Impl::lookupAdapter(VampPluginHandle handle)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::lookupAdapter(" << handle << ")" << endl;
#endif

    lock_guard<mutex> adapterMapGuard(adapterMapMutex());
    
    if (!m_adapterMap) return 0;
    AdapterMap::const_iterator i = m_adapterMap->find(handle);
    if (i == m_adapterMap->end()) return 0;
    return i->second;
}

VampPluginHandle
PluginAdapterBase::Impl::vampInstantiate(const VampPluginDescriptor *desc,
                                         float inputSampleRate)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampInstantiate(" << desc << ")" << endl;
#endif

    lock_guard<mutex> adapterMapGuard(adapterMapMutex());
    
    if (!m_adapterMap) {
        m_adapterMap = new AdapterMap();
    }

    if (m_adapterMap->find(desc) == m_adapterMap->end()) {
        cerr << "WARNING: PluginAdapterBase::Impl::vampInstantiate: Descriptor " << desc << " not in adapter map" << endl;
        return 0;
    }

    Impl *adapter = (*m_adapterMap)[desc];
    if (desc != &adapter->m_descriptor) return 0;

    Plugin *plugin = adapter->m_base->createPlugin(inputSampleRate);
    if (plugin) {
        (*m_adapterMap)[plugin] = adapter;
    }

#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampInstantiate(" << desc << "): returning handle " << plugin << endl;
#endif

    return plugin;
}

void
PluginAdapterBase::Impl::vampCleanup(VampPluginHandle handle)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampCleanup(" << handle << ")" << endl;
#endif

    Impl *adapter = lookupAdapter(handle);
    if (!adapter) {
        delete ((Plugin *)handle);
        return;
    }
    adapter->cleanup(((Plugin *)handle));
}

int
PluginAdapterBase::Impl::vampInitialise(VampPluginHandle handle,
                                        unsigned int channels,
                                        unsigned int stepSize,
                                        unsigned int blockSize)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampInitialise(" << handle << ", " << channels << ", " << stepSize << ", " << blockSize << ")" << endl;
#endif

    Impl *adapter = lookupAdapter(handle);
    if (!adapter) return 0;
    bool result = ((Plugin *)handle)->initialise(channels, stepSize, blockSize);
    adapter->markOutputsChanged((Plugin *)handle);
    return result ? 1 : 0;
}

void
PluginAdapterBase::Impl::vampReset(VampPluginHandle handle) 
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampReset(" << handle << ")" << endl;
#endif

    ((Plugin *)handle)->reset();
}

float
PluginAdapterBase::Impl::vampGetParameter(VampPluginHandle handle,
                                    int param) 
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampGetParameter(" << handle << ", " << param << ")" << endl;
#endif

    Impl *adapter = lookupAdapter(handle);
    if (!adapter) return 0.0;
    Plugin::ParameterList &list = adapter->m_parameters;
    return ((Plugin *)handle)->getParameter(list[param].identifier);
}

void
PluginAdapterBase::Impl::vampSetParameter(VampPluginHandle handle,
                                    int param, float value)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampSetParameter(" << handle << ", " << param << ", " << value << ")" << endl;
#endif

    Impl *adapter = lookupAdapter(handle);
    if (!adapter) return;
    Plugin::ParameterList &list = adapter->m_parameters;
    ((Plugin *)handle)->setParameter(list[param].identifier, value);
    adapter->markOutputsChanged((Plugin *)handle);
}

unsigned int
PluginAdapterBase::Impl::vampGetCurrentProgram(VampPluginHandle handle)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampGetCurrentProgram(" << handle << ")" << endl;
#endif

    Impl *adapter = lookupAdapter(handle);
    if (!adapter) return 0;
    Plugin::ProgramList &list = adapter->m_programs;
    string program = ((Plugin *)handle)->getCurrentProgram();
    for (unsigned int i = 0; i < list.size(); ++i) {
        if (list[i] == program) return i;
    }
    return 0;
}

void
PluginAdapterBase::Impl::vampSelectProgram(VampPluginHandle handle,
                                           unsigned int program)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampSelectProgram(" << handle << ", " << program << ")" << endl;
#endif

    Impl *adapter = lookupAdapter(handle);
    if (!adapter) return;

    Plugin::ProgramList &list = adapter->m_programs;
    ((Plugin *)handle)->selectProgram(list[program]);

    adapter->markOutputsChanged((Plugin *)handle);
}

unsigned int
PluginAdapterBase::Impl::vampGetPreferredStepSize(VampPluginHandle handle)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampGetPreferredStepSize(" << handle << ")" << endl;
#endif

    return ((Plugin *)handle)->getPreferredStepSize();
}

unsigned int
PluginAdapterBase::Impl::vampGetPreferredBlockSize(VampPluginHandle handle) 
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampGetPreferredBlockSize(" << handle << ")" << endl;
#endif

    return ((Plugin *)handle)->getPreferredBlockSize();
}

unsigned int
PluginAdapterBase::Impl::vampGetMinChannelCount(VampPluginHandle handle)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampGetMinChannelCount(" << handle << ")" << endl;
#endif

    return ((Plugin *)handle)->getMinChannelCount();
}

unsigned int
PluginAdapterBase::Impl::vampGetMaxChannelCount(VampPluginHandle handle)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampGetMaxChannelCount(" << handle << ")" << endl;
#endif

    return ((Plugin *)handle)->getMaxChannelCount();
}

unsigned int
PluginAdapterBase::Impl::vampGetOutputCount(VampPluginHandle handle)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampGetOutputCount(" << handle << ")" << endl;
#endif

    Impl *adapter = lookupAdapter(handle);

//    cerr << "vampGetOutputCount: handle " << handle << " -> adapter "<< adapter << endl;

    if (!adapter) return 0;
    return adapter->getOutputCount((Plugin *)handle);
}

VampOutputDescriptor *
PluginAdapterBase::Impl::vampGetOutputDescriptor(VampPluginHandle handle,
                                                 unsigned int i)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampGetOutputDescriptor(" << handle << ", " << i << ")" << endl;
#endif

    Impl *adapter = lookupAdapter(handle);

//    cerr << "vampGetOutputDescriptor: handle " << handle << " -> adapter "<< adapter << endl;

    if (!adapter) return 0;
    return adapter->getOutputDescriptor((Plugin *)handle, i);
}

void
PluginAdapterBase::Impl::vampReleaseOutputDescriptor(VampOutputDescriptor *desc)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampReleaseOutputDescriptor(" << desc << ")" << endl;
#endif

    if (desc->identifier) free((void *)desc->identifier);
    if (desc->name) free((void *)desc->name);
    if (desc->description) free((void *)desc->description);
    if (desc->unit) free((void *)desc->unit);
    if (desc->hasFixedBinCount && desc->binNames) {
        for (unsigned int i = 0; i < desc->binCount; ++i) {
            if (desc->binNames[i]) {
                free((void *)desc->binNames[i]);
            }
        }
    }
    if (desc->binNames) free((void *)desc->binNames);
    free((void *)desc);
}

VampFeatureList *
PluginAdapterBase::Impl::vampProcess(VampPluginHandle handle,
                                     const float *const *inputBuffers,
                                     int sec,
                                     int nsec)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampProcess(" << handle << ", " << sec << ", " << nsec << ")" << endl;
#endif

    Impl *adapter = lookupAdapter(handle);
    if (!adapter) return 0;
    return adapter->process((Plugin *)handle, inputBuffers, sec, nsec);
}

VampFeatureList *
PluginAdapterBase::Impl::vampGetRemainingFeatures(VampPluginHandle handle)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampGetRemainingFeatures(" << handle << ")" << endl;
#endif

    Impl *adapter = lookupAdapter(handle);
    if (!adapter) return 0;
    return adapter->getRemainingFeatures((Plugin *)handle);
}

void
PluginAdapterBase::Impl::vampReleaseFeatureSet(VampFeatureList *)
{
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::vampReleaseFeatureSet" << endl;
#endif
}

void 
PluginAdapterBase::Impl::cleanup(Plugin *plugin)
{
    // at this point no mutex is held
    
    lock_guard<mutex> adapterMapGuard(adapterMapMutex());
    lock_guard<mutex> guard(m_mutex);
    
    if (m_fs.find(plugin) != m_fs.end()) {
        size_t outputCount = 0;
        if (m_pluginOutputs[plugin]) {
            outputCount = m_pluginOutputs[plugin]->size();
        }
#ifdef DEBUG_PLUGIN_ADAPTER
        cerr << "PluginAdapterBase::Impl::cleanup: " << outputCount << " output(s)" << endl;
#endif
        VampFeatureList *list = m_fs[plugin];
        for (unsigned int i = 0; i < outputCount; ++i) {
            for (unsigned int j = 0; j < m_fsizes[plugin][i]; ++j) {
                if (list[i].features[j].v1.label) {
                    free(list[i].features[j].v1.label);
                }
                if (list[i].features[j].v1.values) {
                    free(list[i].features[j].v1.values);
                }
            }
            if (list[i].features) free(list[i].features);
        }
        if (list) free((void *)list);
        m_fs.erase(plugin);
        m_fsizes.erase(plugin);
        m_fvsizes.erase(plugin);
    }

    if (m_pluginOutputs.find(plugin) != m_pluginOutputs.end()) {
        delete m_pluginOutputs[plugin];
        m_pluginOutputs.erase(plugin);
    }

    if (m_adapterMap) {
        m_adapterMap->erase(plugin);

        if (m_adapterMap->empty()) {
            delete m_adapterMap;
            m_adapterMap = 0;
        }
    }

    delete ((Plugin *)plugin);
}

void 
PluginAdapterBase::Impl::checkOutputMap(Plugin *plugin)
{
    // must be called with m_mutex held
    
    OutputMap::iterator i = m_pluginOutputs.find(plugin);

    if (i == m_pluginOutputs.end() || !i->second) {

        m_pluginOutputs[plugin] = new Plugin::OutputList
            (plugin->getOutputDescriptors());

//        cerr << "PluginAdapterBase::Impl::checkOutputMap: Have " << m_pluginOutputs[plugin]->size() << " outputs for plugin " << plugin->getIdentifier() << endl;
    }
}

void
PluginAdapterBase::Impl::markOutputsChanged(Plugin *plugin)
{
    lock_guard<mutex> guard(m_mutex);

    OutputMap::iterator i = m_pluginOutputs.find(plugin);

//    cerr << "PluginAdapterBase::Impl::markOutputsChanged" << endl;

    if (i != m_pluginOutputs.end()) {

        Plugin::OutputList *list = i->second;
        m_pluginOutputs.erase(i);
        delete list;
    }
}

unsigned int 
PluginAdapterBase::Impl::getOutputCount(Plugin *plugin)
{
    lock_guard<mutex> guard(m_mutex);

    checkOutputMap(plugin);

    return m_pluginOutputs[plugin]->size();
}

VampOutputDescriptor *
PluginAdapterBase::Impl::getOutputDescriptor(Plugin *plugin,
                                             unsigned int i)
{
    lock_guard<mutex> guard(m_mutex);

    checkOutputMap(plugin);

    Plugin::OutputDescriptor &od =
        (*m_pluginOutputs[plugin])[i];

    VampOutputDescriptor *desc = (VampOutputDescriptor *)
        malloc(sizeof(VampOutputDescriptor));

    desc->identifier = strdup(od.identifier.c_str());
    desc->name = strdup(od.name.c_str());
    desc->description = strdup(od.description.c_str());
    desc->unit = strdup(od.unit.c_str());
    desc->hasFixedBinCount = od.hasFixedBinCount;
    desc->binCount = od.binCount;

    if (od.hasFixedBinCount && od.binCount > 0
        // We would like to do "&& !od.binNames.empty()" here -- but we
        // can't, because it will crash older versions of the host adapter
        // which try to copy the names across whenever the bin count is
        // non-zero, regardless of whether they exist or not
        ) {
        desc->binNames = (const char **)
            malloc(od.binCount * sizeof(const char *));
        
        for (unsigned int i = 0; i < od.binCount; ++i) {
            if (i < od.binNames.size()) {
                desc->binNames[i] = strdup(od.binNames[i].c_str());
            } else {
                desc->binNames[i] = 0;
            }
        }
    } else {
        desc->binNames = 0;
    }

    desc->hasKnownExtents = od.hasKnownExtents;
    desc->minValue = od.minValue;
    desc->maxValue = od.maxValue;
    desc->isQuantized = od.isQuantized;
    desc->quantizeStep = od.quantizeStep;

    switch (od.sampleType) {
    case Plugin::OutputDescriptor::OneSamplePerStep:
        desc->sampleType = vampOneSamplePerStep; break;
    case Plugin::OutputDescriptor::FixedSampleRate:
        desc->sampleType = vampFixedSampleRate; break;
    case Plugin::OutputDescriptor::VariableSampleRate:
        desc->sampleType = vampVariableSampleRate; break;
    }

    desc->sampleRate = od.sampleRate;
    desc->hasDuration = od.hasDuration;

    return desc;
}
    
VampFeatureList *
PluginAdapterBase::Impl::process(Plugin *plugin,
                                 const float *const *inputBuffers,
                                 int sec, int nsec)
{
//    cerr << "PluginAdapterBase::Impl::process" << endl;

    RealTime rt(sec, nsec);

    // We don't want to hold the mutex during the actual process call,
    // only while looking up the associated metadata
    {    
        lock_guard<mutex> guard(m_mutex);
        checkOutputMap(plugin);
    }

    return convertFeatures(plugin, plugin->process(inputBuffers, rt));
}
    
VampFeatureList *
PluginAdapterBase::Impl::getRemainingFeatures(Plugin *plugin)
{
//    cerr << "PluginAdapterBase::Impl::getRemainingFeatures" << endl;

    // We don't want to hold the mutex during the actual call, only
    // while looking up the associated metadata
    {    
        lock_guard<mutex> guard(m_mutex);
        checkOutputMap(plugin);
    }

    return convertFeatures(plugin, plugin->getRemainingFeatures());
}

VampFeatureList *
PluginAdapterBase::Impl::convertFeatures(Plugin *plugin,
                                         const Plugin::FeatureSet &features)
{
    lock_guard<mutex> guard(m_mutex);

    int lastN = -1;

    int outputCount = 0;
    if (m_pluginOutputs[plugin]) outputCount = m_pluginOutputs[plugin]->size();
    
    resizeFS(plugin, outputCount);
    VampFeatureList *fs = m_fs[plugin];

//    cerr << "PluginAdapter(v2)::convertFeatures: NOTE: sizeof(Feature) == " << sizeof(Plugin::Feature) << ", sizeof(VampFeature) == " << sizeof(VampFeature) << ", sizeof(VampFeatureList) == " << sizeof(VampFeatureList) << endl;

    for (Plugin::FeatureSet::const_iterator fi = features.begin();
         fi != features.end(); ++fi) {

        int n = fi->first;
        
//        cerr << "PluginAdapterBase::Impl::convertFeatures: n = " << n << endl;

        if (n >= int(outputCount)) {
            cerr << "WARNING: PluginAdapterBase::Impl::convertFeatures: Too many outputs from plugin (" << n+1 << ", only should be " << outputCount << ")" << endl;
            continue;
        }

        if (n > lastN + 1) {
            for (int i = lastN + 1; i < n; ++i) {
                fs[i].featureCount = 0;
            }
        }

        const Plugin::FeatureList &fl = fi->second;

        size_t sz = fl.size();
        if (sz > m_fsizes[plugin][n]) resizeFL(plugin, n, sz);
        fs[n].featureCount = sz;
        
        for (size_t j = 0; j < sz; ++j) {

//            cerr << "PluginAdapterBase::Impl::convertFeatures: j = " << j << endl;

            VampFeature *feature = &fs[n].features[j].v1;

            feature->hasTimestamp = fl[j].hasTimestamp;
            feature->sec = fl[j].timestamp.sec;
            feature->nsec = fl[j].timestamp.nsec;
            feature->valueCount = fl[j].values.size();

            VampFeatureV2 *v2 = &fs[n].features[j + sz].v2;
            
            v2->hasDuration = fl[j].hasDuration;
            v2->durationSec = fl[j].duration.sec;
            v2->durationNsec = fl[j].duration.nsec;

            if (feature->label) free(feature->label);

            if (fl[j].label.empty()) {
                feature->label = 0;
            } else {
                feature->label = strdup(fl[j].label.c_str());
            }

            if (feature->valueCount > m_fvsizes[plugin][n][j]) {
                resizeFV(plugin, n, j, feature->valueCount);
            }

            for (unsigned int k = 0; k < feature->valueCount; ++k) {
//                cerr << "PluginAdapterBase::Impl::convertFeatures: k = " << k << endl;
                feature->values[k] = fl[j].values[k];
            }
        }

        lastN = n;
    }

    if (lastN == -1) return 0;

    if (int(outputCount) > lastN + 1) {
        for (int i = lastN + 1; i < int(outputCount); ++i) {
            fs[i].featureCount = 0;
        }
    }

//    cerr << "PluginAdapter(v2)::convertFeatures: NOTE: have " << outputCount << " outputs" << endl;
//    for (int i = 0; i < outputCount; ++i) {
//        cerr << "PluginAdapter(v2)::convertFeatures: NOTE: output " << i << " has " << fs[i].featureCount << " features" << endl;
//    }


    return fs;
}

void
PluginAdapterBase::Impl::resizeFS(Plugin *plugin, int n)
{
    // called with m_mutex held
    
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::resizeFS(" << plugin << ", " << n << ")" << endl;
#endif

    int i = m_fsizes[plugin].size();
    if (i >= n) return;

#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "resizing from " << i << endl;
#endif

    m_fs[plugin] = (VampFeatureList *)realloc
        (m_fs[plugin], n * sizeof(VampFeatureList));

    while (i < n) {
        m_fs[plugin][i].featureCount = 0;
        m_fs[plugin][i].features = 0;
        m_fsizes[plugin].push_back(0);
        m_fvsizes[plugin].push_back(vector<size_t>());
        i++;
    }
}

void
PluginAdapterBase::Impl::resizeFL(Plugin *plugin, int n, size_t sz)
{
    // called with m_mutex held
    
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::resizeFL(" << plugin << ", " << n << ", "
              << sz << ")" << endl;
#endif
    
    size_t i = m_fsizes[plugin][n];
    if (i >= sz) return;

#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "resizing from " << i << endl;
#endif

    m_fs[plugin][n].features = (VampFeatureUnion *)realloc
        (m_fs[plugin][n].features, 2 * sz * sizeof(VampFeatureUnion));

    while (m_fsizes[plugin][n] < sz) {
        m_fs[plugin][n].features[m_fsizes[plugin][n]].v1.hasTimestamp = 0;
        m_fs[plugin][n].features[m_fsizes[plugin][n]].v1.valueCount = 0;
        m_fs[plugin][n].features[m_fsizes[plugin][n]].v1.values = 0;
        m_fs[plugin][n].features[m_fsizes[plugin][n]].v1.label = 0;
        m_fs[plugin][n].features[m_fsizes[plugin][n] + sz].v2.hasDuration = 0;
        m_fvsizes[plugin][n].push_back(0);
        m_fsizes[plugin][n]++;
    }
}

void
PluginAdapterBase::Impl::resizeFV(Plugin *plugin, int n, int j, size_t sz)
{
    // called with m_mutex held
    
#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "PluginAdapterBase::Impl::resizeFV(" << plugin << ", " << n << ", "
              << j << ", " << sz << ")" << endl;
#endif
    
    size_t i = m_fvsizes[plugin][n][j];
    if (i >= sz) return;

#ifdef DEBUG_PLUGIN_ADAPTER
    cerr << "resizing from " << i << endl;
#endif
    
    m_fs[plugin][n].features[j].v1.values = (float *)realloc
        (m_fs[plugin][n].features[j].v1.values, sz * sizeof(float));

    m_fvsizes[plugin][n][j] = sz;
}
  
PluginAdapterBase::Impl::AdapterMap *
PluginAdapterBase::Impl::m_adapterMap = 0;

}

_VAMP_SDK_PLUGSPACE_END(PluginAdapter.cpp)

