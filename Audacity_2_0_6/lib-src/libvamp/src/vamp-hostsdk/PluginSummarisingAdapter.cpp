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

#include <vamp-hostsdk/PluginSummarisingAdapter.h>

#include <map>
#include <algorithm>
#include <cmath>
#include <climits>

//#define DEBUG_PLUGIN_SUMMARISING_ADAPTER 1
//#define DEBUG_PLUGIN_SUMMARISING_ADAPTER_SEGMENT 1

_VAMP_SDK_HOSTSPACE_BEGIN(PluginSummarisingAdapter.cpp)

namespace Vamp {

namespace HostExt {

class PluginSummarisingAdapter::Impl
{
public:
    Impl(Plugin *plugin, float inputSampleRate);
    ~Impl();

    bool initialise(size_t channels, size_t stepSize, size_t blockSize);

    void reset();

    FeatureSet process(const float *const *inputBuffers, RealTime timestamp);
    FeatureSet getRemainingFeatures();

    void setSummarySegmentBoundaries(const SegmentBoundaries &);

    FeatureList getSummaryForOutput(int output,
                                    SummaryType type,
                                    AveragingMethod avg);

    FeatureSet getSummaryForAllOutputs(SummaryType type,
                                       AveragingMethod avg);

protected:
    Plugin *m_plugin;
    float m_inputSampleRate;
    size_t m_stepSize;
    size_t m_blockSize;

    SegmentBoundaries m_boundaries;

    typedef std::vector<float> ValueList;

    struct Result { // smaller than Feature
        RealTime time;
        RealTime duration;
        ValueList values; // bin number -> value
    };

    typedef std::vector<Result> ResultList;

    struct OutputAccumulator {
        int bins;
        ResultList results;
        OutputAccumulator() : bins(0) { }
    };

    typedef std::map<int, OutputAccumulator> OutputAccumulatorMap;
    OutputAccumulatorMap m_accumulators; // output number -> accumulator

    typedef std::map<RealTime, OutputAccumulator> SegmentAccumulatorMap;
    typedef std::map<int, SegmentAccumulatorMap> OutputSegmentAccumulatorMap;
    OutputSegmentAccumulatorMap m_segmentedAccumulators; // output -> segmented

    typedef std::map<int, RealTime> OutputTimestampMap;
    OutputTimestampMap m_prevTimestamps; // output number -> timestamp
    OutputTimestampMap m_prevDurations; // output number -> durations

    struct OutputBinSummary {

        int count;

        // extents
        double minimum;
        double maximum;
        double sum;

        // sample-average results
        double median;
        double mode;
        double variance;

        // continuous-time average results
        double median_c;
        double mode_c;
        double mean_c;
        double variance_c;
    };

    typedef std::map<int, OutputBinSummary> OutputSummary;
    typedef std::map<RealTime, OutputSummary> SummarySegmentMap;
    typedef std::map<int, SummarySegmentMap> OutputSummarySegmentMap;

    OutputSummarySegmentMap m_summaries;

    bool m_reduced;
    RealTime m_endTime;

    void accumulate(const FeatureSet &fs, RealTime, bool final);
    void accumulate(int output, const Feature &f, RealTime, bool final);
    void accumulateFinalDurations();
    void findSegmentBounds(RealTime t, RealTime &start, RealTime &end);
    void segment();
    void reduce();

    std::string getSummaryLabel(SummaryType type, AveragingMethod avg);
};

static RealTime INVALID_DURATION(INT_MIN, INT_MIN);
    
PluginSummarisingAdapter::PluginSummarisingAdapter(Plugin *plugin) :
    PluginWrapper(plugin)
{
    m_impl = new Impl(plugin, m_inputSampleRate);
}

PluginSummarisingAdapter::~PluginSummarisingAdapter()
{
    delete m_impl;
}

bool
PluginSummarisingAdapter::initialise(size_t channels,
                                     size_t stepSize, size_t blockSize)
{
    return
        PluginWrapper::initialise(channels, stepSize, blockSize) &&
        m_impl->initialise(channels, stepSize, blockSize);
}

void
PluginSummarisingAdapter::reset()
{
    m_impl->reset();
}

Plugin::FeatureSet
PluginSummarisingAdapter::process(const float *const *inputBuffers, RealTime timestamp)
{
    return m_impl->process(inputBuffers, timestamp);
}

Plugin::FeatureSet
PluginSummarisingAdapter::getRemainingFeatures()
{
    return m_impl->getRemainingFeatures();
}

void
PluginSummarisingAdapter::setSummarySegmentBoundaries(const SegmentBoundaries &b)
{
    m_impl->setSummarySegmentBoundaries(b);
}

Plugin::FeatureList
PluginSummarisingAdapter::getSummaryForOutput(int output,
                                              SummaryType type,
                                              AveragingMethod avg)
{
    return m_impl->getSummaryForOutput(output, type, avg);
}

Plugin::FeatureSet
PluginSummarisingAdapter::getSummaryForAllOutputs(SummaryType type,
                                                  AveragingMethod avg)
{
    return m_impl->getSummaryForAllOutputs(type, avg);
}

PluginSummarisingAdapter::Impl::Impl(Plugin *plugin, float inputSampleRate) :
    m_plugin(plugin),
    m_inputSampleRate(inputSampleRate),
    m_reduced(false)
{
}

PluginSummarisingAdapter::Impl::~Impl()
{
}

bool
PluginSummarisingAdapter::Impl::initialise(size_t channels,
                                           size_t stepSize, size_t blockSize)
{
    m_stepSize = stepSize;
    m_blockSize = blockSize;
    return true;
}

void
PluginSummarisingAdapter::Impl::reset()
{
    m_accumulators.clear();
    m_segmentedAccumulators.clear();
    m_prevTimestamps.clear();
    m_prevDurations.clear();
    m_summaries.clear();
    m_reduced = false;
    m_endTime = RealTime();
    m_plugin->reset();
}

Plugin::FeatureSet
PluginSummarisingAdapter::Impl::process(const float *const *inputBuffers,
                                        RealTime timestamp)
{
    if (m_reduced) {
        std::cerr << "WARNING: Cannot call PluginSummarisingAdapter::process() or getRemainingFeatures() after one of the getSummary methods" << std::endl;
    }
    FeatureSet fs = m_plugin->process(inputBuffers, timestamp);
    accumulate(fs, timestamp, false);
    m_endTime = timestamp + 
        RealTime::frame2RealTime(m_stepSize, int(m_inputSampleRate + 0.5));
    return fs;
}

Plugin::FeatureSet
PluginSummarisingAdapter::Impl::getRemainingFeatures()
{
    if (m_reduced) {
        std::cerr << "WARNING: Cannot call PluginSummarisingAdapter::process() or getRemainingFeatures() after one of the getSummary methods" << std::endl;
    }
    FeatureSet fs = m_plugin->getRemainingFeatures();
    accumulate(fs, m_endTime, true);
    return fs;
}

void
PluginSummarisingAdapter::Impl::setSummarySegmentBoundaries(const SegmentBoundaries &b)
{
    m_boundaries = b;
#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER
    std::cerr << "PluginSummarisingAdapter::setSummarySegmentBoundaries: boundaries are:" << std::endl;
    for (SegmentBoundaries::const_iterator i = m_boundaries.begin();
         i != m_boundaries.end(); ++i) {
        std::cerr << *i << "  ";
    }
    std::cerr << std::endl;
#endif
}

Plugin::FeatureList
PluginSummarisingAdapter::Impl::getSummaryForOutput(int output,
                                                    SummaryType type,
                                                    AveragingMethod avg)
{
    if (!m_reduced) {
        accumulateFinalDurations();
        segment();
        reduce();
        m_reduced = true;
    }

    bool continuous = (avg == ContinuousTimeAverage);

    FeatureList fl;
    for (SummarySegmentMap::const_iterator i = m_summaries[output].begin();
         i != m_summaries[output].end(); ++i) {

        Feature f;

        f.hasTimestamp = true;
        f.timestamp = i->first;

        f.hasDuration = true;
        SummarySegmentMap::const_iterator ii = i;
        if (++ii == m_summaries[output].end()) {
            f.duration = m_endTime - f.timestamp;
        } else {
            f.duration = ii->first - f.timestamp;
        }

        f.label = getSummaryLabel(type, avg);

        for (OutputSummary::const_iterator j = i->second.begin();
             j != i->second.end(); ++j) {

            // these will be ordered by bin number, and no bin numbers
            // will be missing except at the end (because of the way
            // the accumulators were initially filled in accumulate())

            const OutputBinSummary &summary = j->second;
            double result = 0.f;

            switch (type) {

            case Minimum:
                result = summary.minimum;
                break;

            case Maximum:
                result = summary.maximum;
                break;

            case Mean:
                if (continuous) {
                    result = summary.mean_c;
                } else if (summary.count) {
                    result = summary.sum / summary.count;
                }
                break;

            case Median:
                if (continuous) result = summary.median_c;
                else result = summary.median;
                break;

            case Mode:
                if (continuous) result = summary.mode_c;
                else result = summary.mode;
                break;

            case Sum:
                result = summary.sum;
                break;

            case Variance:
                if (continuous) result = summary.variance_c;
                else result = summary.variance;
                break;

            case StandardDeviation:
                if (continuous) result = sqrtf(summary.variance_c);
                else result = sqrtf(summary.variance);
                break;

            case Count:
                result = summary.count;
                break;

            case UnknownSummaryType:
                break;

            default:
                break;
            }
            
            f.values.push_back(result);
        }

        fl.push_back(f);
    }
    return fl;
}

Plugin::FeatureSet
PluginSummarisingAdapter::Impl::getSummaryForAllOutputs(SummaryType type,
                                                        AveragingMethod avg)
{
    if (!m_reduced) {
        accumulateFinalDurations();
        segment();
        reduce();
        m_reduced = true;
    }

    FeatureSet fs;
    for (OutputSummarySegmentMap::const_iterator i = m_summaries.begin();
         i != m_summaries.end(); ++i) {
        fs[i->first] = getSummaryForOutput(i->first, type, avg);
    }
    return fs;
}

void
PluginSummarisingAdapter::Impl::accumulate(const FeatureSet &fs,
                                           RealTime timestamp, 
                                           bool final)
{
    for (FeatureSet::const_iterator i = fs.begin(); i != fs.end(); ++i) {
        for (FeatureList::const_iterator j = i->second.begin();
             j != i->second.end(); ++j) {
            if (j->hasTimestamp) {
                accumulate(i->first, *j, j->timestamp, final);
            } else {
                //!!! is this correct?
                accumulate(i->first, *j, timestamp, final);
            }
        }
    }
}

std::string
PluginSummarisingAdapter::Impl::getSummaryLabel(SummaryType type,
                                                AveragingMethod avg)
{
    std::string label;
    std::string avglabel;

    if (avg == SampleAverage) avglabel = ", sample average";
    else avglabel = ", continuous-time average";

    switch (type) {
    case Minimum:  label = "(minimum value)"; break;
    case Maximum:  label = "(maximum value)"; break;
    case Mean:     label = "(mean value" + avglabel + ")"; break;
    case Median:   label = "(median value" + avglabel + ")"; break;
    case Mode:     label = "(modal value" + avglabel + ")"; break;
    case Sum:      label = "(sum)"; break;
    case Variance: label = "(variance" + avglabel + ")"; break;
    case StandardDeviation: label = "(standard deviation" + avglabel + ")"; break;
    case Count:    label = "(count)"; break;
    case UnknownSummaryType: label = "(unknown summary)"; break;
    }
    
    return label;
}

void
PluginSummarisingAdapter::Impl::accumulate(int output,
                                           const Feature &f,
                                           RealTime timestamp,
                                           bool final)
{
    // What should happen if a feature's duration spans a segment
    // boundary?  I think we probably want to chop it, and pretend
    // that it appears in both.  A very long feature (e.g. key, if the
    // whole audio is in a single key) might span many or all
    // segments, and we want that to be reflected in the results
    // (e.g. it is the modal key in all of those segments, not just
    // the first).  This is actually quite complicated to do.

    // If features spanning a boundary should be chopped, then we need
    // to have per-segment accumulators (and the feature value goes
    // into both -- with a separate phase to split the accumulator up
    // into segments).

#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER
    std::cerr << "output " << output << ": timestamp " << timestamp << ", prev timestamp " << m_prevTimestamps[output] << ", final " << final << std::endl;
#endif

    // At each process step, accumulate() is called once for each
    // feature on each output within that process's returned feature
    // list, and with the timestamp passed in being that of the start
    // of the process block.

    // At the end (in getRemainingFeatures), accumulate() is called
    // once for each feature on each output within the feature list
    // returned by getRemainingFeatures, and with the timestamp being
    // the same as the last process block and final set to true.

    // (What if getRemainingFeatures doesn't return any features?  We
    // still need to ensure that the final duration is written.  Need
    // a separate function to close the durations.)

    // At each call, we pull out the value for the feature and stuff
    // it into the accumulator's appropriate values array; and we
    // calculate the duration for the _previous_ feature, or pull it
    // from the prevDurations array if the previous feature had a
    // duration in its structure, and stuff that into the
    // accumulator's appropriate durations array.

    if (m_prevDurations.find(output) != m_prevDurations.end()) {

        // Not the first time accumulate has been called for this
        // output -- there has been a previous feature

        RealTime prevDuration;

        // Note that m_prevDurations[output] only contains the
        // duration field that was contained in the previous feature.
        // If it didn't have an explicit duration,
        // m_prevDurations[output] should be INVALID_DURATION and we
        // will have to calculate the duration from the previous and
        // current timestamps.

        if (m_prevDurations[output] != INVALID_DURATION) {
            prevDuration = m_prevDurations[output];
#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER
            std::cerr << "Previous duration from previous feature: " << prevDuration << std::endl;
#endif
        } else {
            prevDuration = timestamp - m_prevTimestamps[output];
#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER
            std::cerr << "Previous duration from diff: " << timestamp << " - "
                      << m_prevTimestamps[output] << std::endl;
#endif
        }

#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER
        std::cerr << "output " << output << ": ";
        std::cerr << "Pushing previous duration as " << prevDuration << std::endl;
#endif
        
        m_accumulators[output].results
            [m_accumulators[output].results.size() - 1]
            .duration = prevDuration;
    }

    if (f.hasDuration) m_prevDurations[output] = f.duration;
    else m_prevDurations[output] = INVALID_DURATION;

    m_prevTimestamps[output] = timestamp;

    if (f.hasDuration) {
        RealTime et = timestamp;
        et = et + f.duration;
        if (et > m_endTime) m_endTime = et;
    }

    Result result;
    result.time = timestamp;
    result.duration = INVALID_DURATION;

    if (int(f.values.size()) > m_accumulators[output].bins) {
        m_accumulators[output].bins = f.values.size();
    }

    for (int i = 0; i < int(f.values.size()); ++i) {
        result.values.push_back(f.values[i]);
    }

    m_accumulators[output].results.push_back(result);
}

void
PluginSummarisingAdapter::Impl::accumulateFinalDurations()
{
    for (OutputTimestampMap::iterator i = m_prevTimestamps.begin();
         i != m_prevTimestamps.end(); ++i) {

        int output = i->first;

        int acount = m_accumulators[output].results.size();

        if (acount == 0) continue;

#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER
        std::cerr << "output " << output << ": ";
#endif

        if (m_prevDurations.find(output) != m_prevDurations.end() &&
            m_prevDurations[output] != INVALID_DURATION) {

#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER
            std::cerr << "Pushing final duration from feature as " << m_prevDurations[output] << std::endl;
#endif

            m_accumulators[output].results[acount - 1].duration =
                m_prevDurations[output];

        } else {

#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER
            std::cerr << "Pushing final duration from diff as " << m_endTime << " - " << m_prevTimestamps[output] << std::endl;
#endif

            m_accumulators[output].results[acount - 1].duration =
                m_endTime - m_prevTimestamps[output];
        }
        
#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER
        std::cerr << "so duration for result no " << acount-1 << " is "
                  << m_accumulators[output].results[acount-1].duration
                  << std::endl;
#endif
    }
}

void
PluginSummarisingAdapter::Impl::findSegmentBounds(RealTime t,
                                                  RealTime &start,
                                                  RealTime &end)
{
#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER_SEGMENT
    std::cerr << "findSegmentBounds: t = " << t <<  std::endl;
#endif

    SegmentBoundaries::const_iterator i = std::upper_bound
        (m_boundaries.begin(), m_boundaries.end(), t);

    start = RealTime::zeroTime;
    end = m_endTime;

    if (i != m_boundaries.end()) {
        end = *i;
    }

    if (i != m_boundaries.begin()) {
        start = *--i;
    }

#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER_SEGMENT
    std::cerr << "findSegmentBounds: " << t << " is in segment " << start << " -> " << end << std::endl;
#endif
}

void
PluginSummarisingAdapter::Impl::segment()
{
    SegmentBoundaries::iterator boundaryitr = m_boundaries.begin();
    
#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER_SEGMENT
    std::cerr << "segment: starting" << std::endl;
#endif

    for (OutputAccumulatorMap::iterator i = m_accumulators.begin();
         i != m_accumulators.end(); ++i) {

        int output = i->first;
        OutputAccumulator &source = i->second;

#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER_SEGMENT
        std::cerr << "segment: total results for output " << output << " = "
                  << source.results.size() << std::endl;
#endif

        // This is basically nonsense if the results have no values
        // (i.e. their times and counts are the only things of
        // interest)... but perhaps it's the user's problem if they
        // ask for segmentation (or any summary at all) in that case

        for (int n = 0; n < int(source.results.size()); ++n) {
            
            // This result spans source.results[n].time to
            // source.results[n].time + source.results[n].duration.
            // We need to dispose it into segments appropriately

            RealTime resultStart = source.results[n].time;
            RealTime resultEnd = resultStart + source.results[n].duration;

#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER_SEGMENT
            std::cerr << "output: " << output << ", result start = " << resultStart << ", end = " << resultEnd << std::endl;
#endif

            RealTime segmentStart = RealTime::zeroTime;
            RealTime segmentEnd = resultEnd - RealTime(1, 0);
            
            RealTime prevSegmentStart = segmentStart - RealTime(1, 0);

            while (segmentEnd < resultEnd) {

#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER_SEGMENT
                std::cerr << "segment end " << segmentEnd << " < result end "
                          << resultEnd << " (with result start " << resultStart << ")" <<  std::endl;
#endif

                findSegmentBounds(resultStart, segmentStart, segmentEnd);

                if (segmentStart == prevSegmentStart) {
                    // This can happen when we reach the end of the
                    // input, if a feature's end time overruns the
                    // input audio end time
                    break;
                }
                prevSegmentStart = segmentStart;
                
                RealTime chunkStart = resultStart;
                if (chunkStart < segmentStart) chunkStart = segmentStart;

                RealTime chunkEnd = resultEnd;
                if (chunkEnd > segmentEnd) chunkEnd = segmentEnd;
                
                m_segmentedAccumulators[output][segmentStart].bins = source.bins;

                Result chunk;
                chunk.time = chunkStart;
                chunk.duration = chunkEnd - chunkStart;
                chunk.values = source.results[n].values;

#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER_SEGMENT
                std::cerr << "chunk for segment " << segmentStart << ": from " << chunk.time << ", duration " << chunk.duration << std::endl;
#endif

                m_segmentedAccumulators[output][segmentStart].results
                    .push_back(chunk);

                resultStart = chunkEnd;
            }
        }
    }
}

struct ValueDurationFloatPair
{
    float value;
    float duration;

    ValueDurationFloatPair() : value(0), duration(0) { }
    ValueDurationFloatPair(float v, float d) : value(v), duration(d) { }
    ValueDurationFloatPair &operator=(const ValueDurationFloatPair &p) {
        value = p.value;
        duration = p.duration;
        return *this;
    }
    bool operator<(const ValueDurationFloatPair &p) const {
        return value < p.value;
    }
};

static double toSec(const RealTime &r)
{
    return r.sec + double(r.nsec) / 1000000000.0;
}

void
PluginSummarisingAdapter::Impl::reduce()
{
    for (OutputSegmentAccumulatorMap::iterator i =
             m_segmentedAccumulators.begin();
         i != m_segmentedAccumulators.end(); ++i) {

        int output = i->first;
        SegmentAccumulatorMap &segments = i->second;

        for (SegmentAccumulatorMap::iterator j = segments.begin();
             j != segments.end(); ++j) {

            RealTime segmentStart = j->first;
            OutputAccumulator &accumulator = j->second;

            int sz = accumulator.results.size();

#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER
            std::cerr << "reduce: segment starting at " << segmentStart
                      << " on output " << output << " has " << sz << " result(s)" << std::endl;
#endif

            double totalDuration = 0.0;
            //!!! is this right?
            if (sz > 0) {
#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER
                std::cerr << "last time = " << accumulator.results[sz-1].time 
                          << ", duration = " << accumulator.results[sz-1].duration
                          << " (step = " << m_stepSize << ", block = " << m_blockSize << ")"
                          << std::endl;
#endif
                totalDuration = toSec((accumulator.results[sz-1].time +
                                       accumulator.results[sz-1].duration) -
                                      segmentStart);
            }

            for (int bin = 0; bin < accumulator.bins; ++bin) {

                // work on all values over time for a single bin

                OutputBinSummary summary;

                summary.count = sz;

                summary.minimum = 0.f;
                summary.maximum = 0.f;

                summary.median = 0.f;
                summary.mode = 0.f;
                summary.sum = 0.f;
                summary.variance = 0.f;

                summary.median_c = 0.f;
                summary.mode_c = 0.f;
                summary.mean_c = 0.f;
                summary.variance_c = 0.f;

                if (sz == 0) continue;

                std::vector<ValueDurationFloatPair> valvec;

                for (int k = 0; k < sz; ++k) {
                    while (int(accumulator.results[k].values.size()) <
                           accumulator.bins) {
                        accumulator.results[k].values.push_back(0.f);
                    }
                }

                for (int k = 0; k < sz; ++k) {
                    float value = accumulator.results[k].values[bin];
                    valvec.push_back(ValueDurationFloatPair
                                     (value,
                                      toSec(accumulator.results[k].duration)));
                }

                std::sort(valvec.begin(), valvec.end());

                summary.minimum = valvec[0].value;
                summary.maximum = valvec[sz-1].value;

#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER
                std::cerr << "total duration = " << totalDuration << std::endl;
#endif

#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER
/*
                std::cerr << "value vector for medians:" << std::endl;
                for (int k = 0; k < sz; ++k) {
                    std::cerr << "(" << valvec[k].value << "," << valvec[k].duration << ") ";
                }
                std::cerr << std::endl;
*/
#endif

                if (sz % 2 == 1) {
                    summary.median = valvec[sz/2].value;
                } else {
                    summary.median = (valvec[sz/2].value + valvec[sz/2 + 1].value) / 2;
                }
            
                double duracc = 0.0;
                summary.median_c = valvec[sz-1].value;

                for (int k = 0; k < sz; ++k) {
                    duracc += valvec[k].duration;
                    if (duracc > totalDuration/2) {
                        summary.median_c = valvec[k].value;
                        break;
                    }
                }

#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER
                std::cerr << "median_c = " << summary.median_c << std::endl;
                std::cerr << "median = " << summary.median << std::endl;
#endif
                
                std::map<float, int> distribution;

                for (int k = 0; k < sz; ++k) {
                    summary.sum += accumulator.results[k].values[bin];
                    distribution[accumulator.results[k].values[bin]] += 1;
                }

                int md = 0;

                for (std::map<float, int>::iterator di = distribution.begin();
                     di != distribution.end(); ++di) {
                    if (di->second > md) {
                        md = di->second;
                        summary.mode = di->first;
                    }
                }

                distribution.clear();

                std::map<float, double> distribution_c;

                for (int k = 0; k < sz; ++k) {
                    distribution_c[accumulator.results[k].values[bin]]
                        += toSec(accumulator.results[k].duration);
                }

                double mrd = 0.0;

                for (std::map<float, double>::iterator di = distribution_c.begin();
                     di != distribution_c.end(); ++di) {
                    if (di->second > mrd) {
                        mrd = di->second;
                        summary.mode_c = di->first;
                    }
                }

                distribution_c.clear();

                if (totalDuration > 0.0) {

                    double sum_c = 0.0;

                    for (int k = 0; k < sz; ++k) {
                        double value = accumulator.results[k].values[bin]
                            * toSec(accumulator.results[k].duration);
                        sum_c += value;
                    }

#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER
                    std::cerr << "mean_c = " << sum_c << " / " << totalDuration << " = "
                              << sum_c / totalDuration << " (sz = " << sz << ")" << std::endl;
#endif
                
                    summary.mean_c = sum_c / totalDuration;

                    for (int k = 0; k < sz; ++k) {
                        double value = accumulator.results[k].values[bin];
//                            * toSec(accumulator.results[k].duration);
                        summary.variance_c +=
                            (value - summary.mean_c) * (value - summary.mean_c)
                            * toSec(accumulator.results[k].duration);
                    }

//                    summary.variance_c /= summary.count;
                    summary.variance_c /= totalDuration;
                }

                double mean = summary.sum / summary.count;

#ifdef DEBUG_PLUGIN_SUMMARISING_ADAPTER
                std::cerr << "mean = " << summary.sum << " / " << summary.count << " = "
                          << summary.sum / summary.count << std::endl;
#endif

                for (int k = 0; k < sz; ++k) {
                    float value = accumulator.results[k].values[bin];
                    summary.variance += (value - mean) * (value - mean);
                }
                summary.variance /= summary.count;

                m_summaries[output][segmentStart][bin] = summary;
            }
        }
    }

    m_segmentedAccumulators.clear();
    m_accumulators.clear();
}


}

}

_VAMP_SDK_HOSTSPACE_END(PluginSummarisingAdapter.cpp)

