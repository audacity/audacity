#include "tracktemplatefactory.h"

#include "WaveTrack.h"
#include "WaveClip.h"

#include <tuple>
#include <random>

using namespace au::au3;

namespace au::trackedit {
TrackTemplateFactory::TrackTemplateFactory(au::au3::Au3Project& project, double sampleRate)
    : m_project(project)
    , m_sampleRate(sampleRate)
{}

std::shared_ptr<WaveTrack> TrackTemplateFactory::createTrackFromTemplate(const std::string& name,
                                                                         const std::vector<ClipTemplate>& clipTemplates)
{
    auto& trackFactory = Au3WaveTrackFactory::Get(m_project);
    auto track = trackFactory.Create(sampleFormat::floatSample, m_sampleRate);

    for (const auto& tmpl : clipTemplates) {
        std::vector<float> data;
        for (const auto& dataTmpl : tmpl.dataGenerator) {
            const auto duration = std::get<0>(dataTmpl);
            const auto fn = std::get<1>(dataTmpl);
            std::vector<float> segmentData = fn(duration, m_sampleRate);
            data.insert(data.end(), segmentData.begin(), segmentData.end());
        }

        constSamplePtr buffer = reinterpret_cast<constSamplePtr>(data.data());
        const auto clip = track->CreateClip(tmpl.startPosition, name);
        clip->Append(&buffer, sampleFormat::floatSample, data.size(), 1, sampleFormat::floatSample);
        clip->Flush();
        track->InsertInterval(clip, true);
    }

    return track;
}

au::au3::Au3TrackId TrackTemplateFactory::addTrackToProject(std::shared_ptr<WaveTrack> track)
{
    auto& trackList = Au3TrackList::Get(m_project);
    trackList.Add(track, ::TrackList::DoAssignId::Yes, ::TrackList::EventPublicationSynchrony::Synchronous);
    return track->GetId();
}

au::au3::Au3TrackId TrackTemplateFactory::addTrackFromTemplate(const std::string& name, const std::vector<ClipTemplate>& clipTemplates)
{
    auto track = createTrackFromTemplate(name, clipTemplates);
    return addTrackToProject(track);
}

std::vector<float> TrackTemplateFactory::createNoise(double duration, double sampleRate)
{
    std::vector<float> data(static_cast<size_t>(duration * sampleRate));
    std::default_random_engine generator;
    std::normal_distribution<float> distribution(0.0f, 1.0f);

    for (auto& sample : data) {
        sample = distribution(generator);
    }

    return data;
}

std::vector<float> TrackTemplateFactory::createSilence(double duration, double sampleRate)
{
    return std::vector<float>(static_cast<size_t>(duration * sampleRate), 0.0f);
}
}
