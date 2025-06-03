#pragma once

#include <functional>
#include <memory>
#include <string>

#include "au3wrap/au3types.h"

namespace au::trackedit {
using ClipDataGeneratorFunc = std::function<std::vector<float>(double duration, double sampleRate)>;

struct ClipTemplate {
    double startPosition;
    std::vector<std::tuple<double, ClipDataGeneratorFunc> > dataGenerator;
};

class TrackTemplateFactory
{
public:
    TrackTemplateFactory(au::au3::Au3Project& project, double sampleRate);
    std::shared_ptr<WaveTrack> createTrackFromTemplate(const std::string& name, const std::vector<ClipTemplate>& clipTemplates);
    au::au3::Au3TrackId addTrackToProject(std::shared_ptr<WaveTrack> track);
    au::au3::Au3TrackId addTrackFromTemplate(const std::string& name, const std::vector<ClipTemplate>& clipTemplates);

    static std::vector<float> createNoise(double duration, double sampleRate);
    static std::vector<float> createSilence(double duration, double sampleRate);

private:
    au::au3::Au3Project& m_project;
    double m_sampleRate;
};
}
