#include "audacityproject.h"

#include <cstdlib>

using namespace au::project;
using namespace au::processing;

// just for test at the moment
static double SAMPLE_RATE = 44100.0;

static int myrand(int min, int max)
{
    static const double fraction = 1.0 / (static_cast<double>(RAND_MAX) + 1.0);
    return static_cast<int>(std::rand() * fraction * (max - min + 1) + min);
}

static void sinus(double freq, double intensity, uint32_t sampleNumber, std::vector<int16_t>& out)
{
    double value = 0;
    const double sample = ((M_PI * 2) / SAMPLE_RATE) * freq;
    out.resize(sampleNumber);

    for (uint32_t i = 0; i < sampleNumber; i++) {
        value += sample;
        if (value > 2 * M_PI) {
            value -= 2 * M_PI;
        }

        out[i] = (32767 * myrand(0, intensity * 100) / 100.0 * sin(value));
    }
}

IAudacityProjectPtr AudacityProject::makeMock()
{
    AudacityProject* a = new AudacityProject();

    ProcessingProjectPtr proc = a->m_processingProject;

    TrackList tracks;

    // track1
    {
        std::vector<int16_t> data;
        Clips clips;
        Clip c1;
        sinus(440, 1, SAMPLE_RATE * 10, data);
        Wave w1(data.data(), data.size());
        c1.setWave(w1);
        clips.push_back(c1);

        Clip c2;
        sinus(880, 1, SAMPLE_RATE * 10, data);
        Wave w2(data.data(), data.size());
        c2.setWave(w2);
        clips.push_back(c1);

        Track t;
        t.setClips(clips);
        tracks.push_back(t);
    }

    // track2
    {
        std::vector<int16_t> data;
        Clips clips;
        Clip c1;
        sinus(440, 1, SAMPLE_RATE * 10, data);
        Wave w1(data.data(), data.size());
        c1.setWave(w1);
        clips.push_back(c1);

        Clip c2;
        sinus(880, 1, SAMPLE_RATE * 10, data);
        Wave w2(data.data(), data.size());
        c2.setWave(w2);
        clips.push_back(c1);

        Track t;
        t.setClips(clips);
        tracks.push_back(t);
    }

    proc->setTrackList(tracks);

    return std::shared_ptr<IAudacityProject>(a);
}

AudacityProject::AudacityProject()
{
    m_processingProject = std::make_shared<ProcessingProject>();
}

const ProcessingProjectPtr AudacityProject::processingProject() const
{
    return m_processingProject;
}
