#include "audacityproject.h"

#include <cstdlib>

#include "au3wrap/audacity3project.h"

#include "log.h"

using namespace mu;
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

IAudacityProjectPtr Audacity4Project::makeMock()
{
    Audacity4Project* a = new Audacity4Project();

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

Audacity4Project::Audacity4Project()
{
    m_processingProject = std::make_shared<ProcessingProject>();
}

mu::Ret Audacity4Project::load(const mu::io::path_t& path, bool forceMode, const std::string& format_)
{
    TRACEFUNC;

    std::string format = format_.empty() ? io::suffix(path) : format_;

    LOGD() << "try load: " << path << ", format: " << format;

    //setupProject();
    setPath(path);

    //! TODO AU4
    // if (!isAudacityFile(format)) {
    //     Ret ret = doImport(path, forceMode);
    //     if (ret) {
    //         listenIfNeedSaveChanges();
    //     }

    //     return ret;
    // }

    Ret ret = doLoad(path, forceMode, format);
    if (!ret) {
        LOGE() << "failed load, err: " << ret.toString();
        return ret;
    }

    //! TODO AU4
    // listenIfNeedSaveChanges();

    return ret;
}

mu::Ret Audacity4Project::doLoad(const io::path_t& path, bool forceMode, const std::string& format)
{
    TRACEFUNC;

    UNUSED(forceMode);
    UNUSED(format);

    m_au3Project = au3::Audacity3Project::create();
    bool isLoaded = m_au3Project->load(path);
    if (!isLoaded) {
        LOGE() << "Failed load:" << path;
        return mu::make_ret(mu::Ret::Code::UnknownError);
    }

    LOGI() << "success loaded: " << m_au3Project->title();

    return mu::make_ret(Ret::Code::Ok);
}

void Audacity4Project::setPath(const io::path_t& path)
{
    if (m_path == path) {
        return;
    }

    m_path = path;
    m_pathChanged.notify();
}

const ProcessingProjectPtr Audacity4Project::processingProject() const
{
    return m_processingProject;
}
