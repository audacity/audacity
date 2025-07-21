/*
* Audacity: A Digital Audio Editor
*/

#include "exportconfiguration.h"

#include "global/settings.h"

#include "log.h"

using namespace au::importexport;

static const std::string module_name("export");

static const muse::Settings::Key EXPORT_PROCESS(module_name, "importexport/process");
static const muse::Settings::Key EXPORT_DIRECTORY_PATH(module_name, "importexport/directoryPath");
static const muse::Settings::Key EXPORT_FORMAT(module_name, "importexport/format");
static const muse::Settings::Key EXPORT_CHANNELS(module_name, "importexport/channels");
static const muse::Settings::Key EXPORT_SAMPLE_RATE(module_name, "importexport/defaultProjectSampleRate");
static const muse::Settings::Key EXPORT_ENCODING(module_name, "importexport/encoding");

void ExportConfiguration::init()
{
    muse::settings()->setDefaultValue(EXPORT_PROCESS, muse::Val(ExportProcessType::FULL_PROJECT_AUDIO));
    muse::settings()->valueChanged(EXPORT_PROCESS).onReceive(nullptr, [this] (const muse::Val& val) {
        m_processChanged.notify();
    });

    muse::settings()->valueChanged(EXPORT_DIRECTORY_PATH).onReceive(nullptr, [this] (const muse::Val& val) {
        m_directoryPathChanged.notify();
    });

    muse::settings()->valueChanged(EXPORT_FORMAT).onReceive(nullptr, [this] (const muse::Val& val) {
        m_currentFormatChanged.notify();
    });

    muse::settings()->setDefaultValue(EXPORT_CHANNELS, muse::Val(ExportChannelsPref::ExportChannels::STEREO));
    muse::settings()->valueChanged(EXPORT_CHANNELS).onReceive(nullptr, [this] (const muse::Val& val) {
        m_exportChannelsChanged.notify();
    });

    muse::settings()->setDefaultValue(EXPORT_SAMPLE_RATE, muse::Val("44100"));
    muse::settings()->valueChanged(EXPORT_SAMPLE_RATE).onReceive(nullptr, [this] (const muse::Val& val) {
        m_exportSampleRateChanged.notify();
    });
}

ExportProcessType ExportConfiguration::processType() const
{
    return muse::settings()->value(EXPORT_PROCESS).toEnum<ExportProcessType>();
}

void ExportConfiguration::setProcessType(ExportProcessType process)
{
    muse::settings()->setSharedValue(EXPORT_PROCESS, muse::Val(process));
}

muse::async::Notification ExportConfiguration::processTypeChanged() const
{
    return m_processChanged;
}

muse::io::path_t ExportConfiguration::directoryPath() const
{
    return muse::settings()->value(EXPORT_DIRECTORY_PATH).toString();
}

void ExportConfiguration::setDirectoryPath(const muse::io::path_t& path)
{
    muse::settings()->setSharedValue(EXPORT_DIRECTORY_PATH, muse::Val(path));
}

muse::async::Notification ExportConfiguration::directoryPathChanged() const
{
    return m_directoryPathChanged;
}

int ExportConfiguration::exportChannels() const
{
    return muse::settings()->value(EXPORT_CHANNELS).toInt();
}

void ExportConfiguration::setExportChannels(int channels)
{
    muse::settings()->setSharedValue(EXPORT_CHANNELS, muse::Val(channels));
}

muse::async::Notification ExportConfiguration::exportChannelsChanged() const
{
    return m_exportChannelsChanged;
}

std::string ExportConfiguration::currentFormat() const
{
    return muse::settings()->value(EXPORT_FORMAT).toString();
}

void ExportConfiguration::setCurrentFormat(const std::string& format)
{
    muse::settings()->setSharedValue(EXPORT_FORMAT, muse::Val(format));
}

muse::async::Notification ExportConfiguration::currentFormatChanged() const
{
    return m_currentFormatChanged;
}

int ExportConfiguration::exportSampleRate() const
{
    return muse::settings()->value(EXPORT_SAMPLE_RATE).toInt();
}

void ExportConfiguration::setExportSampleRate(int newRate)
{
    muse::settings()->setSharedValue(EXPORT_SAMPLE_RATE, muse::Val(static_cast<int>(newRate)));
}

muse::async::Notification ExportConfiguration::exportSampleRateChanged() const
{
    return m_exportSampleRateChanged;
}

std::vector<std::string> ExportConfiguration::exportSampleFormatList() const
{
    NOT_IMPLEMENTED;
    return {};
}

std::string ExportConfiguration::exportSampleFormat() const
{
    NOT_IMPLEMENTED;
    return {};
}

void ExportConfiguration::setExportSampleFormat(const std::string& format)
{
    Q_UNUSED(format);
    NOT_IMPLEMENTED;
}

muse::async::Notification ExportConfiguration::exportSampleFormatChanged() const
{
    NOT_IMPLEMENTED;
    return {};
}
