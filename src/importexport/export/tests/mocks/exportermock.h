/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "importexport/export/iexporter.h"

namespace au::importexport {
class ExporterMock : public IExporter
{
public:
    MOCK_METHOD(void, init, (), (override));
    MOCK_METHOD(muse::Ret, exportData, (const muse::io::path_t&, const Options&, muse::ProgressPtr, au::project::IAudacityProjectPtr),
                (override));

    MOCK_METHOD(muse::Ret, prepareSeparateFiles, (const Options&), (override));
    MOCK_METHOD(std::vector<std::string>, separateFileNames, (), (const, override));
    MOCK_METHOD(muse::Ret, exportSeparateFiles, (const muse::io::path_t&, muse::ProgressPtr), (override));

    MOCK_METHOD(std::vector<std::string>, formatsList, (), (const, override));
    MOCK_METHOD(int, formatIndex, (const std::string&), (const, override));
    MOCK_METHOD(std::vector<std::string>, formatExtensions, (const std::string&), (const, override));
    MOCK_METHOD(std::vector<std::string>, cloudPreferredAudioFormats, (bool), (const, override));
    MOCK_METHOD(ExportParameters, cloudExportParameters, (const std::string&), (const, override));
    MOCK_METHOD(muse::RetVal<ExportParameters>, exportParameters, (const ExportFormatSettings&, int), (const, override));
    MOCK_METHOD(int, defaultSampleRate, (const ExportFormatSettings&), (const, override));
    MOCK_METHOD(std::optional<std::vector<int> >, supportedSampleRates, (const ExportFormatSettings&), (const, override));
    MOCK_METHOD(std::optional<std::string>, optionLabel, (const ExportOptionValue&), (const, override));
    MOCK_METHOD(bool, isCustomFFmpegExportFormat, (), (const, override));
    MOCK_METHOD(bool, hasMetadata, (), (const, override));

    MOCK_METHOD(int, maxChannels, (), (const, override));
    MOCK_METHOD(std::vector<int>, sampleRateList, (), (const, override));
    MOCK_METHOD(int, projectSampleRate, (), (const, override));
    MOCK_METHOD(int, optionsCount, (), (const, override));

    MOCK_METHOD(std::optional<ExportOption>, option, (int), (const, override));
    MOCK_METHOD(std::optional<OptionValue>, value, (int), (const, override));
    MOCK_METHOD(void, setValue, (int, const OptionValue&), (override));
};
}
