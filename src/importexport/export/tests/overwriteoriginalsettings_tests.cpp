#include <gtest/gtest.h>

#include <map>
#include <string>
#include <tuple>
#include <utility>
#include <variant>
#include <vector>

#include <QVariantMap>

#include "importexport/export/overwriteoriginalsettings.h"

#include "au3-import-export/ExportOptionsEditor.h"
#include "au3-strings/TranslatableString.h"

namespace {
class FakeExportOptionsEditor final : public ExportOptionsEditor
{
public:
    FakeExportOptionsEditor(std::string name, std::vector<ExportOption> options)
        : m_name(std::move(name)), m_options(std::move(options))
    {
        for (const auto& option : m_options) {
            m_values[option.id] = option.defaultValue;
        }
    }

    std::string GetName() const override
    {
        return m_name;
    }

    int GetOptionsCount() const override
    {
        return static_cast<int>(m_options.size());
    }

    bool GetOption(int index, ExportOption& option) const override
    {
        if (index < 0 || index >= static_cast<int>(m_options.size())) {
            return false;
        }

        option = m_options[index];
        return true;
    }

    bool GetValue(ExportOptionID id, ExportValue& value) const override
    {
        const auto it = m_values.find(id);
        if (it == m_values.end()) {
            return false;
        }

        value = it->second;
        return true;
    }

    bool SetValue(ExportOptionID id, const ExportValue& value) override
    {
        if (m_values.find(id) == m_values.end()) {
            return false;
        }

        m_values[id] = value;
        const auto str = std::get_if<std::string>(&value);
        if (str && *str == "VBR") {
            setHidden(1, true);
            setHidden(2, false);
            setHidden(3, true);
            setHidden(4, true);
        }

        return true;
    }

    SampleRateList GetSampleRateList() const override
    {
        return {};
    }

    void Store(audacity::BasicSettings&) const override {}
    void Load(const audacity::BasicSettings&) override {}

    template<typename T>
    T value(ExportOptionID id) const
    {
        return std::get<T>(m_values.at(id));
    }

private:
    void setHidden(ExportOptionID id, bool hidden)
    {
        for (auto& option : m_options) {
            if (option.id != id) {
                continue;
            }

            if (hidden) {
                option.flags |= ExportOption::Hidden;
            } else {
                option.flags &= ~ExportOption::Hidden;
            }
        }
    }

    std::string m_name;
    std::vector<ExportOption> m_options;
    std::map<ExportOptionID, ExportValue> m_values;
};

ExportOption option(
    ExportOptionID id, const char* title, ExportValue defaultValue, int flags, std::vector<ExportValue> values)
{
    return {
        id,
        TranslatableString("import-export", title),
        std::move(defaultValue),
        flags,
        std::move(values),
        {}
    };
}

FakeExportOptionsEditor makeMp3Editor()
{
    return FakeExportOptionsEditor("mp3", {
        option(0, "Bit Rate Mode", std::string("SET"), ExportOption::TypeEnum,
               { std::string("SET"), std::string("VBR"), std::string("ABR"), std::string("CBR") }),
        option(1, "Quality", 2, ExportOption::TypeEnum, { 0, 1, 2, 3 }),
        option(2, "Quality", 2, ExportOption::TypeEnum | ExportOption::Hidden, { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }),
        option(3, "Quality", 192, ExportOption::TypeEnum | ExportOption::Hidden, { 320, 256, 224, 192, 160, 144, 128 }),
        option(4, "Quality", 192, ExportOption::TypeEnum | ExportOption::Hidden, { 320, 256, 224, 192, 160, 144, 128 }),
    });
}

FakeExportOptionsEditor makeFlacEditor()
{
    return FakeExportOptionsEditor("plain", {
        option(0, "Bit Depth", std::string("16"), ExportOption::TypeEnum, { std::string("16"), std::string("24") }),
        option(1, "Level", std::string("5"), ExportOption::TypeEnum,
               { std::string("0"), std::string("1"), std::string("2"), std::string("3"), std::string("4"),
                 std::string("5"), std::string("6"), std::string("7"), std::string("8") }),
    });
}

FakeExportOptionsEditor makePcmEditor()
{
    return FakeExportOptionsEditor("plain", {
        option(0, "Bit Depth", 16, ExportOption::TypeEnum, { 16, 24, 32 }),
    });
}

FakeExportOptionsEditor makeBitRateEditor()
{
    return FakeExportOptionsEditor("plain", {
        option(0, "Bit Rate", 128, ExportOption::TypeEnum, { 96, 128, 160, 192, 224, 256, 320 }),
    });
}

QVariantMap codecSettingsWithBitRate(double bitRate)
{
    QVariantMap settings;
    settings.insert("bitRate", bitRate);
    return settings;
}

QVariantMap codecSettingsWithBitDepth(int bitDepth)
{
    QVariantMap settings;
    settings.insert("bitDepth", bitDepth);
    return settings;
}
}

class Mp3VbrPresetTests : public ::testing::TestWithParam<std::tuple<double, int> >
{
};

class PcmBitDepthTests : public ::testing::TestWithParam<int>
{
};

class GenericBitRateTests : public ::testing::TestWithParam<std::tuple<double, int> >
{
};

TEST_P(Mp3VbrPresetTests, MapsEstimatedBitRateToConfiguredPreset)
{
    const auto [bitRate, expectedPreset] = GetParam();
    const auto preset = au::importexport::Mp3VbrPresetForBitRate(bitRate);

    ASSERT_TRUE(preset.has_value());
    EXPECT_EQ(*preset, expectedPreset);
}

INSTANTIATE_TEST_SUITE_P(
    OverwriteOriginalSettingsTests,
    Mp3VbrPresetTests,
    ::testing::Values(
        std::make_tuple(320000.0, 0),
        std::make_tuple(230000.0, 0),
        std::make_tuple(229000.0, 1),
        std::make_tuple(200000.0, 1),
        std::make_tuple(199000.0, 2),
        std::make_tuple(180000.0, 2),
        std::make_tuple(179000.0, 3),
        std::make_tuple(170000.0, 3),
        std::make_tuple(169000.0, 4),
        std::make_tuple(140000.0, 4),
        std::make_tuple(139000.0, 5),
        std::make_tuple(120000.0, 5),
        std::make_tuple(119000.0, 6),
        std::make_tuple(96.0, 6)));

TEST(OverwriteOriginalSettingsTests, AppliesMp3BitRateAsVbrModeAndPreset)
{
    auto editor = makeMp3Editor();

    au::importexport::ApplyCodecSettingsToExportOptions(editor, codecSettingsWithBitRate(179000.0));

    EXPECT_EQ(editor.value<std::string>(0), "VBR");
    EXPECT_EQ(editor.value<int>(2), 3);
}

TEST(OverwriteOriginalSettingsTests, AppliesFlacStringBitDepthValues)
{
    auto editor = makeFlacEditor();

    au::importexport::ApplyCodecSettingsToExportOptions(editor, codecSettingsWithBitDepth(24));

    EXPECT_EQ(editor.value<std::string>(0), "24");
    EXPECT_EQ(editor.value<std::string>(1), "5");
}

TEST(OverwriteOriginalSettingsTests, AppliesFlacSixteenBitDepth)
{
    auto editor = makeFlacEditor();
    editor.SetValue(0, std::string("24"));

    au::importexport::ApplyCodecSettingsToExportOptions(editor, codecSettingsWithBitDepth(16));

    EXPECT_EQ(editor.value<std::string>(0), "16");
}

TEST_P(PcmBitDepthTests, AppliesNumericPcmBitDepthValues)
{
    const auto bitDepth = GetParam();
    auto editor = makePcmEditor();
    editor.SetValue(0, 16);

    au::importexport::ApplyCodecSettingsToExportOptions(editor, codecSettingsWithBitDepth(bitDepth));

    EXPECT_EQ(editor.value<int>(0), bitDepth);
}

INSTANTIATE_TEST_SUITE_P(
    OverwriteOriginalSettingsTests,
    PcmBitDepthTests,
    ::testing::Values(16, 24, 32));

TEST_P(GenericBitRateTests, AppliesGenericBitRateToClosestKbpsOption)
{
    const auto [sourceBitRate, expectedOptionValue] = GetParam();
    auto editor = makeBitRateEditor();

    au::importexport::ApplyCodecSettingsToExportOptions(editor, codecSettingsWithBitRate(sourceBitRate));

    EXPECT_EQ(editor.value<int>(0), expectedOptionValue);
}

INSTANTIATE_TEST_SUITE_P(
    OverwriteOriginalSettingsTests,
    GenericBitRateTests,
    ::testing::Values(
        std::make_tuple(96000.0, 96),
        std::make_tuple(128.0, 128),
        std::make_tuple(179000.0, 192),
        std::make_tuple(229000.0, 224),
        std::make_tuple(320000.0, 320)));

TEST(OverwriteOriginalSettingsTests, DoesNotTreatLowOggStyleQualityValuesAsBitRateTargets)
{
    FakeExportOptionsEditor editor("plain", {
        option(0, "Quality", 5, ExportOption::TypeEnum, { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 }),
    });

    au::importexport::ApplyCodecSettingsToExportOptions(editor, codecSettingsWithBitRate(96000.0));

    EXPECT_EQ(editor.value<int>(0), 5);
}
