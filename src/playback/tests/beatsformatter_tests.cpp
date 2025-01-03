/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../view/toolbars/components/beatsformatter.h"

namespace au::playback {

struct BeatsFormatterTestParam {
    QString format;
    int fracPart;
    BeatsFormatterMode mode;
    double inputValue;
    std::string expected;
};

// Test fixture for parameterized tests
class BeatsFormatterParameterizedTests : public ::testing::TestWithParam<BeatsFormatterTestParam> {
protected:
    void SetUp() override {
        const auto& param = GetParam();
        const BeatsFormatterMode mode = param.mode;
        m_formatter = std::make_unique<BeatsFormatter>(param.format, param.fracPart, mode);
        setupFormatter();
    }

    void setupFormatter()
    {
        m_formatter->setSampleRate(44100.0);
        m_formatter->setTempo(120);
        m_formatter->setUpperTimeSignature(3);
        m_formatter->setLowerTimeSignature(4);
        m_formatter->init();
    }

    std::unique_ptr<BeatsFormatter> m_formatter;
};

TEST_P(BeatsFormatterParameterizedTests, ValueToStringProducesExpectedOutput) {
    const auto& param = GetParam();
    auto result = m_formatter->valueToString(param.inputValue, true);
    EXPECT_EQ(result.valueString, QString::fromStdString(param.expected));
}

INSTANTIATE_TEST_SUITE_P(
    AllBeatsFormatterTests,
    BeatsFormatterParameterizedTests,
    ::testing::Values(
        BeatsFormatterTestParam{ "bar:beat", 0, BeatsFormatterMode::TimePoint, 0.0, "001 bar 01 beat" },
        BeatsFormatterTestParam{ "bar:beat", 0, BeatsFormatterMode::TimePoint, 0.6, "001 bar 02 beat" },
        BeatsFormatterTestParam{ "bar:beat", 0, BeatsFormatterMode::TimePoint, 1.0, "001 bar 03 beat" },
        BeatsFormatterTestParam{ "bar:beat", 0, BeatsFormatterMode::TimePoint, 1.6, "002 bar 01 beat" },
        BeatsFormatterTestParam{ "bar:beat", 0, BeatsFormatterMode::Duration, 0.0, "000 bar 00 beat" },
        BeatsFormatterTestParam{ "bar:beat", 0, BeatsFormatterMode::Duration, 0.6, "000 bar 01 beat" },
        BeatsFormatterTestParam{ "bar:beat", 0, BeatsFormatterMode::Duration, 1.0, "000 bar 02 beat" },
        BeatsFormatterTestParam{ "bar:beat", 0, BeatsFormatterMode::Duration, 1.6, "001 bar 00 beat" },
        BeatsFormatterTestParam{ "bar:beat:tick", 16, BeatsFormatterMode::TimePoint, -1.0, "--- bar -- beat --" },
        BeatsFormatterTestParam{ "bar:beat:tick", 16, BeatsFormatterMode::TimePoint, 0.0, "001 bar 01 beat 01" },
        BeatsFormatterTestParam{ "bar:beat:tick", 16, BeatsFormatterMode::TimePoint, 0.6, "001 bar 02 beat 01" },
        BeatsFormatterTestParam{ "bar:beat:tick", 16, BeatsFormatterMode::TimePoint, 1.0, "001 bar 03 beat 01" },
        BeatsFormatterTestParam{ "bar:beat:tick", 16, BeatsFormatterMode::TimePoint, 1.9, "002 bar 01 beat 04" },
        BeatsFormatterTestParam{ "bar:beat:tick", 16, BeatsFormatterMode::Duration, -1.0, "--- bar -- beat --" },
        BeatsFormatterTestParam{ "bar:beat:tick", 16, BeatsFormatterMode::Duration, 0.0, "000 bar 00 beat 00" },
        BeatsFormatterTestParam{ "bar:beat:tick", 16, BeatsFormatterMode::Duration, 0.6, "000 bar 01 beat 00" },
        BeatsFormatterTestParam{ "bar:beat:tick", 16, BeatsFormatterMode::Duration, 1.0, "000 bar 02 beat 00" },
        BeatsFormatterTestParam{ "bar:beat:tick", 16, BeatsFormatterMode::Duration, 1.9, "001 bar 00 beat 03" }
    )
);

}