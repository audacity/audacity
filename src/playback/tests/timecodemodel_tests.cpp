/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../view/toolbars/components/timecodemodel.h"

namespace au::playback {
constexpr double SAMPLE_RATE = 44100.0;
constexpr double TEMPO = 120.0;
constexpr int UPPER_TIME_SIGNATURE = 3;
constexpr int LOWER_TIME_SIGNATURE = 4;
constexpr double BEAT_DURATION = 60.0 / TEMPO;
constexpr double BAR_DURATION = UPPER_TIME_SIGNATURE * BEAT_DURATION;
constexpr double TICK_DURATION = BEAT_DURATION / 4;

struct TimecodeModelTestParam {
    TimecodeModel::ViewFormatType format;
    TimecodeMode mode;
    double inputValue;
    QString expectedOutput;
};

class TimecodeModelParameterizedTests : public ::testing::TestWithParam<TimecodeModelTestParam>
{
protected:
    void SetUp() override
    {
        setupModel();
    }

    void setupModel()
    {
        m_model.setSampleRate(SAMPLE_RATE);
        m_model.setTempo(TEMPO);
        m_model.setUpperTimeSignature(UPPER_TIME_SIGNATURE);
        m_model.setLowerTimeSignature(LOWER_TIME_SIGNATURE);
    }

    TimecodeModel m_model;
};

TEST_P(TimecodeModelParameterizedTests, ParseValueToString) {
    const auto& param = GetParam();
    m_model.setCurrentFormat(static_cast<int>(param.format));
    m_model.setMode(param.mode);
    m_model.setValue(param.inputValue);
    EXPECT_EQ(param.expectedOutput, m_model.valueString());
}

INSTANTIATE_TEST_SUITE_P(
    AllTimecodeModelTests,
    TimecodeModelParameterizedTests,
    ::testing::Values(
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::HHMMSS, TimecodeMode::TimePoint, 0.0, "00 h 00 m 00 s" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::HHMMSS, TimecodeMode::TimePoint, 0.6, "00 h 00 m 01 s" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::HHMMSS, TimecodeMode::TimePoint, 1.0, "00 h 00 m 01 s" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::HHMMSS, TimecodeMode::TimePoint, 1.6, "00 h 00 m 02 s" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::HHMMSS, TimecodeMode::TimePoint, 60.0, "00 h 01 m 00 s" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::HHMMSS, TimecodeMode::TimePoint, 61.0, "00 h 01 m 01 s" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::HHMMSS, TimecodeMode::TimePoint, 3600.0, "01 h 00 m 00 s" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::HHMMSS, TimecodeMode::TimePoint, 3601.0, "01 h 00 m 01 s" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeat, TimecodeMode::TimePoint, 0.0, "001 bar 01 beat" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeat, TimecodeMode::TimePoint,
                                 (1.0 * BEAT_DURATION + 1.0 * TICK_DURATION), "001 bar 02 beat" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeat, TimecodeMode::TimePoint, (2.0 * BEAT_DURATION),
                                 "001 bar 03 beat" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeat, TimecodeMode::TimePoint,
                                 (1.0 * BAR_DURATION + 1.0 * TICK_DURATION), "002 bar 01 beat" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeat, TimecodeMode::Duration, 0.0, "000 bar 00 beat" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeat, TimecodeMode::Duration,
                                 (1.0 * BEAT_DURATION + 1.0 * TICK_DURATION), "000 bar 01 beat" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeat, TimecodeMode::Duration, (2.0 * BEAT_DURATION), "000 bar 02 beat" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeat, TimecodeMode::Duration, (1.0 * BAR_DURATION + 1.0 * TICK_DURATION),
                                 "001 bar 00 beat" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeatTick, TimecodeMode::TimePoint, -1.0, "--- bar -- beat --" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeatTick, TimecodeMode::TimePoint, 0.0, "001 bar 01 beat 01" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeatTick, TimecodeMode::TimePoint,
                                 (1.0 * BEAT_DURATION + 0.7 * TICK_DURATION), "001 bar 02 beat 01" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeatTick, TimecodeMode::TimePoint, (2.0 * BEAT_DURATION),
                                 "001 bar 03 beat 01" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeatTick, TimecodeMode::TimePoint,
                                 (1.0 * BAR_DURATION + 3.2 * TICK_DURATION), "002 bar 01 beat 04" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeatTick, TimecodeMode::Duration, -1.0, "--- bar -- beat --" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeatTick, TimecodeMode::Duration, 0.0, "000 bar 00 beat 00" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeatTick, TimecodeMode::Duration,
                                 (1.0 * BEAT_DURATION + 0.7 * TICK_DURATION), "000 bar 01 beat 00" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeatTick, TimecodeMode::Duration, (2.0 * BEAT_DURATION),
                                 "000 bar 02 beat 00" },
        TimecodeModelTestParam { TimecodeModel::ViewFormatType::BarBeatTick, TimecodeMode::Duration,
                                 (1.0 * BAR_DURATION + 3.2 * TICK_DURATION), "001 bar 00 beat 03" }
        )
    );

class TimecodeModelEditTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_model.setCurrentFormat(static_cast<int>(TimecodeModel::ViewFormatType::HHMMSS));
        m_model.setSampleRate(44100.0);
    }

    TimecodeModel m_model = TimecodeModel();
};

TEST_F(TimecodeModelEditTests, ValueIsModifiedWhenUpKeyIsPressed)
{
    //! [GIVEN] Current value is 59.0
    m_model.setValue(59.0);
    EXPECT_EQ(m_model.valueString(), "00 h 00 m 59 s");

    //! [GIVEN] Current edited field is 9 in 59 sec
    m_model.setCurrentEditedFieldIndex(11);

    //! [WHEN] Press up key
    QKeyEvent keyEvent(QEvent::KeyPress, Qt::Key_Up, Qt::NoModifier);
    qApp->sendEvent(qApp, &keyEvent);

    //! [THEN] Check value
    EXPECT_EQ(m_model.valueString(), "00 h 01 m 00 s");
}
}
