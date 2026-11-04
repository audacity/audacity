/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <QLocale>
#include <QTimeZone>

#include "../internal/timedrecordingformatter.h"
#include "../qml/Audacity/Record/timedrecordingviewformatter.h"

namespace au::record {
namespace {
constexpr qint64 MINUTE = 60;
constexpr qint64 HOUR = 60 * MINUTE;
constexpr qint64 DAY = 24 * HOUR;

QDateTime at(int year, int month, int day, int hour, int minute, int second = 0)
{
    return QDateTime(QDate(year, month, day), QTime(hour, minute, second), QTimeZone::utc());
}
}

class TimedRecordingFormatterTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_previousLocale = QLocale();
        QLocale::setDefault(QLocale(QLocale::English, QLocale::UnitedKingdom));
    }

    void TearDown() override
    {
        QLocale::setDefault(m_previousLocale);
    }

private:
    QLocale m_previousLocale;
};

TEST_F(TimedRecordingFormatterTests, DurationLeavesOutZeroUnitsWithoutRounding)
{
    using namespace std::chrono_literals;

    EXPECT_EQ(TimedRecordingFormatter::duration(0s).toStdString(), "0s");
    EXPECT_EQ(TimedRecordingFormatter::duration(45s).toStdString(), "45s");
    EXPECT_EQ(TimedRecordingFormatter::duration(5min).toStdString(), "5m");
    EXPECT_EQ(TimedRecordingFormatter::duration(43min + 30s).toStdString(), "43m 30s");
    EXPECT_EQ(TimedRecordingFormatter::duration(1h).toStdString(), "1h");
    EXPECT_EQ(TimedRecordingFormatter::duration(1h + 30min).toStdString(), "1h 30m");
    EXPECT_EQ(TimedRecordingFormatter::duration(17h + 36min + 30s).toStdString(), "17h 36m 30s");
    EXPECT_EQ(TimedRecordingFormatter::duration(48h).toStdString(), "2d");
    EXPECT_EQ(TimedRecordingFormatter::duration(24h + 5s).toStdString(), "1d 5s");
}

TEST_F(TimedRecordingFormatterTests, ClockShowsHoursOnlyWhenThereAreSome)
{
    EXPECT_EQ(TimedRecordingViewFormatter::clock(0), "00:00");
    EXPECT_EQ(TimedRecordingViewFormatter::clock(10), "00:10");
    EXPECT_EQ(TimedRecordingViewFormatter::clock(5 * MINUTE), "05:00");
    EXPECT_EQ(TimedRecordingViewFormatter::clock(59 * MINUTE + 55), "59:55");
    EXPECT_EQ(TimedRecordingViewFormatter::clock(HOUR), "01:00:00");
    EXPECT_EQ(TimedRecordingViewFormatter::clock(26 * HOUR + 5), "26:00:05");
}

TEST_F(TimedRecordingFormatterTests, DateTimeHasWeekdayDateYearAndTime)
{
    EXPECT_EQ(TimedRecordingViewFormatter::dateTime(at(2026, 10, 24, 12, 23, 30)), "Sat 24 Oct 2026 at 12:23:30");
}

TEST_F(TimedRecordingFormatterTests, StartingNowForADurationGivesTheEndTime)
{
    const QDateTime now = at(2026, 10, 24, 15, 28, 20);

    EXPECT_EQ(TimedRecordingViewFormatter::summary(now, std::nullopt, HOUR, true),
              "Starts when you press Start timed recording and ends today at 16:28:20 (1h).");
    EXPECT_EQ(TimedRecordingViewFormatter::summary(now, std::nullopt, 9 * HOUR, true),
              "Starts when you press Start timed recording and ends tomorrow at 00:28:20 (9h).");
    EXPECT_EQ(TimedRecordingViewFormatter::summary(now, std::nullopt, 2 * DAY, true),
              "Starts when you press Start timed recording and ends on Mon 26 Oct 2026 at 15:28:20 (2d).");
}

TEST_F(TimedRecordingFormatterTests, StartingNowUntilATimeGivesTheDuration)
{
    const QDateTime now = at(2026, 10, 24, 10, 0);

    EXPECT_EQ(TimedRecordingViewFormatter::summary(now, std::nullopt, 2 * HOUR, false),
              "Starts when you press Start timed recording and records for 2h.");
}

TEST_F(TimedRecordingFormatterTests, StartUnderAnHourAwayIsAnExactCountdown)
{
    EXPECT_EQ(TimedRecordingViewFormatter::summary(at(2026, 10, 24, 11, 40), at(2026, 10, 24, 12, 23, 30), 2 * DAY, false),
              "Starts in 43m 30s and records for 2d.");
    EXPECT_EQ(TimedRecordingViewFormatter::summary(at(2026, 10, 24, 12, 22, 45), at(2026, 10, 24, 12, 23, 30), HOUR, false),
              "Starts in 45s and records for 1h.");
    EXPECT_EQ(TimedRecordingViewFormatter::summary(at(2026, 10, 24, 23, 50), at(2026, 10, 25, 0, 20), HOUR, false),
              "Starts in 30m and records for 1h.");
}

TEST_F(TimedRecordingFormatterTests, StartLaterTodayGivesTheClockTime)
{
    EXPECT_EQ(TimedRecordingViewFormatter::summary(at(2026, 10, 24, 10, 0), at(2026, 10, 24, 12, 23, 30), 5 * MINUTE, false),
              "Starts today at 12:23:30 and records for 5m.");
}

TEST_F(TimedRecordingFormatterTests, TomorrowIsTheNextDateNotTheNext24Hours)
{
    EXPECT_EQ(TimedRecordingViewFormatter::summary(at(2026, 10, 23, 10, 0), at(2026, 10, 24, 12, 23, 30), HOUR + 30 * MINUTE, false),
              "Starts tomorrow at 12:23:30 and records for 1h 30m.");
    EXPECT_EQ(TimedRecordingViewFormatter::summary(at(2026, 10, 23, 23, 0), at(2026, 10, 24, 1, 0), HOUR, false),
              "Starts tomorrow at 01:00:00 and records for 1h.");
}

TEST_F(TimedRecordingFormatterTests, StartBeyondTomorrowGivesTheFullDate)
{
    EXPECT_EQ(TimedRecordingViewFormatter::summary(at(2026, 10, 22, 10, 0), at(2026, 10, 24, 12, 23, 30), 17 * HOUR + 36 * MINUTE + 30,
                                                   false),
              "Starts on Sat 24 Oct 2026 at 12:23:30 and records for 17h 36m 30s.");
    EXPECT_EQ(TimedRecordingViewFormatter::summary(at(2026, 10, 22, 10, 0), at(2026, 11, 22, 10, 0), 2 * HOUR + 30 * MINUTE, false),
              "Starts on Sun 22 Nov 2026 at 10:00:00 and records for 2h 30m.");
}

TEST_F(TimedRecordingFormatterTests, StartInThePastStartsWhenConfirmed)
{
    EXPECT_EQ(TimedRecordingViewFormatter::summary(at(2026, 10, 24, 12, 0), at(2026, 10, 24, 11, 0), HOUR, false),
              "Starts when you press Start timed recording and records for 1h.");
}

TEST_F(TimedRecordingFormatterTests, PassedTimesAskForATimeAfterNow)
{
    EXPECT_EQ(TimedRecordingViewFormatter::startTimePassedError(at(2026, 10, 24, 12, 25, 14)),
              "Start time has passed. Choose a time after 12:25:14.");
    EXPECT_EQ(TimedRecordingViewFormatter::endTimePassedError(at(2026, 10, 24, 12, 25, 14)),
              "End time has passed. Choose a time after 12:25:14.");
}
}
