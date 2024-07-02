/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../view/toolbars/components/timecodemodel.h"

namespace au::playback {
class TimecodeModelTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_model = new TimecodeModel(nullptr);
    }

    QString currentValueString() const
    {
        return m_model->m_valueString;
    }

    void TearDown() override
    {
        delete m_model;
    }

protected:

    TimecodeModel* m_model = nullptr;
};

TEST_F(TimecodeModelTests, Parse_Numeric)
{
    //! [GIVEN] Sample rate is 44100.0
    m_model->setSampleRate(44100.0);

    std::vector < std::pair<double, QString> > expectedValues = {
        { 0.0, "00 h 00 m 00 s" },
        { 0.6, "00 h 00 m 00 s" },
        { 1.0, "00 h 00 m 01 s" },
        { 1.6, "00 h 00 m 01 s" },
        { 60.0, "00 h 01 m 00 s" },
        { 61.0, "00 h 01 m 01 s" },
        { 3600.0, "01 h 00 m 00 s" },
        { 3601.0, "01 h 00 m 01 s" },
    };

    for (const auto& [value, expected] : expectedValues) {
        //! [WHEN] Set value
        m_model->setValue(value);

        //! [THEN] Check value
        EXPECT_EQ(currentValueString(), expected);
    }

    //! [GIVEN] Current value is 59.0
    m_model->setValue(59.0);
    EXPECT_EQ(currentValueString(), "00 h 00 m 59 s");

    //! [GIEN] Current edited field is 9 in 59 sec
    m_model->setCurrentEditedFieldIndex(11);

    //! [WHEN] Press up key
    QKeyEvent keyEvent(QEvent::KeyPress, Qt::Key_Up, Qt::NoModifier);
    qApp->sendEvent(qApp, &keyEvent);

    //! [THEN] Check value
    EXPECT_EQ(currentValueString(), "00 h 01 m 00 s");
}

TEST_F(TimecodeModelTests, Parse_Beats)
{
    //! [GIVEN] Sample rate is 44100.0
    m_model->setSampleRate(44100.0);

    //! [GIVEN] Current format is BarBeat
    m_model->setCurrentFormat(static_cast<int>(TimecodeModel::ViewFormatType::BarBeat));

    //! [GIVEN] Tempo is 120.0, upper time signature is 3, lower time signature is 4
    m_model->setTempo(120.0);
    m_model->setUpperTimeSignature(3);
    m_model->setLowerTimeSignature(4);

    std::vector < std::pair<double, QString> > expectedValues = {
        { 0.0, "001 bar 01 beat" },
        { 0.6, "001 bar 02 beat" },
        { 1.0, "001 bar 03 beat" },
        { 1.6, "002 bar 01 beat" },
    };

    for (const auto& [value, expected] : expectedValues) {
        //! [WHEN] Set value
        m_model->setValue(value);

        //! [THEN] Check value
        EXPECT_EQ(currentValueString(), expected);
    }

    //! [GIVEN] Current format is BarBeatTick
    m_model->setCurrentFormat(static_cast<int>(TimecodeModel::ViewFormatType::BarBeatTick));

    expectedValues = {
        { -1.0, "--- bar -- beat --" },
        { 0.0, "001 bar 01 beat 01" },
        { 0.6, "001 bar 02 beat 01" },
        { 1.0, "001 bar 03 beat 01" },
        { 1.9, "002 bar 01 beat 04" },
    };

    for (const auto& [value, expected] : expectedValues) {
        //! [WHEN] Set value
        m_model->setValue(value);

        //! [THEN] Check value
        EXPECT_EQ(currentValueString(), expected);
    }
}
}
