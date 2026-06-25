/*
* Audacity: A Digital Audio Editor
*/
#include "timecodemodel.h"

#include "internal/numeric/numericformatter.h"
#include "internal/numeric/beatsformatter.h"

#include "translation.h"

using namespace au::uicomponents;

namespace {
QList<NumericViewFormat> makeTimecodeFormats()
{
    return {
        // translate all
        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::Seconds), muse::qtrc("uicomponents", "seconds"),
                            "01000,01000s" },
        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::SecondsMilliseconds), muse::qtrc("uicomponents",
                                                                                                                    "seconds + milliseconds"),
                            "01000,01000>01000 s" },
        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSS), muse::qtrc("uicomponents", "hh:mm:ss"),
                            "0100 h 060 m 060 s" },
        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::DDHHMMSS), muse::qtrc("uicomponents", "dd:hh:mm:ss"),
                            "0100 d 024 h 060 m 060 s" },

        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSSHundredths), muse::qtrc("uicomponents",
                                                                                                                 "hh:mm:ss + hundredths"),
                            "0100 h 060 m 060>0100 s" },
        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSSMilliseconds), muse::qtrc("uicomponents",
                                                                                                                   "hh:mm:ss + milliseconds"),
                            "0100 h 060 m 060>01000 s" },

        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSSSamples), muse::qtrc("uicomponents",
                                                                                                              "hh:mm:ss + samples"),
                            "0100 h 060 m 060 s+># samples" },
        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::Samples), muse::qtrc("uicomponents", "samples"),
                            "01000,01000,01000 samples|#" },

        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSSFilmFrames), muse::qtrc("uicomponents",
                                                                                                                 "hh:mm:ss + film frames (24 fps)"),
                            "0100 h 060 m 060 s+>24 frames" },
        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::FilmFrames), muse::qtrc("uicomponents",
                                                                                                           "Film frames (24 fps)"),
                            "01000,01000 frames|24" },

        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSSNTSCDropFrames), muse::qtrc("uicomponents",
                                                                                                                     "hh:mm:ss + NTSC drop frames"),
                            "0100 h 060 m 060 s+>30 frames|N" },
        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSSNTSCNonDropFrames), muse::qtrc("uicomponents",
                                                                                                                        "hh:mm:ss + NTSC non-drop frames"),
                            "0100 h 060 m 060 s+>030 frames| .999000999" },
        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::NTSCFrames), muse::qtrc("uicomponents", "NTSC frames"),
                            "01000,01000 frames|29.97002997" },

        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSSPALFrames), muse::qtrc("uicomponents",
                                                                                                                "hh:mm:ss + PAL frames (25 fps)"),
                            "0100 h 060 m 060 s+>25 frames" },
        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::PALFrames), muse::qtrc("uicomponents",
                                                                                                          "PAL frames (25 fps)"),
                            "01000,01000 frames|25" },

        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSSCDDAFrames), muse::qtrc("uicomponents",
                                                                                                                 "hh:mm:ss + CDDA frames (25 fps)"),
                            "0100 h 060 m 060 s+>75 frames" },
        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::CDDAFrames), muse::qtrc("uicomponents",
                                                                                                           "CDDA frames (75 fps)"),
                            "01000,01000 frames|75" },

        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::BarBeat), muse::qtrc("uicomponents", "bar:beat"),
                            "bar:beat" },
        NumericViewFormat { static_cast<NumericViewFormatType>(TimecodeFormatType::BarBeatTick),
                            muse::qtrc("uicomponents", "bar:beat:tick"),
                            "bar:beat:tick" },
    };
}
}

TimecodeModel::TimecodeModel(QObject* parent)
    : NumericViewModel(parent, makeTimecodeFormats())
{
    setCurrentFormat(static_cast<int>(TimecodeFormatType::HHMMSS));

    reloadFormatter();

    setValue(0.0);
}

TimecodeMode TimecodeModel::mode() const
{
    return m_mode;
}

void TimecodeModel::setMode(TimecodeMode mode)
{
    if (m_mode == mode) {
        return;
    }

    m_mode = mode;

    reloadFormatter();
    updateValueString();
    emit valueChanged();
}

void TimecodeModel::reloadFormatter()
{
    NumericViewFormat currentFormat = currentViewFormat();
    if (!currentFormat.isValid()) {
        return;
    }

    TimecodeFormatType formatType = static_cast<TimecodeFormatType>(currentFormat.type);
    if (formatType == TimecodeFormatType::BarBeat || formatType == TimecodeFormatType::BarBeatTick) {
        int fracPart = formatType == TimecodeFormatType::BarBeat ? 0 : 16;
        m_formatter = std::make_shared<BeatsFormatter>(currentFormat.formatStr, fracPart, m_mode);
    } else {
        m_formatter = std::make_shared<NumericFormatter>(currentFormat.formatStr);
    }

    initFormatter();

    m_fieldsInteractionController->setFormatter(m_formatter);
}
