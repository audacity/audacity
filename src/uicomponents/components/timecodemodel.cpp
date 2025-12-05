/*
* Audacity: A Digital Audio Editor
*/
#include "timecodemodel.h"

#include "numericformatter.h"
#include "beatsformatter.h"

#include "translation.h"

using namespace au::uicomponents;

TimecodeModel::TimecodeModel(QObject* parent)
    : NumericViewModel(parent)
{
    // translate all
    m_availableViewFormats = {
        { static_cast<NumericViewFormatType>(TimecodeFormatType::Seconds), muse::qtrc("uicomponents", "seconds"), "01000,01000s" },
        { static_cast<NumericViewFormatType>(TimecodeFormatType::SecondsMilliseconds), muse::qtrc("uicomponents", "seconds + milliseconds"),
          "01000,01000>01000 s" },
        { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSS), muse::qtrc("uicomponents", "hh:mm:ss"), "0100 h 060 m 060 s" },
        { static_cast<NumericViewFormatType>(TimecodeFormatType::DDHHMMSS), muse::qtrc("uicomponents", "dd:hh:mm:ss"),
          "0100 d 024 h 060 m 060 s" },

        { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSSHundredths), muse::qtrc("uicomponents", "hh:mm:ss + hundredths"),
          "0100 h 060 m 060>0100 s" },
        { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSSMilliseconds), muse::qtrc("uicomponents", "hh:mm:ss + milliseconds"),
          "0100 h 060 m 060>01000 s" },

        { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSSSamples), muse::qtrc("uicomponents", "hh:mm:ss + samples"),
          "0100 h 060 m 060 s+># samples" },
        { static_cast<NumericViewFormatType>(TimecodeFormatType::Samples), muse::qtrc("uicomponents", "samples"),
          "01000,01000,01000 samples|#" },

        { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSSFilmFrames), muse::qtrc("uicomponents",
                                                                                               "hh:mm:ss + film frames (24 fps)"),
          "0100 h 060 m 060 s+>24 frames" },
        { static_cast<NumericViewFormatType>(TimecodeFormatType::FilmFrames), muse::qtrc("uicomponents", "Film frames (24 fps)"),
          "01000,01000 frames|24" },

        { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSSNTSCDropFrames), muse::qtrc("uicomponents",
                                                                                                   "hh:mm:ss + NTSC drop frames"),
          "0100 h 060 m 060 s+>30 frames|N" },
        { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSSNTSCNonDropFrames), muse::qtrc("uicomponents",
                                                                                                      "hh:mm:ss + NTSC non-drop frames"),
          "0100 h 060 m 060 s+>030 frames| .999000999" },
        { static_cast<NumericViewFormatType>(TimecodeFormatType::NTSCFrames), muse::qtrc("uicomponents", "NTSC frames"),
          "01000,01000 frames|29.97002997" },

        { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSSPALFrames), muse::qtrc("uicomponents",
                                                                                              "hh:mm:ss + PAL frames (25 fps)"),
          "0100 h 060 m 060 s+>25 frames" },
        { static_cast<NumericViewFormatType>(TimecodeFormatType::PALFrames), muse::qtrc("uicomponents", "PAL frames (25 fps)"),
          "01000,01000 frames|25" },

        { static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSSCDDAFrames), muse::qtrc("uicomponents",
                                                                                               "hh:mm:ss + CDDA frames (25 fps)"),
          "0100 h 060 m 060 s+>75 frames" },
        { static_cast<NumericViewFormatType>(TimecodeFormatType::CDDAFrames), muse::qtrc("uicomponents", "CDDA frames (75 fps)"),
          "01000,01000 frames|75" },

        { static_cast<NumericViewFormatType>(TimecodeFormatType::BarBeat), muse::qtrc("uicomponents", "bar:beat"), "bar:beat" },
        { static_cast<NumericViewFormatType>(TimecodeFormatType::BarBeatTick), muse::qtrc("uicomponents", "bar:beat:tick"),
          "bar:beat:tick" },
    };

    m_currentFormat = static_cast<NumericViewFormatType>(TimecodeFormatType::HHMMSS);

    initFieldInteractionController();

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
