/*
* Audacity: A Digital Audio Editor
*/
#include "timecodemodel.h"

#include "uicomponents/view/menuitem.h"
#include "ui/uiaction.h"

#include "numericformatter.h"
#include "beatsformatter.h"

#include "translation.h"
#include "log.h"

using namespace au::playback;
using namespace muse::uicomponents;
using namespace muse::ui;

static bool isFieldEditable(const QChar& fieldSymbol)
{
    return fieldSymbol.isDigit();
}

TimecodeModel::TimecodeModel(QObject* parent)
    : QAbstractListModel(parent)
{
    // translate all
    m_availableViewFormats = {
        { TimecodeFormatType::Seconds, muse::qtrc("playback", "seconds"), "01000,01000s" },
        { TimecodeFormatType::SecondsMilliseconds, muse::qtrc("playback", "seconds + milliseconds"), "01000,01000>01000 s" },
        { TimecodeFormatType::HHMMSS, muse::qtrc("playback", "hh:mm:ss"), "0100 h 060 m 060 s" },
        { TimecodeFormatType::DDHHMMSS, muse::qtrc("playback", "dd:hh:mm:ss"), "0100 d 024 h 060 m 060 s" },

        { TimecodeFormatType::HHMMSSHundredths, muse::qtrc("playback", "hh:mm:ss + hundredths"), "0100 h 060 m 060>0100 s" },
        { TimecodeFormatType::HHMMSSMilliseconds, muse::qtrc("playback", "hh:mm:ss + milliseconds"), "0100 h 060 m 060>01000 s" },

        { TimecodeFormatType::HHMMSSSamples, muse::qtrc("playback", "hh:mm:ss + samples"), "0100 h 060 m 060 s+># samples" },
        { TimecodeFormatType::Samples, muse::qtrc("playback", "samples"), "01000,01000,01000 samples|#" },

        { TimecodeFormatType::HHMMSSFilmFrames, muse::qtrc("playback", "hh:mm:ss + film frames (24 fps)"),
          "0100 h 060 m 060 s+>24 frames" },
        { TimecodeFormatType::FilmFrames, muse::qtrc("playback", "Film frames (24 fps)"), "01000,01000 frames|24" },

        { TimecodeFormatType::HHMMSSNTSCDropFrames, muse::qtrc("playback", "hh:mm:ss + NTSC drop frames"),
          "0100 h 060 m 060 s+>30 frames|N" },
        { TimecodeFormatType::HHMMSSNTSCNonDropFrames, muse::qtrc("playback", "hh:mm:ss + NTSC non-drop frames"),
          "0100 h 060 m 060 s+>030 frames| .999000999" },
        { TimecodeFormatType::NTSCFrames, muse::qtrc("playback", "NTSC frames"), "01000,01000 frames|29.97002997" },

        { TimecodeFormatType::HHMMSSPALFrames, muse::qtrc("playback", "hh:mm:ss + PAL frames (25 fps)"), "0100 h 060 m 060 s+>25 frames" },
        { TimecodeFormatType::PALFrames, muse::qtrc("playback", "PAL frames (25 fps)"), "01000,01000 frames|25" },

        { TimecodeFormatType::HHMMSSCDDAFrames, muse::qtrc("playback", "hh:mm:ss + CDDA frames (25 fps)"),
          "0100 h 060 m 060 s+>75 frames" },
        { TimecodeFormatType::CDDAFrames, muse::qtrc("playback", "CDDA frames (75 fps)"), "01000,01000 frames|75" },

        { TimecodeFormatType::BarBeat, muse::qtrc("playback", "bar:beat"), "bar:beat" },
        { TimecodeFormatType::BarBeatTick, muse::qtrc("playback", "bar:beat:tick"), "bar:beat:tick" },
    };

    initFieldInteractionController();

    reloadFormatter();

    setValue(0.0);
}

QVariantList TimecodeModel::availableFormats()
{
    muse::uicomponents::MenuItemList result;

    for (const ViewFormat& viewFormat : m_availableViewFormats) {
        MenuItem* item = new MenuItem(this);

        int id = static_cast<int>(viewFormat.type);

        item->setId(QString::number(id));

        UiAction action;
        action.title = muse::TranslatableString::untranslatable(viewFormat.title);
        action.checkable = Checkable::Yes;
        item->setAction(action);

        UiActionState state;
        state.enabled = true;
        state.checked = m_currentFormat == viewFormat.type;

        item->setState(state);

        result << item;
    }

    return menuItemListToVariantList(result);
}

int TimecodeModel::rowCount(const QModelIndex& parent) const
{
    UNUSED(parent);
    return m_valueString.size();
}

QVariant TimecodeModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid()) {
        return QVariant();
    }

    QChar ch = m_valueString[index.row()];

    switch (role) {
    case rSymbol: return QVariant::fromValue(ch);
    case rIsEditable: return isFieldEditable(ch);
    }

    return QVariant();
}

QHash<int, QByteArray> TimecodeModel::roleNames() const
{
    static const QHash<int, QByteArray> roles = {
        { rSymbol, "symbol" },
        { rIsEditable, "editable" }
    };

    return roles;
}

QString TimecodeModel::valueString() const
{
    return m_valueString;
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

double TimecodeModel::value() const
{
    return m_value;
}

void TimecodeModel::setValue(double value)
{
    if (qFuzzyCompare(m_value, value)) {
        return;
    }

    m_value = value;

    updateValueString();

    emit valueChanged();
}

int TimecodeModel::currentFormat() const
{
    return static_cast<int>(m_currentFormat);
}

void TimecodeModel::setCurrentFormat(int format)
{
    TimecodeFormatType newFormat = static_cast<TimecodeFormatType>(format);
    if (m_currentFormat == newFormat) {
        return;
    }

    m_currentFormat = newFormat;

    reloadFormatter();
    updateValueString();

    emit currentFormatChanged();
    emit availableFormatsChanged();
    emit valueChanged();
}

QString TimecodeModel::currentFormatStr() const
{
    ViewFormat current = currentViewFormat();
    if (!current.isValid()) {
        return QString();
    }

    return current.title;
}

void TimecodeModel::setCurrentFormatStr(const QString& title)
{
    for (int i = 0; i < m_availableViewFormats.size(); ++i) {
        ViewFormat format = m_availableViewFormats[i];
        if (format.title == title) {
            setCurrentFormat(static_cast<int>(format.type));
            return;
        }
    }
    // TODO log error
}

int TimecodeModel::currentEditedFieldIndex() const
{
    return m_fieldsInteractionController->currentEditedFieldIndex();
}

void TimecodeModel::setCurrentEditedFieldIndex(int index)
{
    m_fieldsInteractionController->setCurrentEditedFieldIndex(index);
}

QQuickItem* TimecodeModel::visualItem() const
{
    return m_fieldsInteractionController->visualItem();
}

void TimecodeModel::setVisualItem(QQuickItem* item)
{
    m_fieldsInteractionController->setVisualItem(item);
}

void TimecodeModel::reloadFormatter()
{
    ViewFormat currentFormat = currentViewFormat();
    if (!currentFormat.isValid()) {
        return;
    }

    if (currentFormat.type == TimecodeFormatType::BarBeat || currentFormat.type == TimecodeFormatType::BarBeatTick) {
        int fracPart = currentFormat.type == TimecodeFormatType::BarBeat ? 0 : 16;
        m_formatter = std::make_shared<BeatsFormatter>(currentFormat.formatStr, fracPart, m_mode);
    } else {
        m_formatter = std::make_shared<NumericFormatter>(currentFormat.formatStr);
    }

    initFormatter();

    m_fieldsInteractionController->setFormatter(m_formatter);
}

void TimecodeModel::initFormatter()
{
    m_formatter->setSampleRate(m_sampleRate);
    m_formatter->setTempo(m_tempo);
    m_formatter->setUpperTimeSignature(m_upperTimeSignature);
    m_formatter->setLowerTimeSignature(m_lowerTimeSignature);

    m_formatter->init();
}

void TimecodeModel::initFieldInteractionController()
{
    m_fieldsInteractionController = std::make_shared<FieldsInteractionController>(this);

    connect(m_fieldsInteractionController.get(), &FieldsInteractionController::currentEditedFieldIndexChanged,
            this, &TimecodeModel::currentEditedFieldIndexChanged);

    connect(m_fieldsInteractionController.get(), &FieldsInteractionController::valueChanged,
            this, &TimecodeModel::setValue);
}

void TimecodeModel::updateValueString()
{
    constexpr auto toNearest = true;
    QString newValueString = m_formatter->valueToString(m_value, toNearest).valueString;

    beginResetModel();
    m_valueString = newValueString;
    endResetModel();

    m_fieldsInteractionController->setValueString(m_valueString);
}

const TimecodeModel::ViewFormat& TimecodeModel::currentViewFormat() const
{
    for (int i = 0; i < m_availableViewFormats.size(); ++i) {
        if (m_availableViewFormats[i].type == m_currentFormat) {
            return m_availableViewFormats[i];
        }
    }

    static ViewFormat stub;
    return stub;
}

double TimecodeModel::sampleRate() const
{
    return m_sampleRate;
}

void TimecodeModel::setSampleRate(double sampleRate)
{
    if (qFuzzyCompare(m_sampleRate, sampleRate)) {
        return;
    }

    m_sampleRate = sampleRate;

    initFormatter();
    updateValueString();

    emit currentFormatChanged();
}

double TimecodeModel::tempo() const
{
    return m_tempo;
}

void TimecodeModel::setTempo(double tempo)
{
    if (qFuzzyCompare(m_tempo, tempo)) {
        return;
    }

    m_tempo = tempo;

    initFormatter();
    updateValueString();
}

int TimecodeModel::upperTimeSignature() const
{
    return m_upperTimeSignature;
}

void TimecodeModel::setUpperTimeSignature(int timeSignature)
{
    if (m_upperTimeSignature == timeSignature) {
        return;
    }

    m_upperTimeSignature = timeSignature;

    initFormatter();
    updateValueString();
}

int TimecodeModel::lowerTimeSignature() const
{
    return m_lowerTimeSignature;
}

void TimecodeModel::setLowerTimeSignature(int timeSignature)
{
    if (m_lowerTimeSignature == timeSignature) {
        return;
    }

    m_lowerTimeSignature = timeSignature;

    initFormatter();
    updateValueString();
}
