/*
* Audacity: A Digital Audio Editor
*/
#include "timecodemodel.h"

#include <QKeyEvent>

#include "uicomponents/view/menuitem.h"
#include "ui/uiaction.h"

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
    m_availableViewFormats = {
        { ViewFormatType::Seconds, muse::qtrc("playback", "Seconds"), "01000,01000s" },
        { ViewFormatType::SecondsMilliseconds, muse::qtrc("playback", "Seconds + milliseconds"), "01000,01000>01000 s" },
        { ViewFormatType::HHMMSS, muse::qtrc("playback", "hh:mm:ss"), "0100 h 060 m 060 s" },
        { ViewFormatType::DDHHMMSS, muse::qtrc("playback", "dd:hh:mm:ss"), "0100 d 024 h 060 m 060 s" },

        { ViewFormatType::HHMMSSHundredths, muse::qtrc("playback", "hh:mm:ss + hundredths"), "0100 h 060 m 060>0100 s" },
        { ViewFormatType::HHMMSSMilliseconds, muse::qtrc("playback", "hh:mm:ss + milliseconds"), "0100 h 060 m 060>01000 s" },

        { ViewFormatType::HHMMSSSamples, muse::qtrc("playback", "hh:mm:ss + samples"), "0100 h 060 m 060 s+># samples" },
        { ViewFormatType::Samples, muse::qtrc("playback", "Samples"), "01000,01000,01000 samples|#" },

        { ViewFormatType::HHMMSSFilmFrames, muse::qtrc("playback", "hh:mm:ss + film frames (24 fps)"), "0100 h 060 m 060 s+>24 frames" },
        { ViewFormatType::FilmFrames, muse::qtrc("playback", "Film frames (24 fps)"), "01000,01000 frames|24" },

        { ViewFormatType::HHMMSSNTSCDropFrames, muse::qtrc("playback", "hh:mm:ss + NTSC drop frames"), "0100 h 060 m 060 s+>30 frames|N" },
        { ViewFormatType::HHMMSSNTSCNonDropFrames, muse::qtrc("playback", "hh:mm:ss + NTSC non-drop frames"),
          "0100 h 060 m 060 s+>030 frames| .999000999" },
        { ViewFormatType::NTSCFrames, muse::qtrc("playback", "NTSC frames"), "01000,01000 frames|29.97002997" },

        { ViewFormatType::HHMMSSPALFrames, muse::qtrc("playback", "hh:mm:ss + PAL frames (25 fps)"), "0100 h 060 m 060 s+>25 frames" },
        { ViewFormatType::PALFrames, muse::qtrc("playback", "PAL frames (25 fps)"), "01000,01000 frames|25" },

        { ViewFormatType::HHMMSSCDDAFrames, muse::qtrc("playback", "hh:mm:ss + CDDA frames (25 fps)"), "0100 h 060 m 060 s+>75 frames" },
        { ViewFormatType::CDDAFrames, muse::qtrc("playback", "CDDA frames (75 fps)"), "01000,01000 frames|75" },

        { ViewFormatType::BarBeat, muse::qtrc("playback", "bar:beat"), "01000,01000s" },                                                                //! doesn't work
        { ViewFormatType::BarBeatTick, muse::qtrc("playback", "bar:beat:tick"), "01000,01000s" },                                                       //! doesn't work
    };

    m_formater = std::make_shared<NumericConverterFormatter>(NumericType::Time, m_availableViewFormats[currentFormat()].formatStr);
    m_formater->parseFormatString();

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
        state.checked = m_currentFormat == id;

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
    return m_currentFormat;
}

void TimecodeModel::setCurrentFormat(int format)
{
    if (m_currentFormat == format) {
        return;
    }

    m_currentFormat = format;

    m_formater = std::make_shared<NumericConverterFormatter>(NumericType::Time, m_availableViewFormats[format].formatStr);
    m_formater->setSampleRate(sampleRate());
    m_formater->parseFormatString();

    updateValueString();

    emit currentFormatChanged();
    emit availableFormatsChanged();
    emit valueChanged();
}

int TimecodeModel::currentEditedFieldIndex() const
{
    return m_currentEditedFieldIndex;
}

void TimecodeModel::setCurrentEditedFieldIndex(int index)
{
    if (m_currentEditedFieldIndex == index) {
        return;
    }

    if (index >= 0) {
        qApp->installEventFilter(this);
    } else {
        qApp->removeEventFilter(this);
    }

    m_currentEditedFieldIndex = index;
    emit currentEditedFieldIndexChanged();
}

QQuickItem* TimecodeModel::visualItem() const
{
    return m_visualItem;
}

void TimecodeModel::setVisualItem(QQuickItem* newVisualItem)
{
    if (m_visualItem == newVisualItem) {
        return;
    }

    m_visualItem = newVisualItem;
    emit visualItemChanged();
}

bool TimecodeModel::eventFilter(QObject* watched, QEvent* event)
{
    if (event->type() == QEvent::KeyPress) {
        QKeyEvent* keyEvent = dynamic_cast<QKeyEvent*>(event);
        int key = keyEvent->key();
        if (key == Qt::Key_Escape) {
            setCurrentEditedFieldIndex(-1);
            return true;
        } else if (key >= Qt::Key_0 && key <= Qt::Key_9) {
            QString newValueStr = m_valueString;
            newValueStr.replace(m_currentEditedFieldIndex, 1, QChar('0' + (key - Qt::Key_0)));

            setValue(m_formater->stringToValue(newValueStr).value());
            return true;
        } else if (key == Qt::Key_Left || key == Qt::Key_Right) {
            moveCurrentEditedField(key);
            return true;
        } else if (key == Qt::Key_Up || key == Qt::Key_Down) {
            adjustCurrentEditedField(key);
            return true;
        }
    }

    if (event->type() == QEvent::MouseButtonPress) {
        if (!isMouseWithinBoundaries(QCursor::pos())) {
            setCurrentEditedFieldIndex(-1);
        }
    }

    return QObject::eventFilter(watched, event);
}

bool TimecodeModel::isMouseWithinBoundaries(const QPoint& mousePos) const
{
    if (!m_visualItem) {
        return false;
    }

    QRectF viewRect = m_visualItem->boundingRect();
    viewRect.moveTopLeft(m_visualItem->mapToGlobal(QPoint(0, 0)));

    return viewRect.contains(mousePos);
}

void TimecodeModel::moveCurrentEditedField(int moveKey)
{
    int newIndex = m_currentEditedFieldIndex;
    while (true) {
        if (moveKey == Qt::Key_Left) {
            newIndex--;
        } else {
            newIndex++;
        }

        if (newIndex < 0) {
            newIndex = rowCount() - 1;
        } else if (newIndex >= rowCount()) {
            newIndex = 0;
        }

        if (isFieldEditable(m_valueString[newIndex])) {
            break;
        }
    }

    setCurrentEditedFieldIndex(newIndex);
}

void TimecodeModel::adjustCurrentEditedField(int adjustKey)
{
    int digitIndex = 0;

    for (int i = 0; i < m_valueString.size(); ++i) {
        if (i == m_currentEditedFieldIndex) {
            break;
        }

        if (isFieldEditable(m_valueString[i])) {
            digitIndex++;
        }
    }

    setValue(m_formater->singleStep(m_value, digitIndex, adjustKey == Qt::Key_Up));
}

void TimecodeModel::updateValueString()
{
    QString newValueString = m_formater->valueToString(m_value, false).valueString;

    if (newValueString.size() != m_valueString.size()) {
        beginResetModel();
        m_valueString = newValueString;
        endResetModel();
    } else {
        for (int i = 0; i < newValueString.size(); ++i) {
            if (newValueString[i] != m_valueString[i]) {
                m_valueString[i] = newValueString[i];

                QModelIndex index = createIndex(i, 0);
                emit dataChanged(index, index, { rSymbol });
            }
        }
    }
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

    m_formater->setSampleRate(sampleRate);

    m_sampleRate = sampleRate;
    emit sampleRateChanged();
}
