/*
* Audacity: A Digital Audio Editor
*/
#include "numericviewmodel.h"

#include "framework/global/log.h"

#include "uicomponents/qml/Muse/UiComponents/menuitem.h"
#include "ui/uiaction.h"

using namespace au::uicomponents;

static bool isFieldEditable(const QChar& fieldSymbol)
{
    return fieldSymbol.isDigit() || fieldSymbol == '-';
}

NumericViewModel::NumericViewModel(QObject* parent)
    : QAbstractListModel(parent)
{
}

int NumericViewModel::rowCount(const QModelIndex& parent) const
{
    UNUSED(parent);
    return m_valueString.size();
}

QVariant NumericViewModel::data(const QModelIndex& index, int role) const
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

QHash<int, QByteArray> NumericViewModel::roleNames() const
{
    static const QHash<int, QByteArray> roles = {
        { rSymbol, "symbol" },
        { rIsEditable, "editable" }
    };

    return roles;
}

QString NumericViewModel::valueString() const
{
    return m_valueString;
}

double NumericViewModel::value() const
{
    return m_value;
}

void NumericViewModel::setValue(double value)
{
    if (qFuzzyCompare(m_value, value)) {
        return;
    }

    m_value = value;

    updateValueString();
    emit valueChanged();
}

int NumericViewModel::currentEditedFieldIndex() const
{
    return m_fieldsInteractionController->currentEditedFieldIndex();
}

void NumericViewModel::setCurrentEditedFieldIndex(int index)
{
    m_fieldsInteractionController->setCurrentEditedFieldIndex(index);
}

QQuickItem* NumericViewModel::visualItem() const
{
    return m_fieldsInteractionController->visualItem();
}

void NumericViewModel::setVisualItem(QQuickItem* item)
{
    m_fieldsInteractionController->setVisualItem(item);
}

int NumericViewModel::currentFormat() const
{
    return static_cast<int>(m_currentFormat);
}

void NumericViewModel::setCurrentFormat(int format)
{
    NumericViewFormatType newFormat = static_cast<NumericViewFormatType>(format);
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

QString NumericViewModel::currentFormatStr() const
{
    NumericViewFormat current = currentViewFormat();
    if (!current.isValid()) {
        return QString();
    }

    return current.title;
}

void NumericViewModel::setCurrentFormatStr(const QString& title)
{
    for (int i = 0; i < m_availableViewFormats.size(); ++i) {
        NumericViewFormat format = m_availableViewFormats[i];
        if (format.title == title) {
            setCurrentFormat(static_cast<int>(format.type));
            return;
        }
    }
    // TODO log error
}

muse::uicomponents::MenuItemList NumericViewModel::availableFormats()
{
    muse::uicomponents::MenuItemList result;

    for (const NumericViewFormat& viewFormat : m_availableViewFormats) {
        muse::uicomponents::MenuItem* item = new muse::uicomponents::MenuItem(this);

        int id = static_cast<int>(viewFormat.type);

        item->setId(QString::number(id));

        muse::ui::UiAction action;
        action.title = muse::TranslatableString::untranslatable(viewFormat.title);
        action.checkable = muse::ui::Checkable::Yes;
        item->setAction(action);

        muse::ui::UiActionState state;
        state.enabled = true;
        state.checked = m_currentFormat == viewFormat.type;

        item->setState(state);

        result << item;
    }

    return result;
}

QVariantList NumericViewModel::availableFormats_property()
{
    return menuItemListToVariantList(availableFormats());
}

double NumericViewModel::sampleRate() const
{
    return m_sampleRate;
}

void NumericViewModel::setSampleRate(double sampleRate)
{
    if (qFuzzyCompare(m_sampleRate, sampleRate)) {
        return;
    }

    m_sampleRate = sampleRate;

    initFormatter();
    updateValueString();
}

double NumericViewModel::tempo() const
{
    return m_tempo;
}

void NumericViewModel::setTempo(double tempo)
{
    if (qFuzzyCompare(m_tempo, tempo)) {
        return;
    }

    m_tempo = tempo;

    initFormatter();
    updateValueString();
}

int NumericViewModel::upperTimeSignature() const
{
    return m_upperTimeSignature;
}

void NumericViewModel::setUpperTimeSignature(int timeSignature)
{
    if (m_upperTimeSignature == timeSignature) {
        return;
    }

    m_upperTimeSignature = timeSignature;

    initFormatter();
    updateValueString();
}

int NumericViewModel::lowerTimeSignature() const
{
    return m_lowerTimeSignature;
}

void NumericViewModel::setLowerTimeSignature(int timeSignature)
{
    if (m_lowerTimeSignature == timeSignature) {
        return;
    }

    m_lowerTimeSignature = timeSignature;

    initFormatter();
    updateValueString();
}

void NumericViewModel::initFieldInteractionController()
{
    m_fieldsInteractionController = std::make_shared<FieldsInteractionController>(this);

    connect(m_fieldsInteractionController.get(), &FieldsInteractionController::currentEditedFieldIndexChanged,
            this, &NumericViewModel::currentEditedFieldIndexChanged);

    connect(m_fieldsInteractionController.get(), &FieldsInteractionController::valueChanged,
            this, &NumericViewModel::setValue);
}

void NumericViewModel::initFormatter()
{
    if (!m_formatter) {
        return;
    }

    m_formatter->setSampleRate(m_sampleRate);
    m_formatter->setTempo(m_tempo);
    m_formatter->setUpperTimeSignature(m_upperTimeSignature);
    m_formatter->setLowerTimeSignature(m_lowerTimeSignature);

    m_formatter->init();
}

void NumericViewModel::updateValueString(bool toNearest)
{
    if (!m_formatter) {
        return;
    }

    QString newValueString = m_formatter->valueToString(m_value, toNearest).valueString;

    if (newValueString.size() != m_valueString.size()) {
        beginResetModel();
        m_valueString = newValueString;
        endResetModel();
    } else {
        m_valueString = newValueString;

        QModelIndex topLeft = index(0, 0);
        QModelIndex bottomRight = index(rowCount() - 1, 0);

        emit dataChanged(topLeft, bottomRight, { rSymbol, rIsEditable });
    }

    m_fieldsInteractionController->setValueString(m_valueString);
}

const NumericViewFormat& NumericViewModel::currentViewFormat() const
{
    for (int i = 0; i < m_availableViewFormats.size(); ++i) {
        if (m_availableViewFormats[i].type == m_currentFormat) {
            return m_availableViewFormats[i];
        }
    }

    static NumericViewFormat stub;
    return stub;
}
