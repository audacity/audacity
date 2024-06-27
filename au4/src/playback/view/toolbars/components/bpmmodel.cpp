/*
* Audacity: A Digital Audio Editor
*/
#include "bpmmodel.h"

#include <QKeyEvent>

#include "numericformatter.h"

#include "log.h"

static constexpr double BPM_MAX = 999.0;
static constexpr double BPM_MIN = 1.0;

using namespace au::playback;

static bool isFieldEditable(const QChar& fieldSymbol)
{
    return fieldSymbol.isDigit();
}

BPMModel::BPMModel(QObject* parent)
    : QAbstractListModel(parent)
{
    initFieldInteractionController();

    reloadFormatter();

    setValue(0.0);
}

void BPMModel::upValue()
{
    setValue(std::min(m_value + 1, BPM_MAX));
}

void BPMModel::downValue()
{
    setValue(std::max(m_value - 1, BPM_MIN));
}

int BPMModel::rowCount(const QModelIndex& parent) const
{
    UNUSED(parent);
    return m_valueString.size();
}

QVariant BPMModel::data(const QModelIndex& index, int role) const
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

QHash<int, QByteArray> BPMModel::roleNames() const
{
    static const QHash<int, QByteArray> roles = {
        { rSymbol, "symbol" },
        { rIsEditable, "editable" }
    };

    return roles;
}

double BPMModel::value() const
{
    return m_value;
}

void BPMModel::setValue(double value)
{
    if (value < BPM_MIN || value > BPM_MAX) {
        return;
    }

    if (qFuzzyCompare(m_value, value)) {
        return;
    }

    m_value = value;

    updateValueString();

    emit valueChanged();
}

int BPMModel::currentEditedFieldIndex() const
{
    return m_fieldsInteractionController->currentEditedFieldIndex();
}

void BPMModel::setCurrentEditedFieldIndex(int index)
{
    m_fieldsInteractionController->setCurrentEditedFieldIndex(index);
}

QQuickItem* BPMModel::visualItem() const
{
    return m_fieldsInteractionController->visualItem();
}

void BPMModel::setVisualItem(QQuickItem* item)
{
    m_fieldsInteractionController->setVisualItem(item);
}

void BPMModel::reloadFormatter()
{
    static const QString FORMAT = "1000bpm"; // translate

    m_formatter = std::make_shared<NumericFormatter>(FORMAT);

    initFormatter();

    m_fieldsInteractionController->setFormatter(m_formatter);
}

void BPMModel::initFormatter()
{
    m_formatter->init();
}

void BPMModel::initFieldInteractionController()
{
    m_fieldsInteractionController = std::make_shared<FieldsInteractionController>(this);

    connect(m_fieldsInteractionController.get(), &FieldsInteractionController::currentEditedFieldIndexChanged,
            this, &BPMModel::currentEditedFieldIndexChanged);

    connect(m_fieldsInteractionController.get(), &FieldsInteractionController::valueChanged,
            this, &BPMModel::setValue);
}

void BPMModel::updateValueString()
{
    QString newValueString = m_formatter->valueToString(m_value, false).valueString;

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

    m_fieldsInteractionController->setValueString(m_valueString);
}
