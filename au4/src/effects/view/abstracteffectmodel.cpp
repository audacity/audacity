#include "abstracteffectmodel.h"

#include <math.h>

using namespace au::effects;

AbstractEffectModel::AbstractEffectModel(QObject* parent)
    : QObject(parent)
{
}

QString AbstractEffectModel::id() const
{
    return m_id;
}

void AbstractEffectModel::setId(const QString& newId)
{
    if (m_id == newId) {
        return;
    }

    m_id = newId;
    emit idChanged();
}

double AbstractEffectModel::linearToDB(double value) const
{
    return 20.0 * log10(value);
}

double AbstractEffectModel::dBToLinear(double value) const
{
    return pow(10.0, (value) / 20.0);
}
