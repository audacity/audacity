/*
 * Audacity: A Digital Audio Editor
 */
#include "graphiceqbandsmodel.h"

#include <cassert>

namespace au::effects {
namespace {
QString centerFrequencyToString(double frequency)
{
    if (frequency < 1000.0) {
        return QString::number(static_cast<int>(frequency + .5)) + " Hz";
    } else {
        const auto khz = frequency / 1000.0;
        return QString::number(khz, 'f', (khz == static_cast<int>(khz)) ? 0 : 2) + " kHz";
    }
}
}

GraphicEqBandsModel::GraphicEqBandsModel(QObject* parent)
    : QAbstractListModel(parent)
{
    mBandDbs.fill(0.0);
}

int GraphicEqBandsModel::rowCount(const QModelIndex& parent) const
{
    Q_UNUSED(parent);
    return NUM_BANDS;
}

QVariant GraphicEqBandsModel::data(const QModelIndex& index, int role) const
{
    if (!index.isValid() || index.row() < 0 || index.row() >= NUM_BANDS) {
        return QVariant();
    }

    switch (role) {
    case rDbGain:
        return mBandDbs.at(index.row());
    case rCenterFreq:
        return centerFrequencyToString(kThirdOct.at(index.row()));
    default:
        assert(false && "Invalid role in GraphicEqBandsModel::data");
        break;
    }

    return QVariant();
}

bool GraphicEqBandsModel::setData(const QModelIndex& index, const QVariant& value, int role)
{
    if (!index.isValid() || index.row() < 0 || index.row() >= NUM_BANDS) {
        return false;
    }

    if (role == rDbGain) {
        mBandDbs[index.row()] = value.toDouble();
        emit dataChanged(index, index, { role });
        return true;
    }

    assert(false && "Invalid role in GraphicEqBandsModel::setData");
    return false;
}

QHash<int, QByteArray> GraphicEqBandsModel::roleNames() const
{
    QHash<int, QByteArray> roles;
    roles[rDbGain] = "dbGain";
    roles[rCenterFreq] = "centerFreq";
    return roles;
}
}
