/*
 * Audacity: A Digital Audio Editor
 */
#include "graphiceqbandsmodel.h"

#include "graphiceq.h"

#include "log.h"

namespace au::effects {
namespace {
QString centerFrequencyToString(double frequency)
{
    if (frequency < 1000.0) {
        return QString::number(static_cast<int>(frequency + .5));
    } else {
        const auto khz = frequency / 1000.0;
        return QString::number(khz, 'f', (khz == static_cast<int>(khz)) ? 0 : 1) + "k";
    }
}
}

GraphicEqBandsModel::GraphicEqBandsModel(QObject* parent, GraphicEq& eq)
    : QAbstractListModel(parent),  m_eq(eq), mSliders(eq.mCurvesList)
{
}

void GraphicEqBandsModel::reload()
{
    if (!m_inited) {
        m_inited = true;
        mSliders.Init();
    }
    doReload();
    emit dataChanged(index(0), index(NUM_BANDS - 1));
}

void GraphicEqBandsModel::doReload()
{
    // Adapted from EqualizationUI::TransferDataToWindow

    auto& drawMode = m_eq.mCurvesList.mParameters.mDrawMode;

    // Override draw mode, if we're not displaying the radio buttons.
    if (m_eq.GetOptions() == kEqOptionCurve) {
        drawMode = true;
    } else if (m_eq.GetOptions() == kEqOptionGraphic) {
        drawMode = false;
    }

    // Set Graphic (Fader) or Draw mode
    if (!drawMode) {
        updateGraphic();
    }

    // UpdateRuler();
}

void GraphicEqBandsModel::updateGraphic()
{
    auto& parameters = m_eq.mCurvesList.mParameters;
    const auto& lin = parameters.mLin;
    auto& linEnvelope = parameters.mLinEnvelope;
    auto& logEnvelope = parameters.mLogEnvelope;
    const auto& hiFreq = parameters.mHiFreq;

    auto& drawMode = parameters.mDrawMode;

    if (lin) { //going from lin to log freq scale - do not use IsLinear() here
        // add some extra points to the linear envelope for the graphic to follow
        double step = pow(2., 1. / 12.); // twelve steps per octave
        double when, value;
        for (double freq=10.; freq < hiFreq; freq*=step) {
            when = freq / hiFreq;
            value = linEnvelope.GetValue(when);
            linEnvelope.Insert(when, value);
        }

        mSliders.EnvLinToLog();
    }

    mSliders.ErrMin();

    mSliders.GraphicEQ(logEnvelope);
    drawMode = false;
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
        return mSliders.GetSliderValue(index.row());
    case rCenterFreq:
        return centerFrequencyToString(EqualizationBandSliders::kThirdOct[index.row()]);
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
        mSliders.SetSliderValue(index.row(), value.toDouble());
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

void GraphicEqBandsModel::flatten()
{
    mSliders.Flatten();
    emit dataChanged(index(0), index(NUM_BANDS - 1));
}

void GraphicEqBandsModel::invert()
{
    mSliders.Invert();
    emit dataChanged(index(0), index(NUM_BANDS - 1));
}
}
