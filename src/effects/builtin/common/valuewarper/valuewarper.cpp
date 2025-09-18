/*
 * Audacity: A Digital Audio Editor
 */

#include "valuewarper.h"

#include "warpingtransformer.h"

#include "global/types/number.h"
#include "global/log.h"

namespace au::effects {
namespace {
class PassthroughTransformer : public IValueTransformer
{
public:
    double forward(double x) const override { return x; }
    double inverse(double y) const override { return y; }
};
}

bool ValueWarper::useWarper() const
{
    if (m_middle.isNull()) {
        return false;
    }
    const double mid = m_middle.toDouble();
    // Special cases to avoid, or we'll be getting NaNs.
    return m_min < mid && mid < m_max && !muse::is_equal(mid, (m_min + m_max) / 2.0);
}

void ValueWarper::componentComplete()
{
    IF_ASSERT_FAILED(!m_transformer) {
        LOGW() << "ValueWarper already initialized";
    }

    if (!useWarper()) {
        m_transformer = std::make_unique<PassthroughTransformer>();
    } else {
        m_transformer = std::make_unique<WarpingTransformer>(m_min, m_middle.toDouble(), m_max);
    }

    m_warpedValue = m_transformer->forward(m_value);
    emit warpedValueChanged();
}

QVariant ValueWarper::middle() const
{
    return m_middle;
}

void ValueWarper::setMiddle(const QVariant& value)
{
    if (m_middle == value) {
        return;
    }

    m_middle = value;
    emit middleChanged();

    if (!m_transformer) {
        return;
    }

    if (useWarper()) {
        m_transformer = std::make_unique<WarpingTransformer>(m_min, m_middle.toDouble(), m_max);
    } else {
        m_transformer = std::make_unique<PassthroughTransformer>();
    }

    m_warpedValue = m_transformer->forward(m_value);
    emit warpedValueChanged();
}

double ValueWarper::value() const
{
    return m_value;
}

void ValueWarper::setValue(double value)
{
    if (muse::is_equal(value, m_value)) {
        return;
    }

    m_value = value;
    emit valueChanged();

    if (m_transformer) {
        m_warpedValue = m_transformer->forward(m_value);
        emit warpedValueChanged();
    }
}

double ValueWarper::warpedValue() const
{
    return m_warpedValue;
}

void ValueWarper::setWarpedValue(double value)
{
    // Also ignore value updates coming from the control itself until initialized.
    if (!m_transformer || muse::is_equal(m_warpedValue, value)) {
        return;
    }

    m_warpedValue = value;
    emit warpedValueChanged();

    m_value = m_transformer->inverse(m_warpedValue);
    emit valueChanged();
}

double ValueWarper::min() const
{
    return m_min;
}

void ValueWarper::setMin(double value)
{
    if (muse::is_equal(m_min, value)) {
        return;
    }

    m_min = value;
    emit minChanged();

    if (!m_transformer) {
        return;
    }

    if (useWarper()) {
        m_transformer = std::make_unique<WarpingTransformer>(m_min, m_middle.toDouble(), m_max);
    }

    m_warpedValue = m_transformer->forward(m_value);
    emit warpedValueChanged();
}

double ValueWarper::max() const
{
    return m_max;
}

void ValueWarper::setMax(double value)
{
    if (muse::is_equal(m_max, value)) {
        return;
    }

    m_max = value;
    emit maxChanged();

    if (!m_transformer) {
        return;
    }

    if (useWarper()) {
        m_transformer = std::make_unique<WarpingTransformer>(m_min, m_middle.toDouble(), m_max);
    }

    m_warpedValue = m_transformer->forward(m_value);
    emit warpedValueChanged();
}
}
