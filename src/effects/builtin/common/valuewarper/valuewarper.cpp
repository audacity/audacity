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
    double forward(double x, double /* min */, double /* max */) const override { return x; }
    double inverse(double y, double /* min */, double /* max */) const override { return y; }
};
}

void ValueWarper::init()
{
    IF_ASSERT_FAILED(!m_transformer) {
        LOGW() << "ValueWarper already initialized";
    }

    if (m_warpingType == ValueWarpingType::None) {
        m_transformer = std::make_unique<PassthroughTransformer>();
    } else {
        m_transformer = std::make_unique<WarpingTransformer>(m_warpingType);
    }

    m_warpedValue = m_transformer->forward(m_value, m_min, m_max);
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
        m_warpedValue = m_transformer->forward(m_value, m_min, m_max);
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

    m_value = m_transformer->inverse(m_warpedValue, m_min, m_max);
    emit valueChanged();
}

ValueWarpingType ValueWarper::warpingType() const
{
    return m_warpingType;
}

void ValueWarper::setWarpingType(ValueWarpingType type)
{
    if (m_warpingType == type) {
        return;
    }

    m_warpingType = type;
    emit warpingTypeChanged();

    if (!m_transformer) {
        // Not initialized yet, no need to change the impl on the fly.
        return;
    }

    if (type == ValueWarpingType::None) {
        m_transformer = std::make_unique<PassthroughTransformer>();
    } else {
        m_transformer = std::make_unique<WarpingTransformer>(type);
    }
    m_warpedValue = m_transformer->forward(m_value, m_min, m_max);
    emit warpedValueChanged();
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

    if (m_transformer) {
        m_warpedValue = m_transformer->forward(m_value, m_min, m_max);
        emit warpedValueChanged();
    }
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

    if (m_transformer) {
        m_warpedValue = m_transformer->forward(m_value, m_min, m_max);
        emit warpedValueChanged();
    }
}
}
