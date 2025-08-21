/*
* Audacity: A Digital Audio Editor
*/
#include "clickremovalviewmodel.h"
#include "clickremovaleffect.h"

#include "global/log.h"
#include "global/translation.h"

namespace au::effects {
QString ClickRemovalViewModel::effectTitle() const
{
    return muse::qtrc("effects/clickremoval", "Click removal");
}

QString ClickRemovalViewModel::thresholdLabel() const
{
    return muse::qtrc("effects/clickremoval", "Threshold (lower is more sensitive)");
}

int ClickRemovalViewModel::thresholdValue() const
{
    const auto& ce = effect<ClickRemovalEffect>();
    return ce.mThresholdLevel;
}

void ClickRemovalViewModel::setThresholdValue(int newThresholdValue)
{
    auto& ce = effect<ClickRemovalEffect>();
    if (!muse::is_equal(ce.mThresholdLevel, newThresholdValue)) {
        ce.mThresholdLevel = newThresholdValue;
        emit thresholdValueChanged();
    }
}

int ClickRemovalViewModel::thresholdMin() const
{
    return ClickRemovalEffect::Threshold.min;
}

int ClickRemovalViewModel::thresholdMax() const
{
    return ClickRemovalEffect::Threshold.max;
}

int ClickRemovalViewModel::thresholdStep() const
{
    return ClickRemovalEffect::Threshold.step;
}

int ClickRemovalViewModel::thresholdDecimals() const
{
    return 0;
}

QString ClickRemovalViewModel::widthLabel() const
{
    return muse::qtrc("effects/clickremoval", "Max spike width (higher is more sensitive)");
}

int ClickRemovalViewModel::widthValue() const
{
    const auto& ce = effect<ClickRemovalEffect>();
    return ce.mClickWidth;
}

void ClickRemovalViewModel::setWidthValue(int newWidthValue)
{
    auto& ce = effect<ClickRemovalEffect>();
    if (!muse::is_equal(ce.mClickWidth, newWidthValue)) {
        ce.mClickWidth = newWidthValue;
        emit widthValueChanged();
    }
}

int ClickRemovalViewModel::widthMin() const
{
    return ClickRemovalEffect::Width.min;
}

int ClickRemovalViewModel::widthMax() const
{
    return ClickRemovalEffect::Width.max;
}

int ClickRemovalViewModel::widthStep() const
{
    return ClickRemovalEffect::Width.step;
}

int ClickRemovalViewModel::widthDecimals() const
{
    return 0;
}

void ClickRemovalViewModel::doReload()
{
    emit thresholdValueChanged();
    emit widthValueChanged();
}
}
