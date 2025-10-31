/*
* Audacity: A Digital Audio Editor
*/
#include "truncatesilenceviewmodel.h"
#include "truncatesilenceeffect.h"

#include "framework/global/log.h"
#include "framework/global/translation.h"

namespace au::effects {
QString TruncateSilenceViewModel::effectTitle() const
{
    return muse::qtrc("effects/truncatesilence", "Truncate silence");
}

QString TruncateSilenceViewModel::detectSilenceLabel() const
{
    return muse::qtrc("effects/truncatesilence", "Detect silence");
}

QString TruncateSilenceViewModel::thresholdLabel() const
{
    return muse::qtrc("effects/truncatesilence", "Threshold");
}

double TruncateSilenceViewModel::thresholdValue() const
{
    const auto& tse = effect<TruncateSilenceEffect>();
    return tse.mThresholdDB;
}

void TruncateSilenceViewModel::setThresholdValue(double newThresholdValue)
{
    auto& tse = effect<TruncateSilenceEffect>();
    if (!muse::is_equal(tse.mThresholdDB, newThresholdValue)) {
        tse.mThresholdDB = newThresholdValue;
        emit thresholdValueChanged();
    }
}

double TruncateSilenceViewModel::thresholdMin() const
{
    return TruncateSilenceEffect::Threshold.min;
}

double TruncateSilenceViewModel::thresholdMax() const
{
    return TruncateSilenceEffect::Threshold.max;
}

double TruncateSilenceViewModel::thresholdStep() const
{
    return TruncateSilenceEffect::Threshold.step;
}

int TruncateSilenceViewModel::thresholdDecimals() const
{
    return 1;
}

QString TruncateSilenceViewModel::thresholdUnit() const
{
    return dbUnit();
}

QString TruncateSilenceViewModel::minimumLabel() const
{
    return muse::qtrc("effects/truncatesilence", "Duration");
}

double TruncateSilenceViewModel::minimumValue() const
{
    const auto& tse = effect<TruncateSilenceEffect>();
    return tse.mInitialAllowedSilence;
}

void TruncateSilenceViewModel::setMinimumValue(double newMinimumValue)
{
    auto& tse = effect<TruncateSilenceEffect>();
    if (!muse::is_equal(tse.mInitialAllowedSilence, newMinimumValue)) {
        tse.mInitialAllowedSilence = newMinimumValue;
        emit minimumValueChanged();
    }
}

double TruncateSilenceViewModel::minimumMin() const
{
    return TruncateSilenceEffect::Minimum.min;
}

double TruncateSilenceViewModel::minimumMax() const
{
    return TruncateSilenceEffect::Minimum.max;
}

double TruncateSilenceViewModel::minimumStep() const
{
    return TruncateSilenceEffect::Minimum.step;
}

int TruncateSilenceViewModel::minimumDecimals() const
{
    return 3;
}

QString TruncateSilenceViewModel::minimumUnit() const
{
    return secondsUnit();
}

QString TruncateSilenceViewModel::actionLabel() const
{
    return muse::qtrc("effects/truncatesilence", "Action");
}

int TruncateSilenceViewModel::actionIndex() const
{
    const auto& tse = effect<TruncateSilenceEffect>();
    return tse.mActionIndex;
}

void TruncateSilenceViewModel::setActionIndex(int newActionIndex)
{
    auto& tse = effect<TruncateSilenceEffect>();
    if (tse.mActionIndex != newActionIndex) {
        tse.mActionIndex = newActionIndex;
        emit actionIndexChanged();
    }
}

QVariantList TruncateSilenceViewModel::actionModel() const
{
    return {
        QVariantMap { { "text", truncateActionLabel() }, { "value", 0 } },
        QVariantMap { { "text", compressActionLabel() }, { "value", 1 } }
    };
}

QString TruncateSilenceViewModel::truncateToLabel() const
{
    return muse::qtrc("effects/truncatesilence", "Truncate to");
}

QString TruncateSilenceViewModel::truncateActionLabel() const
{
    return muse::qtrc("effects/truncatesilence", "Truncate detected silence");
}

double TruncateSilenceViewModel::truncateValue() const
{
    const auto& tse = effect<TruncateSilenceEffect>();
    return tse.mTruncLongestAllowedSilence;
}

void TruncateSilenceViewModel::setTruncateValue(double newTruncateValue)
{
    auto& tse = effect<TruncateSilenceEffect>();
    if (!muse::is_equal(tse.mTruncLongestAllowedSilence, newTruncateValue)) {
        tse.mTruncLongestAllowedSilence = newTruncateValue;
        emit truncateValueChanged();
    }
}

double TruncateSilenceViewModel::truncateMin() const
{
    return TruncateSilenceEffect::Truncate.min;
}

double TruncateSilenceViewModel::truncateMax() const
{
    return TruncateSilenceEffect::Truncate.max;
}

double TruncateSilenceViewModel::truncateStep() const
{
    return TruncateSilenceEffect::Truncate.step;
}

int TruncateSilenceViewModel::truncateDecimals() const
{
    return 3;
}

QString TruncateSilenceViewModel::truncateUnit() const
{
    return secondsUnit();
}

QString TruncateSilenceViewModel::compressToLabel() const
{
    return muse::qtrc("effects/truncatesilence", "Compress to");
}

QString TruncateSilenceViewModel::compressActionLabel() const
{
    return muse::qtrc("effects/truncatesilence", "Compress excess silence");
}

double TruncateSilenceViewModel::compressValue() const
{
    const auto& tse = effect<TruncateSilenceEffect>();
    return tse.mSilenceCompressPercent;
}

void TruncateSilenceViewModel::setCompressValue(double newCompressValue)
{
    auto& tse = effect<TruncateSilenceEffect>();
    if (!muse::is_equal(tse.mSilenceCompressPercent, newCompressValue)) {
        tse.mSilenceCompressPercent = newCompressValue;
        emit compressValueChanged();
    }
}

double TruncateSilenceViewModel::compressMin() const
{
    return TruncateSilenceEffect::Compress.min;
}

double TruncateSilenceViewModel::compressMax() const
{
    return TruncateSilenceEffect::Compress.max;
}

double TruncateSilenceViewModel::compressStep() const
{
    return TruncateSilenceEffect::Compress.step;
}

int TruncateSilenceViewModel::compressDecimals() const
{
    return 1;
}

QString TruncateSilenceViewModel::compressUnit() const
{
    return percentUnit();
}

QString TruncateSilenceViewModel::independentTruncateLabel() const
{
    return muse::qtrc("effects/truncatesilence", "Truncate tracks independently");
}

QString TruncateSilenceViewModel::independentCompressLabel() const
{
    return muse::qtrc("effects/truncatesilence", "Compress tracks independently");
}

bool TruncateSilenceViewModel::independentValue() const
{
    const auto& tse = effect<TruncateSilenceEffect>();
    return tse.mbIndependent;
}

void TruncateSilenceViewModel::setIndependentValue(bool newIndependentValue)
{
    auto& tse = effect<TruncateSilenceEffect>();
    if (tse.mbIndependent != newIndependentValue) {
        tse.mbIndependent = newIndependentValue;
        emit independentValueChanged();
    }
}

void TruncateSilenceViewModel::doReload()
{
    emit thresholdValueChanged();
    emit actionIndexChanged();
    emit minimumValueChanged();
    emit truncateValueChanged();
    emit compressValueChanged();
    emit independentValueChanged();
}

// Internal unit helpers
QString TruncateSilenceViewModel::dbUnit() const
{
    return muse::qtrc("effects/truncatesilence", "dB");
}

QString TruncateSilenceViewModel::secondsUnit() const
{
    return muse::qtrc("effects/truncatesilence", "seconds");
}

QString TruncateSilenceViewModel::percentUnit() const
{
    return muse::qtrc("effects/truncatesilence", "%");
}
}
