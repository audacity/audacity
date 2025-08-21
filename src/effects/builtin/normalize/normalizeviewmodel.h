/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../common/builtineffectmodel.h"
#include "../common/params.h"

namespace au::effects {
class NormalizeEffect;
class NormalizeViewModel : public BuiltinEffectModel
{
    Q_OBJECT
    Q_PROPERTY(bool removeDC READ removeDC WRITE setRemoveDC NOTIFY removeDCChanged FINAL)
    Q_PROPERTY(
        bool normalizePeakAmplitude READ normalizePeakAmplitude WRITE setNormalizePeakAmplitude NOTIFY normalizePeakAmplitudeChanged FINAL)
    Q_PROPERTY(
        bool normalizeStereoChannelsIndependently READ normalizeStereoChannelsIndependently WRITE setNormalizeStereoChannelsIndependently NOTIFY normalizeStereoChannelsIndependentlyChanged FINAL)
    Q_PROPERTY(double peakAmplitudeTarget READ peakAmplitudeTarget WRITE setPeakAmplitudeTarget NOTIFY peakAmplitudeTargetChanged FINAL)

public:
    NormalizeViewModel() = default;

    bool removeDC() const;
    void setRemoveDC(bool removeDC);

    bool normalizePeakAmplitude() const;
    void setNormalizePeakAmplitude(bool normalizePeakAmplitude);

    bool normalizeStereoChannelsIndependently() const;
    void setNormalizeStereoChannelsIndependently(bool normalizeStereoChannelsIndependently);

    double peakAmplitudeTarget() const;
    void setPeakAmplitudeTarget(double peakAmplitudeTarget);

signals:
    void removeDCChanged();
    void normalizePeakAmplitudeChanged();
    void normalizeStereoChannelsIndependentlyChanged();
    void peakAmplitudeTargetChanged();

private:
    void doReload() override;
};
}
