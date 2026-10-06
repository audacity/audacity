/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/builtin/qml/Audacity/BuiltinEffects/builtineffectmodel.h"

#include <QVariantList>
#include <QVariantMap>

namespace au::effects {
class AutoDuckEffect;

//! Presents the fade lengths of the effect as signed offsets from the start
//! and end of the ducked region, as they appear on the graph: the duck starts
//! before the region starts, and the recovery starts before it ends.
class AutoDuckViewModel : public BuiltinEffectModel
{
    Q_OBJECT

    Q_PROPERTY(double duckStart READ duckStart WRITE setDuckStart NOTIFY paramsChanged FINAL)
    Q_PROPERTY(double duckEnd READ duckEnd WRITE setDuckEnd NOTIFY paramsChanged FINAL)
    Q_PROPERTY(double recoveryStart READ recoveryStart WRITE setRecoveryStart NOTIFY paramsChanged FINAL)
    Q_PROPERTY(double recoveryEnd READ recoveryEnd WRITE setRecoveryEnd NOTIFY paramsChanged FINAL)
    Q_PROPERTY(double gainReduction READ gainReduction WRITE setGainReduction NOTIFY paramsChanged FINAL)
    Q_PROPERTY(double threshold READ threshold WRITE setThreshold NOTIFY paramsChanged FINAL)
    Q_PROPERTY(double hold READ hold WRITE setHold NOTIFY paramsChanged FINAL)

    //! The fade lengths share the same range.
    Q_PROPERTY(double fadeLengthMax READ fadeLengthMax CONSTANT FINAL)
    Q_PROPERTY(double gainReductionMin READ gainReductionMin CONSTANT FINAL)
    Q_PROPERTY(double gainReductionMax READ gainReductionMax CONSTANT FINAL)
    Q_PROPERTY(double thresholdMin READ thresholdMin CONSTANT FINAL)
    Q_PROPERTY(double thresholdMax READ thresholdMax CONSTANT FINAL)
    Q_PROPERTY(double holdMax READ holdMax CONSTANT FINAL)
    //! Default values, keyed by the name of the property they belong to
    Q_PROPERTY(QVariantMap defaults READ defaults CONSTANT FINAL)

    Q_PROPERTY(QVariantList controlTrackOptions READ controlTrackOptions NOTIFY controlTrackChanged FINAL)
    Q_PROPERTY(int controlTrackIndex READ controlTrackIndex WRITE setControlTrackIndex NOTIFY controlTrackChanged FINAL)
    Q_PROPERTY(bool hasControlTrack READ hasControlTrack NOTIFY controlTrackChanged FINAL)

public:
    AutoDuckViewModel(QObject* parent, int instanceId);
    ~AutoDuckViewModel() override = default;

    double duckStart() const;
    void setDuckStart(double value);
    double duckEnd() const;
    void setDuckEnd(double value);
    double recoveryStart() const;
    void setRecoveryStart(double value);
    double recoveryEnd() const;
    void setRecoveryEnd(double value);
    double gainReduction() const;
    void setGainReduction(double value);
    double threshold() const;
    void setThreshold(double value);
    double hold() const;
    void setHold(double value);

    double fadeLengthMax() const;
    double gainReductionMin() const;
    double gainReductionMax() const;
    double thresholdMin() const;
    double thresholdMax() const;
    double holdMax() const;
    QVariantMap defaults() const;

    QVariantList controlTrackOptions() const;
    int controlTrackIndex() const;
    void setControlTrackIndex(int index);
    bool hasControlTrack() const;

signals:
    void paramsChanged();
    void controlTrackChanged();

private:
    void doReload() override;
    void setParam(double AutoDuckEffect::* member, double value);
};

class AutoDuckViewModelFactory : public EffectViewModelFactory<AutoDuckViewModel>
{
};
}
