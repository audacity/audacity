/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "../common/builtineffectmodel.h"

namespace au::effects {
class ChangePitchEffect;
class ChangePitchViewModel : public BuiltinEffectModel
{
    Q_OBJECT

    Q_PROPERTY(int fromPitchValue READ fromPitchValue WRITE setFromPitchValue NOTIFY fromPitchValueChanged FINAL)
    Q_PROPERTY(int fromOctaveValue READ fromOctaveValue WRITE setFromOctaveValue NOTIFY fromOctaveValueChanged FINAL)
    Q_PROPERTY(int toPitchValue READ toPitchValue WRITE setToPitchValue NOTIFY toPitchValueChanged FINAL)
    Q_PROPERTY(int toOctaveValue READ toOctaveValue WRITE setToOctaveValue NOTIFY toOctaveValueChanged FINAL)
    Q_PROPERTY(double semitonesValue READ semitonesValue WRITE setSemitonesValue NOTIFY semitonesValueChanged FINAL)
    Q_PROPERTY(int semitonesIntegerValue READ semitonesIntegerValue WRITE setSemitonesIntegerValue NOTIFY semitonesIntegerValueChanged FINAL)
    Q_PROPERTY(int centsValue READ centsValue WRITE setCentsValue NOTIFY centsValueChanged FINAL)
    Q_PROPERTY(double fromFrequencyValue READ fromFrequencyValue WRITE setFromFrequencyValue NOTIFY fromFrequencyValueChanged FINAL)
    Q_PROPERTY(double toFrequencyValue READ toFrequencyValue WRITE setToFrequencyValue NOTIFY toFrequencyValueChanged FINAL)
    Q_PROPERTY(double percentChangeValue READ percentChangeValue WRITE setPercentChangeValue NOTIFY percentChangeValueChanged FINAL)
    Q_PROPERTY(bool useSBSMSValue READ useSBSMSValue WRITE setUseSBSMSValue NOTIFY useSBSMSValueChanged FINAL)
    Q_PROPERTY(QString estimatedStartPitch READ estimatedStartPitch NOTIFY estimatedStartPitchChanged FINAL)

public:
    ChangePitchViewModel() = default;

    int fromPitchValue() const;
    void setFromPitchValue(int newFromPitch);

    int fromOctaveValue() const;
    void setFromOctaveValue(int newFromOctave);

    int toPitchValue() const;
    void setToPitchValue(int newToPitch);

    int toOctaveValue() const;
    void setToOctaveValue(int newToOctave);

    double semitonesValue() const;
    void setSemitonesValue(double newSemitones);

    double fromFrequencyValue() const;
    void setFromFrequencyValue(double newFromFrequency);

    double toFrequencyValue() const;
    void setToFrequencyValue(double newToFrequency);

    double percentChangeValue() const;
    void setPercentChangeValue(double newPercentChange);

    bool useSBSMSValue() const;
    void setUseSBSMSValue(bool newUseSBSMS);

    QString estimatedStartPitch() const;

    Q_INVOKABLE QString effectTitle() const;

    Q_INVOKABLE QString fromPitchLabel() const;
    Q_INVOKABLE QVariantList fromPitchModel() const;

    Q_INVOKABLE int fromOctaveMin() const;
    Q_INVOKABLE int fromOctaveMax() const;
    Q_INVOKABLE int fromOctaveStep() const;
    Q_INVOKABLE int fromOctaveDecimals() const;
    Q_INVOKABLE QString fromOctaveUnitSymbol() const;

    Q_INVOKABLE QString toPitchLabel() const;
    Q_INVOKABLE QVariantList toPitchModel() const;

    Q_INVOKABLE int toOctaveMin() const;
    Q_INVOKABLE int toOctaveMax() const;
    Q_INVOKABLE int toOctaveStep() const;
    Q_INVOKABLE int toOctaveDecimals() const;
    Q_INVOKABLE QString toOctaveUnitSymbol() const;

    int semitonesIntegerValue() const;
    void setSemitonesIntegerValue(int value);

    int centsValue() const;
    void setCentsValue(int value);

    Q_INVOKABLE QString semitonesLabel() const;
    Q_INVOKABLE int semitonesMin() const;
    Q_INVOKABLE int semitonesMax() const;
    Q_INVOKABLE int semitonesStep() const;
    Q_INVOKABLE int semitonesDecimals() const;
    Q_INVOKABLE QString semitonesUnitSymbol() const;

    Q_INVOKABLE QString centsLabel() const;
    Q_INVOKABLE int centsMin() const;
    Q_INVOKABLE int centsMax() const;
    Q_INVOKABLE int centsStep() const;
    Q_INVOKABLE int centsDecimals() const;
    Q_INVOKABLE QString centsUnitSymbol() const;

    Q_INVOKABLE QString fromFrequencyLabel() const;
    Q_INVOKABLE double fromFrequencyMin() const;
    Q_INVOKABLE double fromFrequencyMax() const;
    Q_INVOKABLE double fromFrequencyStep() const;
    Q_INVOKABLE int fromFrequencyDecimals() const;
    Q_INVOKABLE QString fromFrequencyUnitSymbol() const;

    Q_INVOKABLE QString toFrequencyLabel() const;
    Q_INVOKABLE double toFrequencyMin() const;
    Q_INVOKABLE double toFrequencyMax() const;
    Q_INVOKABLE double toFrequencyStep() const;
    Q_INVOKABLE int toFrequencyDecimals() const;
    Q_INVOKABLE QString toFrequencyUnitSymbol() const;

    Q_INVOKABLE QString percentChangeLabel() const;
    Q_INVOKABLE double percentChangeMin() const;
    Q_INVOKABLE double percentChangeMax() const;
    Q_INVOKABLE double percentChangeStep() const;
    Q_INVOKABLE int percentChangeDecimals() const;
    Q_INVOKABLE QString percentChangeUnitSymbol() const;

    Q_INVOKABLE QString useSBSMSLabel() const;
    Q_INVOKABLE bool useSBSMSEnabled() const;

signals:
    void estimatedStartPitchChanged();
    void fromPitchValueChanged();
    void fromOctaveValueChanged();
    void toPitchValueChanged();
    void toOctaveValueChanged();
    void semitonesValueChanged();
    void semitonesIntegerValueChanged();
    void centsValueChanged();
    void fromFrequencyValueChanged();
    void toFrequencyValueChanged();
    void percentChangeValueChanged();
    void useSBSMSValueChanged();
private:
    void doReload() override;

    QStringList pitchChoices() const;
};
}
