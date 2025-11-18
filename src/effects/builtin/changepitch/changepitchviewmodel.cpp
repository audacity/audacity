/*
 * Audacity: A Digital Audio Editor
 */
#include "changepitchviewmodel.h"
#include "changepitcheffect.h"

#include "../common/measureunits.h"
#include "libraries/lib-math/PitchName.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include <cmath>

using namespace au::effects;

QString ChangePitchViewModel::effectTitle() const
{
    return muse::qtrc("effects/changepitch", "Change Pitch");
}

QString ChangePitchViewModel::estimatedStartPitch() const
{
    const auto& e = effect<ChangePitchEffect>();
    QStringList pitchNames = pitchChoices();
    const QString pitchName = (e.m_nFromPitch >= 0 && e.m_nFromPitch < pitchNames.size())
                              ? pitchNames[e.m_nFromPitch]
                              : "C";
    const double frequency = e.m_FromFrequency;
    //: %1 = pitch name (e.g. C), %2 = octave number, %3 = frequency in Hz
    return muse::qtrc("effects/changepitch", "Estimated start pitch: %1%2 (%3 Hz)")
           .arg(pitchName)
           .arg(e.m_nFromOctave)
           .arg(QString::number(frequency, 'f', 3));
}

// From Pitch
QString ChangePitchViewModel::fromPitchLabel() const
{
    return muse::qtrc("effects/changepitch", "From pitch");
}

int ChangePitchViewModel::fromPitchValue() const
{
    const auto& e = effect<ChangePitchEffect>();
    return e.m_nFromPitch;
}

void ChangePitchViewModel::setFromPitchValue(const int newFromPitch)
{
    auto& e = effect<ChangePitchEffect>();
    if (e.m_nFromPitch == newFromPitch) {
        return;
    }

    e.m_nFromPitch = newFromPitch;
    e.m_bLoopDetect = true;

    // Update FROM frequency from pitch/octave
    const double midiNote = PitchToMIDInote(e.m_nFromPitch, e.m_nFromOctave);
    e.m_FromFrequency = MIDInoteToFreq(midiNote);

    // Recalculate relative values (semitones, percent) from [FROM -> TO] relationship
    e.Calc_SemitonesChange_fromPitches();
    e.Calc_PercentChange();

    e.m_bLoopDetect = false;

    emit fromPitchValueChanged();
    emit estimatedStartPitchChanged();
    emit fromFrequencyValueChanged();
    emit semitonesValueChanged();
    emit semitonesIntegerValueChanged();
    emit centsValueChanged();
    emit percentChangeValueChanged();
}

QStringList ChangePitchViewModel::pitchChoices() const
{
    QStringList choices;
    // Use PitchName from lib-math to get the pitch names with both sharps and flats
    for (int i = 0; i < 12; ++i) {
        // Convert pitch index to MIDI note (using octave 0 as reference)
        const double midiNote = PitchToMIDInote(i, 0);
        // Get the pitch name with both sharp and flat notation
        TranslatableString pitchName = PitchName(midiNote, PitchNameChoice::Both);
        // Convert to QString
        wxString wxStr = pitchName.Translation();
        QString qStr = QString::fromStdWString(wxStr.ToStdWstring());
        choices.append(qStr);
    }
    return choices;
}

QVariantList ChangePitchViewModel::fromPitchModel() const
{
    QStringList choices = pitchChoices();
    QVariantList model;
    for (int i = 0; i < choices.size(); ++i) {
        QVariantMap item;
        item["text"] = choices[i];
        item["value"] = i;
        model.append(item);
    }
    return model;
}

QVariantList ChangePitchViewModel::toPitchModel() const
{
    QStringList choices = pitchChoices();
    QVariantList model;
    for (int i = 0; i < choices.size(); ++i) {
        QVariantMap item;
        item["text"] = choices[i];
        item["value"] = i;
        model.append(item);
    }
    return model;
}

// From Octave
int ChangePitchViewModel::fromOctaveValue() const
{
    const auto& e = effect<ChangePitchEffect>();
    return e.m_nFromOctave;
}

void ChangePitchViewModel::setFromOctaveValue(const int newFromOctave)
{
    auto& e = effect<ChangePitchEffect>();
    if (e.m_nFromOctave == newFromOctave) {
        return;
    }

    e.m_nFromOctave = newFromOctave;
    e.m_bLoopDetect = true;

    // Update FROM frequency from pitch/octave
    const double midiNote = PitchToMIDInote(e.m_nFromPitch, e.m_nFromOctave);
    e.m_FromFrequency = MIDInoteToFreq(midiNote);

    // Recalculate relative values (semitones, percent) from [FROM -> TO] relationship
    e.Calc_SemitonesChange_fromPitches();
    e.Calc_PercentChange();

    e.m_bLoopDetect = false;

    emit fromOctaveValueChanged();
    emit estimatedStartPitchChanged();
    emit fromFrequencyValueChanged();
    emit semitonesValueChanged();
    emit semitonesIntegerValueChanged();
    emit centsValueChanged();
    emit percentChangeValueChanged();
}

int ChangePitchViewModel::fromOctaveMin() const
{
    return -1; // MIDI note 0 is C-1
}

int ChangePitchViewModel::fromOctaveMax() const
{
    return 9; // MIDI note 127 is G9
}

int ChangePitchViewModel::fromOctaveStep() const
{
    return 1;
}

int ChangePitchViewModel::fromOctaveDecimals() const
{
    return 0;
}

QString ChangePitchViewModel::fromOctaveUnitSymbol() const
{
    return units::none().m_symbol;
}

// To Pitch
QString ChangePitchViewModel::toPitchLabel() const
{
    return muse::qtrc("effects/changepitch", "To pitch");
}

int ChangePitchViewModel::toPitchValue() const
{
    const auto& e = effect<ChangePitchEffect>();
    return e.m_nToPitch;
}

void ChangePitchViewModel::setToPitchValue(const int newToPitch)
{
    auto& e = effect<ChangePitchEffect>();
    if (e.m_nToPitch == newToPitch) {
        return;
    }

    e.m_nToPitch = newToPitch;
    e.m_bLoopDetect = true;
    e.Calc_SemitonesChange_fromPitches();
    e.Calc_PercentChange();
    e.Calc_ToFrequency();
    e.m_bLoopDetect = false;

    emit toPitchValueChanged();
    emit semitonesValueChanged();
    emit semitonesIntegerValueChanged();
    emit centsValueChanged();
    emit percentChangeValueChanged();
    emit toFrequencyValueChanged();
}

// To Octave
int ChangePitchViewModel::toOctaveValue() const
{
    const auto& e = effect<ChangePitchEffect>();
    return e.m_nToOctave;
}

void ChangePitchViewModel::setToOctaveValue(const int newToOctave)
{
    auto& e = effect<ChangePitchEffect>();
    if (e.m_nToOctave == newToOctave) {
        return;
    }

    e.m_nToOctave = newToOctave;
    e.m_bLoopDetect = true;
    e.Calc_SemitonesChange_fromPitches();
    e.Calc_PercentChange();
    e.Calc_ToFrequency();
    e.m_bLoopDetect = false;

    emit toOctaveValueChanged();
    emit semitonesValueChanged();
    emit semitonesIntegerValueChanged();
    emit centsValueChanged();
    emit percentChangeValueChanged();
    emit toFrequencyValueChanged();
}

int ChangePitchViewModel::toOctaveMin() const
{
    return -1; // MIDI note 0 is C-1
}

int ChangePitchViewModel::toOctaveMax() const
{
    return 9; // MIDI note 127 is G9
}

int ChangePitchViewModel::toOctaveStep() const
{
    return 1;
}

int ChangePitchViewModel::toOctaveDecimals() const
{
    return 0;
}

QString ChangePitchViewModel::toOctaveUnitSymbol() const
{
    return units::none().m_symbol;
}

// Semitones
QString ChangePitchViewModel::semitonesLabel() const
{
    return muse::qtrc("effects/changepitch", "Semitones");
}

double ChangePitchViewModel::semitonesValue() const
{
    const auto& e = effect<ChangePitchEffect>();
    return e.m_dSemitonesChange;
}

void ChangePitchViewModel::setSemitonesValue(const double newSemitones)
{
    auto& e = effect<ChangePitchEffect>();
    if (e.m_dSemitonesChange == newSemitones) {
        return;
    }

    e.m_dSemitonesChange = newSemitones;
    e.m_bLoopDetect = true;
    e.Calc_ToPitch();
    e.Calc_ToOctave();
    e.Calc_PercentChange();
    e.Calc_ToFrequency();
    e.m_bLoopDetect = false;

    emit semitonesValueChanged();
    emit semitonesIntegerValueChanged();
    emit centsValueChanged();
    emit toPitchValueChanged();
    emit toOctaveValueChanged();
    emit percentChangeValueChanged();
    emit toFrequencyValueChanged();
}

int ChangePitchViewModel::semitonesIntegerValue() const
{
    const auto& e = effect<ChangePitchEffect>();
    return static_cast<int>(std::floor(e.m_dSemitonesChange));
}

void ChangePitchViewModel::setSemitonesIntegerValue(const int value)
{
    const int currentCents = centsValue();
    const double newSemitones = value + (currentCents / 100.0);
    setSemitonesValue(newSemitones);
}

int ChangePitchViewModel::semitonesMin() const
{
    return -12;
}

int ChangePitchViewModel::semitonesMax() const
{
    return 12;
}

int ChangePitchViewModel::semitonesStep() const
{
    return 1;
}

int ChangePitchViewModel::semitonesDecimals() const
{
    return 0;
}

QString ChangePitchViewModel::semitonesUnitSymbol() const
{
    return units::none().m_symbol;
}

QString ChangePitchViewModel::centsLabel() const
{
    return muse::qtrc("effects/changepitch", "Cents");
}

int ChangePitchViewModel::centsValue() const
{
    const auto& e = effect<ChangePitchEffect>();
    const double fractionalPart = e.m_dSemitonesChange - std::floor(e.m_dSemitonesChange);
    return static_cast<int>(std::round(fractionalPart * 100.0));
}

void ChangePitchViewModel::setCentsValue(const int value)
{
    const int currentSemitones = semitonesIntegerValue();
    const double newSemitones = currentSemitones + (value / 100.0);
    setSemitonesValue(newSemitones);
}

int ChangePitchViewModel::centsMin() const
{
    return 0;
}

int ChangePitchViewModel::centsMax() const
{
    return 99;
}

int ChangePitchViewModel::centsStep() const
{
    return 1;
}

int ChangePitchViewModel::centsDecimals() const
{
    return 0;
}

QString ChangePitchViewModel::centsUnitSymbol() const
{
    return units::none().m_symbol;
}

// From Frequency
QString ChangePitchViewModel::fromFrequencyLabel() const
{
    return muse::qtrc("effects/changepitch", "From frequency");
}

double ChangePitchViewModel::fromFrequencyValue() const
{
    auto& e = effect<ChangePitchEffect>();
    return e.m_FromFrequency;
}

void ChangePitchViewModel::setFromFrequencyValue(const double newFromFrequency)
{
    auto& e = effect<ChangePitchEffect>();

    if (e.m_FromFrequency == newFromFrequency) {
        return;
    }

    e.m_FromFrequency = newFromFrequency;
    e.m_bLoopDetect = true;

    // Update FROM pitch/octave from frequency
    const double midiNote = FreqToMIDInote(e.m_FromFrequency);
    e.m_nFromPitch = PitchIndex(midiNote);
    e.m_nFromOctave = PitchOctave(midiNote);

    // Calculate percent change directly from frequencies (not through semitones)
    // to avoid rounding errors when frequency changes don't cross semitone boundaries
    e.m_dPercentChange = ((e.m_ToFrequency / e.m_FromFrequency) - 1.0) * 100.0;

    // Calculate semitones from the percent change
    e.Calc_SemitonesChange_fromPercentChange();

    e.m_bLoopDetect = false;

    emit fromFrequencyValueChanged();
    emit fromPitchValueChanged();
    emit fromOctaveValueChanged();
    emit estimatedStartPitchChanged();
    emit semitonesValueChanged();
    emit semitonesIntegerValueChanged();
    emit centsValueChanged();
    emit percentChangeValueChanged();
}

double ChangePitchViewModel::fromFrequencyMin() const
{
    return 1.0;
}

double ChangePitchViewModel::fromFrequencyMax() const
{
    return 100000.0;
}

double ChangePitchViewModel::fromFrequencyStep() const
{
    return 1.0;
}

int ChangePitchViewModel::fromFrequencyDecimals() const
{
    return 3;
}

QString ChangePitchViewModel::fromFrequencyUnitSymbol() const
{
    return units::hertz().m_symbol;
}

// To Frequency
QString ChangePitchViewModel::toFrequencyLabel() const
{
    return muse::qtrc("effects/changepitch", "To frequency");
}

double ChangePitchViewModel::toFrequencyValue() const
{
    auto& e = effect<ChangePitchEffect>();
    return e.m_ToFrequency;
}

void ChangePitchViewModel::setToFrequencyValue(const double newToFrequency)
{
    auto& e = effect<ChangePitchEffect>();

    if (e.m_ToFrequency == newToFrequency) {
        return;
    }

    e.m_ToFrequency = newToFrequency;
    e.m_bLoopDetect = true;

    // Update pitch/octave from frequency
    double dToMIDInote = FreqToMIDInote(e.m_ToFrequency);
    e.m_nToPitch = PitchIndex(dToMIDInote);
    e.m_nToOctave = PitchOctave(dToMIDInote);

    // Calculate percent change directly from frequencies (not through semitones)
    // to avoid rounding errors when frequency changes don't cross semitone boundaries
    e.m_dPercentChange = ((e.m_ToFrequency / e.m_FromFrequency) - 1.0) * 100.0;

    // Calculate semitones from the percent change
    e.Calc_SemitonesChange_fromPercentChange();

    e.m_bLoopDetect = false;

    emit toFrequencyValueChanged();
    emit toPitchValueChanged();
    emit toOctaveValueChanged();
    emit semitonesValueChanged();
    emit semitonesIntegerValueChanged();
    emit centsValueChanged();
    emit percentChangeValueChanged();
}

double ChangePitchViewModel::toFrequencyMin() const
{
    return 1.0;
}

double ChangePitchViewModel::toFrequencyMax() const
{
    return 100000.0;
}

double ChangePitchViewModel::toFrequencyStep() const
{
    return 1.0;
}

int ChangePitchViewModel::toFrequencyDecimals() const
{
    return 3;
}

QString ChangePitchViewModel::toFrequencyUnitSymbol() const
{
    return units::hertz().m_symbol;
}

// Percent Change
QString ChangePitchViewModel::percentChangeLabel() const
{
    return muse::qtrc("effects/changepitch", "Percentage change");
}

double ChangePitchViewModel::percentChangeValue() const
{
    auto& e = effect<ChangePitchEffect>();
    return e.m_dPercentChange;
}

void ChangePitchViewModel::setPercentChangeValue(const double newPercentChange)
{
    auto& e = effect<ChangePitchEffect>();

    if (e.m_dPercentChange == newPercentChange) {
        return;
    }

    e.m_dPercentChange = newPercentChange;
    e.m_bLoopDetect = true;
    e.Calc_SemitonesChange_fromPercentChange();
    e.Calc_ToPitch();
    e.Calc_ToOctave();
    e.Calc_ToFrequency();
    e.m_bLoopDetect = false;

    emit percentChangeValueChanged();
    emit semitonesValueChanged();
    emit semitonesIntegerValueChanged();
    emit centsValueChanged();
    emit toPitchValueChanged();
    emit toOctaveValueChanged();
    emit toFrequencyValueChanged();
}

double ChangePitchViewModel::percentChangeMin() const
{
    return -99.0;
}

double ChangePitchViewModel::percentChangeMax() const
{
    return 3000.0;
}

double ChangePitchViewModel::percentChangeStep() const
{
    return 0.1;
}

int ChangePitchViewModel::percentChangeDecimals() const
{
    return 3;
}

QString ChangePitchViewModel::percentChangeUnitSymbol() const
{
    return units::none().m_symbol;
}

// Use SBSMS
QString ChangePitchViewModel::useSBSMSLabel() const
{
    return muse::qtrc("effects/changepitch", "Use high quality stretching (slow)");
}

bool ChangePitchViewModel::useSBSMSValue() const
{
    auto& e = effect<ChangePitchEffect>();
    return e.mUseSBSMS;
}

void ChangePitchViewModel::setUseSBSMSValue(const bool newUseSBSMS)
{
    auto& e = effect<ChangePitchEffect>();

    if (e.mUseSBSMS == newUseSBSMS) {
        return;
    }

    e.mUseSBSMS = newUseSBSMS;
    emit useSBSMSValueChanged();
}

bool ChangePitchViewModel::useSBSMSEnabled() const
{
#if USE_SBSMS
    return true;
#else
    return false;
#endif
}

void ChangePitchViewModel::doReload()
{
    // Deduce the start frequency from the audio selection
    auto& e = effect<ChangePitchEffect>();
    e.DeduceFrequencies();

    emit estimatedStartPitchChanged();
    emit fromPitchValueChanged();
    emit fromOctaveValueChanged();
    emit toPitchValueChanged();
    emit toOctaveValueChanged();
    emit semitonesValueChanged();
    emit semitonesIntegerValueChanged();
    emit centsValueChanged();
    emit fromFrequencyValueChanged();
    emit toFrequencyValueChanged();
    emit percentChangeValueChanged();
    emit useSBSMSValueChanged();
}
