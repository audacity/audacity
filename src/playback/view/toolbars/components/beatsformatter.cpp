/*
* Audacity: A Digital Audio Editor
*/
#include "beatsformatter.h"

#include <cstdio>

#include "log.h"
#include "translation.h"

using namespace au::playback;

static constexpr size_t pow10(int power)
{
    size_t result = 1;
    for (int i = 0; i < power; ++i) {
        result *= 10;
    }
    return result;
}

static constexpr std::array<size_t, 3> MIN_DIGITS{3, 2, 2};
static constexpr std::array<size_t, 3> UPPER_BOUNDS{
    pow10(MIN_DIGITS[0] - 1) + 1,
    pow10(MIN_DIGITS[1] - 1) + 1,
    pow10(MIN_DIGITS[2] - 1) + 1
};

static QString barString()
{
    return muse::qtrc("playback", "bar");
}

static QString beatString()
{
    return muse::qtrc("playback", "beat");
}

BeatsFormatter::BeatsFormatter(const QString& formatStr, int fracPart) : TimecodeFormatter(formatStr), m_fracPart(fracPart)
{
}

void BeatsFormatter::init()
{
    const bool formatOk = checkField(1, m_upperTimeSignature) && checkFracField(m_lowerTimeSignature);

    // 1/4 = BPM is used for now
    const auto quarterLength = 60.0 / m_tempo;
    const auto beatLength = quarterLength * 4.0 / m_lowerTimeSignature;
    const auto barLength = m_upperTimeSignature * beatLength;

    m_fieldLengths[0] = barLength;
    m_fieldLengths[1] = beatLength;

    const auto hasFracPart = m_fracPart > m_lowerTimeSignature;

    if (hasFracPart) {
        const auto fracLength = beatLength * m_lowerTimeSignature / m_fracPart;
        m_fieldLengths[2] = fracLength;
    }

    if (formatOk) {
        return;
    }

    updateFields(MIN_DIGITS[0]);
}

BeatsFormatter::ConversionResult BeatsFormatter::valueToString(double value, bool nearest) const
{
    UNUSED(nearest);

    ConversionResult result;
    result.fieldValueStrings.resize(m_fields.size());

    if (value < 0) {
        for (size_t fieldIndex = 0; fieldIndex < m_fields.size(); ++fieldIndex) {
            const auto digitsCount = m_fields[fieldIndex].digits;
            auto& fieldValue = result.fieldValueStrings[fieldIndex];
            for (size_t digitIndex = 0; digitIndex < digitsCount; ++digitIndex) {
                fieldValue += "-";
            }
        }

        updateResultString(result);

        return result;
    }

    // Calculate the epsilon only once, so the total loss of precision is addressed.
    // This is a "multiplicative" epsilon, so there is no need to calculate 1 + eps every time.
    const auto eps = 1.0 + std::max(1.0, value) * std::numeric_limits<double>::epsilon();

    for (size_t fieldIndex = 0; fieldIndex < m_fields.size(); ++fieldIndex) {
        const auto fieldLength = m_fieldLengths[fieldIndex];
        const auto fieldValue = std::max(0, static_cast<int>(std::floor(value * eps / fieldLength)));

        char field[10];
        snprintf(field, sizeof(field), m_fields[fieldIndex].formatStr.toStdString().c_str(), (int)(fieldValue + m_fieldValueOffset));

        result.fieldValueStrings[fieldIndex] = field;

        value = value - fieldValue * fieldLength;
    }

    updateResultString(result);
    return result;
}

std::optional<double> BeatsFormatter::stringToValue(const QString& value) const
{
    if (m_fields.size() > 0 && value.mid(m_fields[0].pos, 1) == '-') {
        return std::nullopt;
    }

    double t = 0.0;
    size_t lastIndex = 0;

    for (size_t i = 0; i < m_fields.size(); i++) {
        const auto& field = m_fields[i];

        const size_t labelIndex = field.label.isEmpty() ? muse::nidx : value.indexOf(field.label, lastIndex);

        const auto fieldStringValue = value.mid(lastIndex, labelIndex == muse::nidx ? labelIndex : labelIndex - lastIndex);

        bool ok = false;
        long val = fieldStringValue.toLong(&ok);

        if (!ok) {
            return std::nullopt;
        }

        t += (val - m_fieldValueOffset) * m_fieldLengths[i];

        lastIndex = labelIndex + field.label.size();
    }

    // Beats formatter does not support negative values
    t = std::max(0.0, t);

    return t;
}

double BeatsFormatter::singleStep(double value, int digitIndex, bool upwards)
{
    if (digitIndex < 0 || size_t(digitIndex) >= m_digits.size()) {
        return value;
    }

    const auto& digit = m_digits[digitIndex];
    const auto& fieldIndex = digit.field;
    const auto& field = m_fields[fieldIndex];

    const auto stepSize = m_fieldLengths[fieldIndex] * std::pow(10, field.digits - digit.index - 1);

    return upwards ? value + stepSize : value - stepSize;
}

void BeatsFormatter::updateResultString(ConversionResult& result) const
{
    for (size_t fieldIndex = 0; fieldIndex < m_fields.size(); ++fieldIndex) {
        result.valueString += result.fieldValueStrings[fieldIndex] + m_fields[fieldIndex].label;
    }
}

//! Check that field exists and has enough digits to fit the value
bool BeatsFormatter::checkField(size_t fieldIndex, int value) const
{
    if (fieldIndex >= m_fields.size()) {
        return false;
    }

    const auto digitsCount = m_fields[fieldIndex].digits;

    // Format always allows at least two digits
    const auto lowerRange = digitsCount > MIN_DIGITS[fieldIndex] ? pow10(digitsCount - 1) : 0;

    const auto upperRange = pow10(digitsCount);

    return value >= int(lowerRange) && value < int(upperRange);
}

bool BeatsFormatter::checkFracField(int newLts) const
{
    if (m_fracPart > newLts) {
        return checkField(2, m_fracPart / m_lowerTimeSignature);
    } else {
        return m_fields.size() == 2;
    }
}

void BeatsFormatter::updateFields(size_t barsDigits)
{
    m_fields.clear();
    m_digits.clear();

    // Range is assumed to allow 999 bars.
    auto& barsField = m_fields.emplace_back(NumericField::withDigits(barsDigits));

    barsField.label = " " + barString() + " ";

    // Beats format is 1 based. For the time point "0" the expected output is
    // "1 bar 1 beat [1]" For this reason we use (uts + 1) as the "range". On
    // top of that, we want at least two digits to be shown. NumericField
    // accepts range as in [0, range), so add 1.

    auto& beatsField = m_fields.emplace_back(NumericField::range(std::max<size_t>(UPPER_BOUNDS[1], m_upperTimeSignature + 1)));

    beatsField.label = " " + beatString();

    const auto hasFracPart = m_fracPart > m_lowerTimeSignature;

    if (hasFracPart) {
        beatsField.label += " ";
        // See the reasoning above about the range
        m_fields.emplace_back(NumericField::range(std::max(11, m_fracPart / m_lowerTimeSignature + 1)));
    }

    // Fill the aux m_digits structure
    size_t pos = 0;
    for (size_t i = 0; i < m_fields.size(); i++) {
        m_fields[i].pos = pos;

        for (size_t j = 0; j < m_fields[i].digits; j++) {
            m_digits.push_back(DigitInfo{i, j, pos});
            pos++;
        }

        pos += m_fields[i].label.length();
    }
}
