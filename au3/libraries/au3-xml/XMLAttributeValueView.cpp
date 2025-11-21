/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLAttributeValueView.cpp

  Dmitry Vedenko

**********************************************************************/

#include "XMLAttributeValueView.h"
#include "FromChars.h"

#include <numeric>

XMLAttributeValueView::XMLAttributeValueView(bool value) noexcept
    : mInteger(value)
    , mType(Type::UnsignedInteger)
{
}

XMLAttributeValueView::XMLAttributeValueView(short value) noexcept
    : mInteger(value)
    , mType(Type::SignedInteger)
{
}

XMLAttributeValueView::XMLAttributeValueView(unsigned short value) noexcept
    : mInteger(value)
    , mType(Type::UnsignedInteger)
{
}

XMLAttributeValueView::XMLAttributeValueView(int value) noexcept
    : mInteger(value)
    , mType(Type::SignedInteger)
{
}

XMLAttributeValueView::XMLAttributeValueView(unsigned int value) noexcept
    : mInteger(value)
    , mType(Type::UnsignedInteger)
{
}

XMLAttributeValueView::XMLAttributeValueView(long value) noexcept
    : mInteger(value)
    , mType(Type::SignedInteger)
{
}

XMLAttributeValueView::XMLAttributeValueView(unsigned long value) noexcept
    : mInteger(value)
    , mType(Type::UnsignedInteger)
{
}

XMLAttributeValueView::XMLAttributeValueView(long long value) noexcept
    : mInteger(value)
    , mType(Type::SignedInteger)
{
}

XMLAttributeValueView::XMLAttributeValueView(unsigned long long value) noexcept
    : mInteger(value)
    , mType(Type::UnsignedInteger)
{
}

XMLAttributeValueView::XMLAttributeValueView(float value) noexcept
    : mFloat(value)
    , mType(Type::Float)
{
}

XMLAttributeValueView::XMLAttributeValueView(double value) noexcept
    : mDouble(value)
    , mType(Type::Double)
{
}

XMLAttributeValueView::XMLAttributeValueView(
    const std::string_view& value) noexcept
    : mType(Type::StringView)
{
    mStringView.Data = value.data();
    mStringView.Length = value.length();
}

XMLAttributeValueView::Type XMLAttributeValueView::GetType() const noexcept
{
    return mType;
}

bool XMLAttributeValueView::IsNull() const noexcept
{
    return mType == Type::Null;
}

bool XMLAttributeValueView::IsSignedInteger() const noexcept
{
    return mType == Type::SignedInteger;
}

bool XMLAttributeValueView::IsUnsignedInteger() const noexcept
{
    return mType == Type::UnsignedInteger;
}

bool XMLAttributeValueView::IsFloat() const noexcept
{
    return mType == Type::Float;
}

bool XMLAttributeValueView::IsDouble() const noexcept
{
    return mType == Type::Double;
}

bool XMLAttributeValueView::IsStringView() const noexcept
{
    return mType == Type::StringView;
}

bool XMLAttributeValueView::TryGet(bool& value) const noexcept
{
    return TryGetInteger(value);
}

bool XMLAttributeValueView::TryGet(short& value) const noexcept
{
    return TryGetInteger(value);
}

bool XMLAttributeValueView::TryGet(unsigned short& value) const noexcept
{
    return TryGetInteger(value);
}

bool XMLAttributeValueView::TryGet(int& value) const noexcept
{
    return TryGetInteger(value);
}

bool XMLAttributeValueView::TryGet(unsigned int& value) const noexcept
{
    return TryGetInteger(value);
}

bool XMLAttributeValueView::TryGet(long& value) const noexcept
{
    return TryGetInteger(value);
}

bool XMLAttributeValueView::TryGet(unsigned long& value) const noexcept
{
    return TryGetInteger(value);
}

bool XMLAttributeValueView::TryGet(long long& value) const noexcept
{
    return TryGetInteger(value);
}

bool XMLAttributeValueView::TryGet(unsigned long long& value) const noexcept
{
    return TryGetInteger(value);
}

bool XMLAttributeValueView::TryGet(float& value) const noexcept
{
    if (mType == Type::Float) {
        value = mFloat;
        return true;
    } else if (mType == Type::SignedInteger || mType == Type::UnsignedInteger) {
        value = mInteger;
        return true;
    } else if (mType == Type::StringView) {
        const char* end = mStringView.Data + mStringView.Length;

        float tempValue = {};

        const auto result = FromChars(mStringView.Data, end, tempValue);

        if (result.ec == std::errc() && result.ptr == end) {
            value = tempValue;
            return true;
        }
    }

    return false;
}

bool XMLAttributeValueView::TryGet(double& value) const noexcept
{
    if (mType == Type::Float) {
        value = mFloat;
        return true;
    } else if (mType == Type::Double) {
        value = mDouble;
        return true;
    } else if (mType == Type::SignedInteger || mType == Type::UnsignedInteger) {
        value = mInteger;
        return true;
    } else if (mType == Type::StringView) {
        const char* end = mStringView.Data + mStringView.Length;

        double tempValue = {};

        const auto result = FromChars(mStringView.Data, end, tempValue);

        if (result.ec == std::errc() && result.ptr == end) {
            value = tempValue;
            return true;
        }
    }

    return false;
}

bool XMLAttributeValueView::TryGet(std::string_view& value) const noexcept
{
    if (mType != Type::StringView) {
        return false;
    }

    value = std::string_view(mStringView.Data, mStringView.Length);

    return true;
}

std::string XMLAttributeValueView::ToString() const
{
    switch (mType) {
    case XMLAttributeValueView::Type::Null:
        return {};
    case XMLAttributeValueView::Type::SignedInteger:
        return std::to_string(mInteger);
    case XMLAttributeValueView::Type::UnsignedInteger:
        return std::to_string(static_cast<uint64_t>(mInteger));
    case XMLAttributeValueView::Type::Float:
        return std::to_string(mFloat);
    case XMLAttributeValueView::Type::Double:
        return std::to_string(mDouble);
    case XMLAttributeValueView::Type::StringView:
        return std::string(mStringView.Data, mStringView.Length);
    }

    return {};
}

wxString XMLAttributeValueView::ToWString() const
{
    switch (mType) {
    case XMLAttributeValueView::Type::Null:
        return {};
    case XMLAttributeValueView::Type::SignedInteger:
        return wxString() << mInteger;
    case XMLAttributeValueView::Type::UnsignedInteger:
        return wxString() << static_cast<uint64_t>(mInteger);
    case XMLAttributeValueView::Type::Float:
        return wxString() << mFloat;
    case XMLAttributeValueView::Type::Double:
        return wxString() << mDouble;
    case XMLAttributeValueView::Type::StringView:
        return wxString::FromUTF8(mStringView.Data, mStringView.Length);
    }

    return {};
}

template<typename ResultType>
bool CheckInteger(ResultType& output, int64_t value) noexcept
{
    constexpr int64_t minValue = std::numeric_limits<ResultType>::min();
    constexpr int64_t maxValue = std::numeric_limits<ResultType>::max();

    if (minValue <= value && value <= maxValue) {
        output = static_cast<ResultType>(value);
        return true;
    }

    return false;
}

template<typename ResultType>
bool CheckInteger(ResultType& output, uint64_t value) noexcept
{
    constexpr uint64_t maxValue = std::numeric_limits<ResultType>::max();
    const uint64_t unsignedValue = static_cast<uint64_t>(value);

    if (unsignedValue <= maxValue) {
        output = static_cast<ResultType>(unsignedValue);
        return true;
    }

    return false;
}

template<typename ResultType>
bool XMLAttributeValueView::TryGetInteger(ResultType& value) const noexcept
{
    static_assert(std::is_integral_v<ResultType>);

    if (mType == Type::SignedInteger) {
        return CheckInteger(value, mInteger);
    } else if (mType == Type::UnsignedInteger) {
        return CheckInteger(value, static_cast<uint64_t>(mInteger));
    } else if (mType == Type::StringView) {
        const char* end = mStringView.Data + mStringView.Length;

        ResultType tempValue = {};

        const auto result = FromChars(mStringView.Data, end, tempValue);

        if (result.ec == std::errc() && result.ptr == end) {
            value = tempValue;
            return true;
        }
    }

    return false;
}
