/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLAttributeValueView.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <cstdint>
#include <stdexcept>
#include <string>
#include <string_view>
#include <limits>

#include <wx/string.h>

/*! \brief A view into an attribute value. The class does not take the ownership of the data.
 *
 * Audacity internally uses regular XML and an internal binary XML-like format.
 * Binary format extends the possible XML types to support integers and floating point values,
 * not just the string values.
 *
 * This class represents the attribute value for both text and binary XML formats.
 *
 * Class has a number of Get overloads, that allow to read the value with the required data type.
 *
 * Integer-based types can be read from the view if it was initialized by an integer value or with a
 * string_view that is a textual representation of an integer value. To avoid runtime data loss it is not
 * possible to read an integer from the floating point values. Boundary checks are always performed.
 * If the type is not large enough to store the value, Get fails.
 *
 * Floating point values can be read from the view, if it is initialized using floating point, integer or
 * a compatible string value. Conversion from Float to Double is forbidden.
 *
 * String values can be read only if the view was created using a string_view.
 * However there are ToString() and ToWString() methods that allow conversion from any type to string.
 * Null type is represented by an empty string.
 */
class XML_API XMLAttributeValueView final
{
public:
    //! Type of the value represented by the XMLAttributeValueView.
    enum class Type
    {
        Null, //!< The value is not initialized.
        SignedInteger, //!< The value is a signed integer.
        UnsignedInteger, //!< The value is an unsigned integer.
        Float, //!< The value is a single precision floating point value.
        Double, //!< The value is a double precision floating point value.
        StringView //!< The value is a string_view.
    };

    //! Construct an uninitialized view of type Null
    XMLAttributeValueView() = default;

    XMLAttributeValueView(const XMLAttributeValueView&) = default;
    XMLAttributeValueView(XMLAttributeValueView&&) = default;

    XMLAttributeValueView& operator =(const XMLAttributeValueView&) = default;
    XMLAttributeValueView& operator =(XMLAttributeValueView&&) = default;

    //! Construct a view of type UnsignedInteger from the value.
    explicit XMLAttributeValueView(bool value) noexcept;
    //! Construct a view of type SignedInteger from the value.
    explicit XMLAttributeValueView(short value) noexcept;
    //! Construct a view of type UnsignedInteger from the value.
    explicit XMLAttributeValueView(unsigned short value) noexcept;
    //! Construct a view of type SignedInteger from the value.
    explicit XMLAttributeValueView(int value) noexcept;
    //! Construct a view of type UnsignedInteger from the value.
    explicit XMLAttributeValueView(unsigned int value) noexcept;
    //! Construct a view of type SignedInteger from the value.
    explicit XMLAttributeValueView(long value) noexcept;
    //! Construct a view of type UnsignedInteger from the value.
    explicit XMLAttributeValueView(unsigned long value) noexcept;
    //! Construct a view of type SignedInteger from the value.
    explicit XMLAttributeValueView(long long value) noexcept;
    //! Construct a view of type UnsignedInteger from the value.
    explicit XMLAttributeValueView(unsigned long long value) noexcept;
    //! Construct a view of type Float from the value.
    explicit XMLAttributeValueView(float value) noexcept;
    //! Construct a view of type Double from the value.
    explicit XMLAttributeValueView(double value) noexcept;
    //! Construct a view of type StringView from the value.
    explicit XMLAttributeValueView(const std::string_view& value) noexcept;

    //! Get the view type.
    Type GetType() const noexcept;

    //! Check if view is Null.
    bool IsNull() const noexcept;

    //! Check if view has the SignedInteger type.
    bool IsSignedInteger() const noexcept;
    //! Check if view has the UnsignedInteger type.
    bool IsUnsignedInteger() const noexcept;
    //! Check if view has the Float type.
    bool IsFloat() const noexcept;
    //! Check if view has the Double type.
    bool IsDouble() const noexcept;
    //! Check if view has the StringView type.
    bool IsStringView() const noexcept;

    //! Try to get a boolean value from the view.
    bool TryGet(bool& value) const noexcept;
    //! Try to get a short value from the view.
    bool TryGet(short& value) const noexcept;
    //! Try to get an unsigned short value from the view.
    bool TryGet(unsigned short& value) const noexcept;
    //! Try to get an int value from the view.
    bool TryGet(int& value) const noexcept;
    //! Try to get an unsigned int value from the view.
    bool TryGet(unsigned int& value) const noexcept;
    //! Try to get a long value from the view.
    bool TryGet(long& value) const noexcept;
    //! Try to get an unsigned long value from the view.
    bool TryGet(unsigned long& value) const noexcept;
    //! Try to get a long long value from the view.
    bool TryGet(long long& value) const noexcept;
    //! Try to get an unsigned long long value from the view.
    bool TryGet(unsigned long long& value) const noexcept;
    //! Try to get a float value from the view.
    bool TryGet(float& value) const noexcept;
    //! Try to get a double value from the view.
    bool TryGet(double& value) const noexcept;
    //! Try to get a string_view value from the view.
    bool TryGet(std::string_view& value) const noexcept;

    //! Returns the value if there is a viable conversion, default value otherwise
    template<typename T>
    T Get(T defaultValue = {}) const noexcept
    {
        // TryGet only modifies the value if conversion is possible,
        // so just reuse the defaultValue
        (void)TryGet(defaultValue);
        return defaultValue;
    }

    //! Convert the view value to an UTF8 string.
    std::string ToString() const;
    //! Convert the view value to wxString.
    wxString ToWString() const;

private:
    template<typename ResultType>
    bool TryGetInteger(ResultType& value) const noexcept;
    // std::variant requires macOS 10.14, which is significantly higher than
    // the current target
    union
    {
        int64_t mInteger;
        double mDouble;
        float mFloat;

        struct
        {
            const char* Data;
            size_t Length;
        } mStringView;
    };

    Type mType { Type::Null };
};
