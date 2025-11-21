/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportTypes.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include <variant>
#include <string>
#include <vector>
#include <future>
#include "Internat.h"

class ExportProcessorDelegate;

using ExportOptionID = int;

enum class ExportResult
{
    Success,
    Error,
    Cancelled,
    Stopped
};

using ExportTask = std::packaged_task<ExportResult (ExportProcessorDelegate&)>;

///\brief A type of option values (parameters) used by exporting plugins
using ExportValue = std::variant<
    bool,
    int,
    double,
    std::string>;

///\brief A type that provides a description of an exporting option.
///Isn't allowed to change except non-type related flags.
struct IMPORT_EXPORT_API ExportOption
{
    enum Flags : int
    {
        TypeMask         = 0xff,///< Mask for bits that hold option type
        TypeRange        = 1,  ///< Range option. `values` holds [min, max]
        TypeEnum         = 2,  ///< List/enum option. `values` holds items, and `names` text to be displayed.

        ReadOnly         = 0x100,///< Parameter is read-only, client should not attempt to change it's value
        Hidden           = 0x200,///< Option is not used and may be hidden from the user

        Default          = 0    ///< No special flags
    };

    ExportOptionID id; ///< Internal option id
    TranslatableString title; ///< Name of an option in a human-readable form
    ExportValue defaultValue; ///< Default valid value for the parameter
    int flags { Default }; ///< A set of flag that desc
    std::vector<ExportValue> values {};///< Interpretation depends on type
    TranslatableStrings names {}; ///< Interpretation depends on type
};
