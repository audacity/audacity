/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2Symbols.h

  @brief Declare URI identifiers used in calls to the lv2 library and a mapping
  of them to integers

  Paul Licameli split from LoadLV2.h

*********************************************************************/

#ifndef LV2SYMBOLS_H
#define LV2SYMBOLS_H

#include <vector>
#include "LV2Utils.h"
#include "MemoryX.h"

#if !defined(LV2_CORE__MIDIPlugin)
#define LV2_CORE__MIDIPlugin   LV2_CORE_PREFIX "MIDIPlugin"
#endif

#if !defined(LV2_CORE__Enabled)
#define LV2_CORE__enabled     LV2_CORE_PREFIX "enabled"
#endif

#if !defined(LV2_UI__makeResident)
#define LV2_UI__makeResident  LV2_UI_PREFIX "makeResident"
#endif

#if !defined(LV2_BUF_SIZE__nominalBlockLength)
#define LV2_BUF_SIZE__nominalBlockLength  LV2_BUF_SIZE_PREFIX "nominalBlockLength"
#endif

// The "ll-plugin" package includes math functions that appear as plugins
// and the best way to exclude them is by their class URI.
#define LL_NAMESPACE "http://ll-plugins.nongnu.org/lv2/namespace#"
#define LL_MATH_CONSTANTS LL_NAMESPACE "MathConstantPlugin"
#define LL_MATH_FUNCTIONS LL_NAMESPACE "MathFunctionPlugin"

// Define the list of URIs that need an LilvNode created
#undef NODE
#define NODE(n, u)

#undef NODELIST
#define NODELIST \
    NODE(AtomPort,               LV2_ATOM__AtomPort) \
    NODE(MaxBlockLength,         LV2_BUF_SIZE__maxBlockLength) \
    NODE(MinBlockLength,         LV2_BUF_SIZE__minBlockLength) \
    NODE(AudioPort,              LV2_CORE__AudioPort) \
    NODE(ControlPort,            LV2_CORE__ControlPort) \
    NODE(CVPort,                 LV2_CORE__CVPort) \
    NODE(Designation,            LV2_CORE__designation) \
    NODE(InputPort,              LV2_CORE__InputPort) \
    NODE(InstrumentPlugin,       LV2_CORE__InstrumentPlugin) \
    NODE(MIDIPlugin,             LV2_CORE__MIDIPlugin) \
    NODE(OutputPort,             LV2_CORE__OutputPort) \
    NODE(Control,                LV2_CORE__control) \
    NODE(Enumeration,            LV2_CORE__enumeration) \
    NODE(Integer,                LV2_CORE__integer) \
    NODE(Name,                   LV2_CORE__name) \
    NODE(OptionalFeature,        LV2_CORE__optionalFeature) \
    NODE(RequiredFeature,        LV2_CORE__requiredFeature) \
    NODE(SampleRate,             LV2_CORE__sampleRate) \
    NODE(Toggled,                LV2_CORE__toggled) \
    NODE(ExternalUI,             LV2_EXTERNAL_UI__Widget) \
    NODE(ExternalUIOld,          LV2_EXTERNAL_UI_DEPRECATED_URI) \
    NODE(MidiEvent,              LV2_MIDI__MidiEvent) \
    NODE(SupportedOption,        LV2_OPTIONS__supportedOption) \
    NODE(RequiredOption,         LV2_OPTIONS__requiredOption) \
    NODE(Group,                  LV2_PORT_GROUPS__group) \
    NODE(Logarithmic,            LV2_PORT_PROPS__logarithmic) \
    NODE(Trigger,                LV2_PORT_PROPS__trigger) \
    NODE(Preset,                 LV2_PRESETS__Preset) \
    NODE(MinimumSize,            LV2_RESIZE_PORT__minimumSize) \
    NODE(Position,               LV2_TIME__Position) \
    NODE(Gtk,                    LV2_UI__GtkUI) \
    NODE(Gtk3,                   LV2_UI__Gtk3UI) \
    NODE(Unit,                   LV2_UNITS__unit) \
    NODE(Comment,                LILV_NS_RDFS "comment") \
    NODE(Label,                  LILV_NS_RDFS "label") \
    NODE(MathConstants,          LL_MATH_CONSTANTS) \
    NODE(MathFunctions,          LL_MATH_FUNCTIONS)

// Define the list of URIs that need a URID registered
#undef URID
#define URID(n, u)

#undef URIDLIST
#define URIDLIST \
    URID(Blank,                  LV2_ATOM__Blank) \
    URID(Bool,                   LV2_ATOM__Bool) \
    URID(Chunk,                  LV2_ATOM__Chunk) \
    URID(Double,                 LV2_ATOM__Double) \
    URID(Float,                  LV2_ATOM__Float) \
    URID(Int,                    LV2_ATOM__Int) \
    URID(Literal,                LV2_ATOM__Literal) \
    URID(Long,                   LV2_ATOM__Long) \
    URID(Object,                 LV2_ATOM__Object) \
    URID(Path,                   LV2_ATOM__Path) \
    URID(Property,               LV2_ATOM__Property) \
    URID(Resource,               LV2_ATOM__Resource) \
    URID(Sequence,               LV2_ATOM__Sequence) \
    URID(String,                 LV2_ATOM__String) \
    URID(Tuple,                  LV2_ATOM__Tuple) \
    URID(Uri,                    LV2_ATOM__URI) \
    URID(Urid,                   LV2_ATOM__URID) \
    URID(Vector,                 LV2_ATOM__Vector) \
    URID(EventTransfer,          LV2_ATOM__eventTransfer) \
    URID(AtomSupports,           LV2_ATOM__supports) \
    URID(MaxBlockLength,         LV2_BUF_SIZE__maxBlockLength) \
    URID(MinBlockLength,         LV2_BUF_SIZE__minBlockLength) \
    URID(NominalBlockLength,     LV2_BUF_SIZE__nominalBlockLength) \
    URID(SequenceSize,           LV2_BUF_SIZE__sequenceSize) \
    URID(InstrumentPlugin,       LV2_CORE__InstrumentPlugin) \
    URID(MIDIPlugin,             LV2_CORE__MIDIPlugin) \
    URID(Toggled,                LV2_CORE__toggled) \
    URID(ExternalUI,             LV2_EXTERNAL_UI__Widget) \
    URID(ExternalUIOld,          LV2_EXTERNAL_UI_DEPRECATED_URI) \
    URID(Error,                  LV2_LOG__Error) \
    URID(Note,                   LV2_LOG__Note) \
    URID(Trace,                  LV2_LOG__Trace) \
    URID(Warning,                LV2_LOG__Warning) \
    URID(MidiEvent,              LV2_MIDI__MidiEvent) \
    URID(RequiredOption,         LV2_OPTIONS__requiredOption) \
    URID(SampleRate,             LV2_PARAMETERS__sampleRate) \
    URID(RangeSteps,             LV2_PORT_PROPS__rangeSteps) \
    URID(NotOnGUI,               LV2_PORT_PROPS__notOnGUI) \
    URID(Expensive,              LV2_PORT_PROPS__expensive) \
    URID(CausesArtifacts,        LV2_PORT_PROPS__causesArtifacts) \
    URID(NotAutomatic,           LV2_PORT_PROPS__notAutomatic) \
    URID(Position,               LV2_TIME__Position) \
    URID(Speed,                  LV2_TIME__speed) \
    URID(Frame,                  LV2_TIME__frame)

namespace LV2Symbols {
extern LV2_API LilvWorld* gWorld;

using URIDMap = std::vector<MallocString<> >;

//! Declare the global map of positive integers to URIs
extern LV2_API URIDMap gURIDMap;

//! Do reverse lookup in a map of URIs to positive integers,
//! returning 0 if not found and `!add`
LV2_API LV2_URID Lookup_URI(URIDMap& map, const char* uri, bool add = true);

// Declare the static LILV URI nodes
#undef NODE
#define NODE(n, u) \
    extern LV2_API LilvNode* node_##n; \
    extern LilvNodePtr unode_##n;
NODELIST

// Declare the static URIDs
#undef URID
#define URID(n, u) extern LV2_API LV2_URID urid_##n;
URIDLIST

//! Call before any use of the constants defined in this file
LV2_API bool InitializeGWorld();
//! Call at end of session
LV2_API void FinalizeGWorld();
}

#endif
