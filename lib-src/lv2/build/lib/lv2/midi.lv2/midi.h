/*
  Copyright 2012-2016 David Robillard <http://drobilla.net>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

/**
   @defgroup midi MIDI

   Definitions of standard MIDI messages, see <http://lv2plug.in/ns/ext/midi>
   for details.

   @{
*/

#ifndef LV2_MIDI_H
#define LV2_MIDI_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define LV2_MIDI_URI    "http://lv2plug.in/ns/ext/midi"  ///< http://lv2plug.in/ns/ext/midi
#define LV2_MIDI_PREFIX LV2_MIDI_URI "#"                 ///< http://lv2plug.in/ns/ext/midi#

#define LV2_MIDI__ActiveSense      LV2_MIDI_PREFIX "ActiveSense"       ///< http://lv2plug.in/ns/ext/midi#ActiveSense
#define LV2_MIDI__Aftertouch       LV2_MIDI_PREFIX "Aftertouch"        ///< http://lv2plug.in/ns/ext/midi#Aftertouch
#define LV2_MIDI__Bender           LV2_MIDI_PREFIX "Bender"            ///< http://lv2plug.in/ns/ext/midi#Bender
#define LV2_MIDI__ChannelPressure  LV2_MIDI_PREFIX "ChannelPressure"   ///< http://lv2plug.in/ns/ext/midi#ChannelPressure
#define LV2_MIDI__Chunk            LV2_MIDI_PREFIX "Chunk"             ///< http://lv2plug.in/ns/ext/midi#Chunk
#define LV2_MIDI__Clock            LV2_MIDI_PREFIX "Clock"             ///< http://lv2plug.in/ns/ext/midi#Clock
#define LV2_MIDI__Continue         LV2_MIDI_PREFIX "Continue"          ///< http://lv2plug.in/ns/ext/midi#Continue
#define LV2_MIDI__Controller       LV2_MIDI_PREFIX "Controller"        ///< http://lv2plug.in/ns/ext/midi#Controller
#define LV2_MIDI__MidiEvent        LV2_MIDI_PREFIX "MidiEvent"         ///< http://lv2plug.in/ns/ext/midi#MidiEvent
#define LV2_MIDI__NoteOff          LV2_MIDI_PREFIX "NoteOff"           ///< http://lv2plug.in/ns/ext/midi#NoteOff
#define LV2_MIDI__NoteOn           LV2_MIDI_PREFIX "NoteOn"            ///< http://lv2plug.in/ns/ext/midi#NoteOn
#define LV2_MIDI__ProgramChange    LV2_MIDI_PREFIX "ProgramChange"     ///< http://lv2plug.in/ns/ext/midi#ProgramChange
#define LV2_MIDI__QuarterFrame     LV2_MIDI_PREFIX "QuarterFrame"      ///< http://lv2plug.in/ns/ext/midi#QuarterFrame
#define LV2_MIDI__Reset            LV2_MIDI_PREFIX "Reset"             ///< http://lv2plug.in/ns/ext/midi#Reset
#define LV2_MIDI__SongPosition     LV2_MIDI_PREFIX "SongPosition"      ///< http://lv2plug.in/ns/ext/midi#SongPosition
#define LV2_MIDI__SongSelect       LV2_MIDI_PREFIX "SongSelect"        ///< http://lv2plug.in/ns/ext/midi#SongSelect
#define LV2_MIDI__Start            LV2_MIDI_PREFIX "Start"             ///< http://lv2plug.in/ns/ext/midi#Start
#define LV2_MIDI__Stop             LV2_MIDI_PREFIX "Stop"              ///< http://lv2plug.in/ns/ext/midi#Stop
#define LV2_MIDI__SystemCommon     LV2_MIDI_PREFIX "SystemCommon"      ///< http://lv2plug.in/ns/ext/midi#SystemCommon
#define LV2_MIDI__SystemExclusive  LV2_MIDI_PREFIX "SystemExclusive"   ///< http://lv2plug.in/ns/ext/midi#SystemExclusive
#define LV2_MIDI__SystemMessage    LV2_MIDI_PREFIX "SystemMessage"     ///< http://lv2plug.in/ns/ext/midi#SystemMessage
#define LV2_MIDI__SystemRealtime   LV2_MIDI_PREFIX "SystemRealtime"    ///< http://lv2plug.in/ns/ext/midi#SystemRealtime
#define LV2_MIDI__Tick             LV2_MIDI_PREFIX "Tick"              ///< http://lv2plug.in/ns/ext/midi#Tick
#define LV2_MIDI__TuneRequest      LV2_MIDI_PREFIX "TuneRequest"       ///< http://lv2plug.in/ns/ext/midi#TuneRequest
#define LV2_MIDI__VoiceMessage     LV2_MIDI_PREFIX "VoiceMessage"      ///< http://lv2plug.in/ns/ext/midi#VoiceMessage
#define LV2_MIDI__benderValue      LV2_MIDI_PREFIX "benderValue"       ///< http://lv2plug.in/ns/ext/midi#benderValue
#define LV2_MIDI__binding          LV2_MIDI_PREFIX "binding"           ///< http://lv2plug.in/ns/ext/midi#binding
#define LV2_MIDI__byteNumber       LV2_MIDI_PREFIX "byteNumber"        ///< http://lv2plug.in/ns/ext/midi#byteNumber
#define LV2_MIDI__channel          LV2_MIDI_PREFIX "channel"           ///< http://lv2plug.in/ns/ext/midi#channel
#define LV2_MIDI__chunk            LV2_MIDI_PREFIX "chunk"             ///< http://lv2plug.in/ns/ext/midi#chunk
#define LV2_MIDI__controllerNumber LV2_MIDI_PREFIX "controllerNumber"  ///< http://lv2plug.in/ns/ext/midi#controllerNumber
#define LV2_MIDI__controllerValue  LV2_MIDI_PREFIX "controllerValue"   ///< http://lv2plug.in/ns/ext/midi#controllerValue
#define LV2_MIDI__noteNumber       LV2_MIDI_PREFIX "noteNumber"        ///< http://lv2plug.in/ns/ext/midi#noteNumber
#define LV2_MIDI__pressure         LV2_MIDI_PREFIX "pressure"          ///< http://lv2plug.in/ns/ext/midi#pressure
#define LV2_MIDI__programNumber    LV2_MIDI_PREFIX "programNumber"     ///< http://lv2plug.in/ns/ext/midi#programNumber
#define LV2_MIDI__property         LV2_MIDI_PREFIX "property"          ///< http://lv2plug.in/ns/ext/midi#property
#define LV2_MIDI__songNumber       LV2_MIDI_PREFIX "songNumber"        ///< http://lv2plug.in/ns/ext/midi#songNumber
#define LV2_MIDI__songPosition     LV2_MIDI_PREFIX "songPosition"      ///< http://lv2plug.in/ns/ext/midi#songPosition
#define LV2_MIDI__status           LV2_MIDI_PREFIX "status"            ///< http://lv2plug.in/ns/ext/midi#status
#define LV2_MIDI__statusMask       LV2_MIDI_PREFIX "statusMask"        ///< http://lv2plug.in/ns/ext/midi#statusMask
#define LV2_MIDI__velocity         LV2_MIDI_PREFIX "velocity"          ///< http://lv2plug.in/ns/ext/midi#velocity

/**
   MIDI Message Type.

   This includes both voice messages (which have a channel) and system messages
   (which do not), as well as a sentinel value for invalid messages.  To get
   the type of a message suitable for use in a switch statement, use
   lv2_midi_get_type() on the status byte.
*/
typedef enum {
	LV2_MIDI_MSG_INVALID          = 0,     /**< Invalid Message */
	LV2_MIDI_MSG_NOTE_OFF         = 0x80,  /**< Note Off */
	LV2_MIDI_MSG_NOTE_ON          = 0x90,  /**< Note On */
	LV2_MIDI_MSG_NOTE_PRESSURE    = 0xA0,  /**< Note Pressure */
	LV2_MIDI_MSG_CONTROLLER       = 0xB0,  /**< Controller */
	LV2_MIDI_MSG_PGM_CHANGE       = 0xC0,  /**< Program Change */
	LV2_MIDI_MSG_CHANNEL_PRESSURE = 0xD0,  /**< Channel Pressure */
	LV2_MIDI_MSG_BENDER           = 0xE0,  /**< Pitch Bender */
	LV2_MIDI_MSG_SYSTEM_EXCLUSIVE = 0xF0,  /**< System Exclusive Begin */
	LV2_MIDI_MSG_MTC_QUARTER      = 0xF1,  /**< MTC Quarter Frame */
	LV2_MIDI_MSG_SONG_POS         = 0xF2,  /**< Song Position */
	LV2_MIDI_MSG_SONG_SELECT      = 0xF3,  /**< Song Select */
	LV2_MIDI_MSG_TUNE_REQUEST     = 0xF6,  /**< Tune Request */
	LV2_MIDI_MSG_CLOCK            = 0xF8,  /**< Clock */
	LV2_MIDI_MSG_START            = 0xFA,  /**< Start */
	LV2_MIDI_MSG_CONTINUE         = 0xFB,  /**< Continue */
	LV2_MIDI_MSG_STOP             = 0xFC,  /**< Stop */
	LV2_MIDI_MSG_ACTIVE_SENSE     = 0xFE,  /**< Active Sensing */
	LV2_MIDI_MSG_RESET            = 0xFF   /**< Reset */
} LV2_Midi_Message_Type;

/**
   Standard MIDI Controller Numbers.
*/
typedef enum {
	LV2_MIDI_CTL_MSB_BANK             = 0x00,  /**< Bank Selection */
	LV2_MIDI_CTL_MSB_MODWHEEL         = 0x01,  /**< Modulation */
	LV2_MIDI_CTL_MSB_BREATH           = 0x02,  /**< Breath */
	LV2_MIDI_CTL_MSB_FOOT             = 0x04,  /**< Foot */
	LV2_MIDI_CTL_MSB_PORTAMENTO_TIME  = 0x05,  /**< Portamento Time */
	LV2_MIDI_CTL_MSB_DATA_ENTRY       = 0x06,  /**< Data Entry */
	LV2_MIDI_CTL_MSB_MAIN_VOLUME      = 0x07,  /**< Main Volume */
	LV2_MIDI_CTL_MSB_BALANCE          = 0x08,  /**< Balance */
	LV2_MIDI_CTL_MSB_PAN              = 0x0A,  /**< Panpot */
	LV2_MIDI_CTL_MSB_EXPRESSION       = 0x0B,  /**< Expression */
	LV2_MIDI_CTL_MSB_EFFECT1          = 0x0C,  /**< Effect1 */
	LV2_MIDI_CTL_MSB_EFFECT2          = 0x0D,  /**< Effect2 */
	LV2_MIDI_CTL_MSB_GENERAL_PURPOSE1 = 0x10,  /**< General Purpose 1 */
	LV2_MIDI_CTL_MSB_GENERAL_PURPOSE2 = 0x11,  /**< General Purpose 2 */
	LV2_MIDI_CTL_MSB_GENERAL_PURPOSE3 = 0x12,  /**< General Purpose 3 */
	LV2_MIDI_CTL_MSB_GENERAL_PURPOSE4 = 0x13,  /**< General Purpose 4 */
	LV2_MIDI_CTL_LSB_BANK             = 0x20,  /**< Bank Selection */
	LV2_MIDI_CTL_LSB_MODWHEEL         = 0x21,  /**< Modulation */
	LV2_MIDI_CTL_LSB_BREATH           = 0x22,  /**< Breath */
	LV2_MIDI_CTL_LSB_FOOT             = 0x24,  /**< Foot */
	LV2_MIDI_CTL_LSB_PORTAMENTO_TIME  = 0x25,  /**< Portamento Time */
	LV2_MIDI_CTL_LSB_DATA_ENTRY       = 0x26,  /**< Data Entry */
	LV2_MIDI_CTL_LSB_MAIN_VOLUME      = 0x27,  /**< Main Volume */
	LV2_MIDI_CTL_LSB_BALANCE          = 0x28,  /**< Balance */
	LV2_MIDI_CTL_LSB_PAN              = 0x2A,  /**< Panpot */
	LV2_MIDI_CTL_LSB_EXPRESSION       = 0x2B,  /**< Expression */
	LV2_MIDI_CTL_LSB_EFFECT1          = 0x2C,  /**< Effect1 */
	LV2_MIDI_CTL_LSB_EFFECT2          = 0x2D,  /**< Effect2 */
	LV2_MIDI_CTL_LSB_GENERAL_PURPOSE1 = 0x30,  /**< General Purpose 1 */
	LV2_MIDI_CTL_LSB_GENERAL_PURPOSE2 = 0x31,  /**< General Purpose 2 */
	LV2_MIDI_CTL_LSB_GENERAL_PURPOSE3 = 0x32,  /**< General Purpose 3 */
	LV2_MIDI_CTL_LSB_GENERAL_PURPOSE4 = 0x33,  /**< General Purpose 4 */
	LV2_MIDI_CTL_SUSTAIN              = 0x40,  /**< Sustain Pedal */
	LV2_MIDI_CTL_PORTAMENTO           = 0x41,  /**< Portamento */
	LV2_MIDI_CTL_SOSTENUTO            = 0x42,  /**< Sostenuto */
	LV2_MIDI_CTL_SOFT_PEDAL           = 0x43,  /**< Soft Pedal */
	LV2_MIDI_CTL_LEGATO_FOOTSWITCH    = 0x44,  /**< Legato Foot Switch */
	LV2_MIDI_CTL_HOLD2                = 0x45,  /**< Hold2 */
	LV2_MIDI_CTL_SC1_SOUND_VARIATION  = 0x46,  /**< SC1 Sound Variation */
	LV2_MIDI_CTL_SC2_TIMBRE           = 0x47,  /**< SC2 Timbre */
	LV2_MIDI_CTL_SC3_RELEASE_TIME     = 0x48,  /**< SC3 Release Time */
	LV2_MIDI_CTL_SC4_ATTACK_TIME      = 0x49,  /**< SC4 Attack Time */
	LV2_MIDI_CTL_SC5_BRIGHTNESS       = 0x4A,  /**< SC5 Brightness */
	LV2_MIDI_CTL_SC6                  = 0x4B,  /**< SC6 */
	LV2_MIDI_CTL_SC7                  = 0x4C,  /**< SC7 */
	LV2_MIDI_CTL_SC8                  = 0x4D,  /**< SC8 */
	LV2_MIDI_CTL_SC9                  = 0x4E,  /**< SC9 */
	LV2_MIDI_CTL_SC10                 = 0x4F,  /**< SC10 */
	LV2_MIDI_CTL_GENERAL_PURPOSE5     = 0x50,  /**< General Purpose 5 */
	LV2_MIDI_CTL_GENERAL_PURPOSE6     = 0x51,  /**< General Purpose 6 */
	LV2_MIDI_CTL_GENERAL_PURPOSE7     = 0x52,  /**< General Purpose 7 */
	LV2_MIDI_CTL_GENERAL_PURPOSE8     = 0x53,  /**< General Purpose 8 */
	LV2_MIDI_CTL_PORTAMENTO_CONTROL   = 0x54,  /**< Portamento Control */
	LV2_MIDI_CTL_E1_REVERB_DEPTH      = 0x5B,  /**< E1 Reverb Depth */
	LV2_MIDI_CTL_E2_TREMOLO_DEPTH     = 0x5C,  /**< E2 Tremolo Depth */
	LV2_MIDI_CTL_E3_CHORUS_DEPTH      = 0x5D,  /**< E3 Chorus Depth */
	LV2_MIDI_CTL_E4_DETUNE_DEPTH      = 0x5E,  /**< E4 Detune Depth */
	LV2_MIDI_CTL_E5_PHASER_DEPTH      = 0x5F,  /**< E5 Phaser Depth */
	LV2_MIDI_CTL_DATA_INCREMENT       = 0x60,  /**< Data Increment */
	LV2_MIDI_CTL_DATA_DECREMENT       = 0x61,  /**< Data Decrement */
	LV2_MIDI_CTL_NRPN_LSB             = 0x62,  /**< Non-registered Parameter Number */
	LV2_MIDI_CTL_NRPN_MSB             = 0x63,  /**< Non-registered Parameter Number */
	LV2_MIDI_CTL_RPN_LSB              = 0x64,  /**< Registered Parameter Number */
	LV2_MIDI_CTL_RPN_MSB              = 0x65,  /**< Registered Parameter Number */
	LV2_MIDI_CTL_ALL_SOUNDS_OFF       = 0x78,  /**< All Sounds Off */
	LV2_MIDI_CTL_RESET_CONTROLLERS    = 0x79,  /**< Reset Controllers */
	LV2_MIDI_CTL_LOCAL_CONTROL_SWITCH = 0x7A,  /**< Local Control Switch */
	LV2_MIDI_CTL_ALL_NOTES_OFF        = 0x7B,  /**< All Notes Off */
	LV2_MIDI_CTL_OMNI_OFF             = 0x7C,  /**< Omni Off */
	LV2_MIDI_CTL_OMNI_ON              = 0x7D,  /**< Omni On */
	LV2_MIDI_CTL_MONO1                = 0x7E,  /**< Mono1 */
	LV2_MIDI_CTL_MONO2                = 0x7F   /**< Mono2 */
} LV2_Midi_Controller;

/**
   Return true iff `msg` is a MIDI voice message (which has a channel).
*/
static inline bool
lv2_midi_is_voice_message(const uint8_t* msg) {
	return msg[0] >= 0x80 && msg[0] < 0xF0;
}

/**
   Return true iff `msg` is a MIDI system message (which has no channel).
*/
static inline bool
lv2_midi_is_system_message(const uint8_t* msg) {
	switch (msg[0]) {
	case 0xF4: case 0xF5: case 0xF7: case 0xF9: case 0xFD:
		return false;
	default:
		return (msg[0] & 0xF0) == 0xF0;
	}
}

/**
   Return the type of a MIDI message.
   @param msg Pointer to the start (status byte) of a MIDI message.
*/
static inline LV2_Midi_Message_Type
lv2_midi_message_type(const uint8_t* msg) {
	if (lv2_midi_is_voice_message(msg)) {
		return (LV2_Midi_Message_Type)(msg[0] & 0xF0);
	} else if (lv2_midi_is_system_message(msg)) {
		return (LV2_Midi_Message_Type)msg[0];
	} else {
		return LV2_MIDI_MSG_INVALID;
	}
}

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* LV2_MIDI_H */

/**
   @}
*/
