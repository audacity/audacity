#define NOTEOFF 0x80
#define NOTEON 0x90
#define PRESSURE 0xa0
#define CONTROLLER 0xb0
#define PITCHBEND 0xe0
#define PROGRAM 0xc0
#define CHANPRESSURE 0xd0

/* These are the strings used in keynote to identify Standard MIDI File */
/* meta text messages. */

#define METATEXT                "Text Event"
#define METACOPYRIGHT           "Copyright Notice"
#define METASEQUENCE            "Sequence/Track Name"
#define METAINSTRUMENT          "Instrument Name"
#define METALYRIC               "Lyric"
#define METAMARKER              "Marker"
#define METACUE                 "Cue Point"
#define METAUNRECOGNIZED        "Unrecognized"
