// Portsmf (also known as Allegro):
// music representation system, with
//      extensible in-memory sequence structure
//      upward compatible with MIDI
//      implementations in C++ and Serpent
//      external, text-based representation
//      compatible with Aura
//
// SERIALBUFFER CLASS
// 
// The Serial_buffer class is defined to support serialization and
// unserialization. A Serial_buffer is just a block of memory with
// a length and a read/write pointer. When writing, it can expand. 
//
// SERIALIZATION
//
// The Alg_track class has static members:
//    ser_buf -- a Serial_buffer
// When objects are serialized, they are first written to 
// ser_buf, which is expanded whenever necessary. Then, when
// the length is known, new memory is allocated and the data
// is copied to a correctly-sized buffer and returned to caller.
// The "external" (callable from outside the library) 
// serialization functions are:
//    Alg_track::serialize()
//    Alg_seq::serialize()
// The "internal" serialization functions to be called from within
// the library are:
//    Alg_track::serialize_track(bool text)
//    Alg_seq::serialize_seq(bool text)
//    Alg_track::serialize_parameter(
//            Alg_parameter *parm, bool text)
// These internal serialize functions append data to ser_buf The text 
// flag says to write an ascii representation as opposed to binary.
//  
// UNSERIALIZATION:
//
// The Alg_track class has a static member: 
//     unserialize(char *buffer, long len)
// that will unserialize anything -- an Alg_track or an Alg_seq.
// No other function should be called from outside the library.
// Internal unserialize functions are:
//     Alg_seq::unserialize_seq()
//     Alg_track::unserialize_track()
//     Alg_track::unserialize_parameter(Alg_parameter_ptr parm_ptr)
// Just as serialization uses ser_buf for output, unserialization uses
// unser_buf for reading. unser_buf is another static member of Alg_track.

#ifndef ALLEGRO_H
#define ALLEGRO_H
#include <assert.h>
#include <cstdint>
#include <istream>
#include <ostream>

#define ALG_EPS 0.000001 // epsilon
#define ALG_DEFAULT_BPM 100.0 // default tempo

// are d1 and d2 within epsilon of each other?
bool within(double d1, double d2, double epsilon);

char *heapify(const char *s); // put a string on the heap


// Alg_attribute is an atom in the symbol table
// with the special addition that the last
// character is prefixed to the string; thus,
// the attribute 'tempor' (a real) is stored
// as 'rtempor'. To get the string name, just
// use attribute+1.
typedef const char *Alg_attribute;
#define alg_attr_name(a) ((a) + 1)
#define alg_attr_type(a) (*(a))

// Alg_atoms is a symbol table of Alg_attributes and other
// unique strings
class Alg_atoms {
public:
    Alg_atoms() {
        maxlen = len = 0;
        atoms = NULL;
    }
    // Note: the code is possibly more correct and faster without the 
    // following destructor, which will only run after the program takes 
    // a normal exit. Cleaning up after the program exit slows down the exit,
    // and will cause problems if any other destructor tries to reference an
    // Alg_atom (which will now be freed). The advantage of this code is
    // that Alg_atoms will not be reported as memory leaks by automation
    // that doesn't know better. -RBD
    virtual ~Alg_atoms() {
        for (int i = 0; i < len; i++) {
            delete atoms[i];
        }
        if (atoms) delete [] atoms;
    }
    // insert/lookup an attribute
    Alg_attribute insert_attribute(Alg_attribute attr);
    // insert/lookup attribute by name (without prefixed type)
    Alg_attribute insert_string(const char *name);
private:
    long maxlen;
    long len;
    Alg_attribute *atoms;

    // insert an Attriubute not in table after moving attr to heap
    Alg_attribute insert_new(const char *name, char attr_type);
    void expand(); // make more space
};

extern Alg_atoms symbol_table;


// an attribute/value pair. Since Alg_attribute names imply type,
// we try to keep attributes and values packaged together as
// Alg_parameter class
typedef class Alg_parameter {
public:
    // This constructor guarantees that an Alg_parameter can be
    // deleted safely without further initialization. It does not
    // do anything useful, so it is expected that the creator will
    // set attr and store a value in the appropriate union field.
    Alg_attribute attr;
    union {
        double r;// real
        const char *s; // string
        int64_t i;  // integer -- 64 bit for 32/64-bit compatibility
        bool l;  // logical
        const char *a; // symbol (atom)
    }; // anonymous union

    Alg_parameter() { attr = "i"; }
    ~Alg_parameter();
    void copy(Alg_parameter *); // copy from another parameter
    const char attr_type() { return alg_attr_type(attr); }
    const char *attr_name() { return alg_attr_name(attr); }
    void set_attr(Alg_attribute a) { attr = a; }
    void show();
} *Alg_parameter_ptr;


// a list of attribute/value pairs
typedef class Alg_parameters {
public:
    class Alg_parameters *next;
    Alg_parameter parm;

    Alg_parameters(Alg_parameters *list) {
        next = list;
    }

    //~Alg_parameters() { }

    // each of these routines takes address of pointer to the list
    // insertion is performed without checking whether or not a
    // parameter already exists with this attribute. See find() and
    // remove_key() to assist in checking for and removing existing 
    // parameters.
    // Note also that these insert_* methods convert name to an
    // attribute. If you have already done the symbol table lookup/insert
    // you can do these operations faster (in which case we should add
    // another set of functions that take attributes as arguments.)
    static void insert_real(Alg_parameters **list, const char *name, double r);
    // insert string will copy string to heap
    static void insert_string(Alg_parameters **list, const char *name, 
                              const char *s);
    static void insert_integer(Alg_parameters **list, const char *name,
                               int64_t i);
    static void insert_logical(Alg_parameters **list, const char *name, bool l);
    static void insert_atom(Alg_parameters **list, const char *name, 
                            const char *s);
    static Alg_parameters *remove_key(Alg_parameters **list, const char *name);
    // find an attribute/value pair
    Alg_parameter_ptr find(Alg_attribute attr);
} *Alg_parameters_ptr;


// these are type codes associated with certain attributes
// see Alg_track::find() where these are bit positions in event_type_mask
#define ALG_NOTE 0 // This is a note, not an update
#define ALG_GATE 1 // "gate"
#define ALG_BEND 2 // "bend"
#define ALG_CONTROL 3 // "control"
#define ALG_PROGRAM 4 // "program"
#define ALG_PRESSURE 5 // "pressure"
#define ALG_KEYSIG 6 // "keysig"
#define ALG_TIMESIG_NUM 7 // "timesig_num"
#define ALG_TIMESIG_DEN 8 // "timesig_den"
#define ALG_OTHER 9 // any other value

// abstract superclass of Alg_note and Alg_update:
typedef class Alg_event {
protected:
    bool selected;
    char type; // 'e' event, 'n' note, 'u' update
    int32_t key; // note identifier -- fixed at 32 bits
    static const char* description; // static buffer for debugging
public:
    double time;
    int32_t chan; // generalization of MIDI channels -- fixed at 32 bits
    virtual void show() = 0;
    // Note: there is no Alg_event() because Alg_event is an abstract class.
    bool is_note() { return (type == 'n'); }   // tell whether an Alg_event is a note
    bool is_update() { return (type == 'u'); } // tell whether an Alg_event is a parameter update
    char get_type() { return type; }   // return 'n' for note, 'u' for update
    int get_type_code();  // 1 = volume change,      2 = pitch bend,
                          // 3 = control change,     4 = program change,
                          // 5 = pressure change,    6 = key signature,
                          // 7 = time sig numerator, 8 = time sig denominator
    bool get_selected() { return selected; }         
    void set_selected(bool b) { selected = b; }     
    // Note: notes are identified by a (channel, identifier) pair. 
    // For midi, the identifier is the key number (pitch). The identifier
    // does not have to represent pitch; it's main purpose is to identify
    // notes so that they can be named by subsequent update events.

    // get MIDI key or note identifier of note or update:
    int32_t get_identifier() { return key; }
    void set_identifier(int32_t i) { key = i; } // set the identifier
    // In all of these set_ methods, strings are owned by the caller and
    // copied as necessary by the callee. For notes, an attribute/value
    // pair is added to the parameters list. For updates, the single
    // attribute/value parameter pair is overwritten. In all cases, the
    // attribute (first argument) must agree in type with the second arg.
    // The last letter of the attribute implies the type (see below).
    void set_parameter(Alg_parameter_ptr new_parameter);
    void set_string_value(const char *attr, const char *value);
    void set_real_value(const char *attr, double value);
    void set_logical_value(const char *attr, bool value);
    void set_integer_value(const char *attr, int64_t value);
    void set_atom_value(const char *attr, const char *atom);

    // Some note methods. These fail (via assert()) if this is not a note:
    //
    float get_pitch();// get pitch in steps -- use this even for MIDI
    float get_loud(); // get loudness (MIDI velocity)
    // times are in seconds or beats, depending upon the units_are_seconds 
    // flag in the containing sequence
    double get_start_time(); // get start time in seconds or beats
    double get_end_time();   // get end time in seconds or beats
    double get_duration();   // get duration in seconds or beats
    void set_pitch(float);
    void set_loud(float);
    void set_duration(double);

    // Notes have lists of attribute values. Attributes are converted
    // to/from strings in this API to avoid explicit use of Alg_attribute
    // types. Attribute names end with a type designation: 's', 'r', 'l',
    // 'i', or 'a'.
    //
    bool has_attribute(const char *attr);      // test if note has attribute/value pair
    char get_attribute_type(const char *attr); // get the associated type: 
        // 's' = string, 
        // 'r' = real (double), 'l' = logical (bool), 'i' = integer (int64_t),
        // 'a' = atom (char *), a unique string stored in Alg_seq
    // get the string value
    const char *get_string_value(const char *attr, const char *value = NULL);
    // get the real value
    double get_real_value(const char *attr, double value = 0.0);
    // get the logical value
    bool get_logical_value(const char *attr, bool value = false);
    // get the integer value
    int64_t get_integer_value(const char *attr, int64_t value = 0);
    // get the atom value
    const char *get_atom_value(const char *attr, const char *value = NULL);
    void delete_attribute(const char *attr);   // delete an attribute/value pair
        // (ignore if no matching attribute/value pair exists)

    // Some attribute/value methods. These fail if this is not an update.
    // Attributes are converted to/from strings to avoid explicit use
    // of Alg_attribute types.
    // 
    const char *get_attribute();    // get the update's attribute (string)
    char get_update_type();   // get the update's type: 's' = string, 
        // 'r' = real (double), 'l' = logical (bool), 'i' = integer (int64_t),
        // 'a' = atom (char *), a unique string stored in Alg_seq
    const char *get_string_value(); // get the update's string value
        // Notes: Caller does not own the return value. Do not modify.
        // Do not use after underlying Alg_seq is modified.
    double get_real_value();  // get the update's real value
    bool get_logical_value(); // get the update's logical value
    int64_t get_integer_value(); // get the update's integer value
    const char *get_atom_value();   // get the update's atom value
        // Notes: Caller does not own the return value. Do not modify.
        // The return value's lifetime is forever.

    // Auxiliary function to aid in editing tracks
    // Returns true if the event overlaps the given region
    bool overlap(double t, double len, bool all);

    const char *GetDescription(); // computes a text description of this event
    // the result is in a static buffer, not thread-safe, just for debugging.
    Alg_event() { selected = false; }
    virtual ~Alg_event() {}
} *Alg_event_ptr;


typedef class Alg_note : public Alg_event {
public:
    virtual ~Alg_note();
    Alg_note(Alg_note *); // copy constructor
    float pitch; // pitch in semitones (69 = A440)
    float loud;  // dynamic corresponding to MIDI velocity
    double dur;   // duration in seconds (normally to release point)
    Alg_parameters_ptr parameters; // attribute/value pair list
    Alg_note() { type = 'n'; parameters = NULL; }
    void show();
} *Alg_note_ptr;


typedef class Alg_update : public Alg_event {
public:
    virtual ~Alg_update() {};
    Alg_update(Alg_update *); // copy constructor
    Alg_parameter parameter; // an update contains one attr/value pair


    Alg_update() { type = 'u'; }
    void show();
} *Alg_update_ptr;


// a sequence of Alg_event objects
typedef class Alg_events {
private:
    long maxlen;
    void expand();
protected:
    long len;
    Alg_event_ptr *events; // events is array of pointers
public:
    // sometimes, it is nice to have the time of the last note-off.
    // In the current implementation,
    // this field is set by append to indicate the time of the 
    // last note-off in the current unit, so it should be correct after 
    // creating a new track and adding notes to it. It is *not*
    // updated after uninsert(), so use it with care.
    double last_note_off;
    // initially false, in_use can be used to mark "do not delete". If an
    // Alg_events instance is deleted while "in_use", an assertion will fail.
    bool in_use;
    virtual int length() { return len; }
    Alg_event_ptr &operator[](int i) {
        assert(i >= 0 && i < len);
        return events[i];
    }
    Alg_events() {
        maxlen = len = 0;
        events = NULL;
        last_note_off = 0;
        in_use = false;
    }
    // destructor deletes the events array, but not the
    // events themselves
    virtual ~Alg_events();
    void set_events(Alg_event_ptr *e, long l, long m) {
        if (events) delete [] events;
        events = e; len = l; maxlen = m; }
    // for use by Alg_track and Alg_seq
    void insert(Alg_event_ptr event);
    void append(Alg_event_ptr event);
    Alg_event_ptr uninsert(long index);
} *Alg_events_ptr;

class Alg_track;

typedef class Alg_event_list : public Alg_events {
protected:
    char type; // 'e' Alg_event_list, 't' Alg_track, 's' Alg_seq
    static const char *last_error_message;
    Alg_track *events_owner; // if this is an Alg_event_list,
        // the events are owned by an Alg_track or an Alg_seq
    static int sequences;  // to keep track of sequence numbers
    int sequence_number;   // this sequence number is incremented
        // whenever an edit is performed on an Alg_track or Alg_seq.
        // When an Alg_event_list is created to contain pointers to
        // a subset of an Alg_track or Alg_seq (the events_owner), 
        // the Alg_event_list gets a copy of the events_owner's 
        // sequence_number. If the events_owner is edited, the pointers
        // in this Alg_event_list will become invalid. This is detected
        // (for debugging) as differing sequence_numbers.

    // every event list, track, and seq has a duration.
    // Usually the duration is set when the list is constructed, e.g.
    // when you extract from 10 to 15 seconds, the duration is 5 secs.
    // The duration does not tell you when is the last note-off.
    // duration is recorded in both beats and seconds:
    double beat_dur; 
    double real_dur;
public:
    // the client should not create one of these, but these are
    // returned from various track and seq operations. An
    // Alg_event_list "knows" the Alg_track or Alg_seq that "owns"
    // the events. All events in an Alg_event_list must belong
    // to the same Alg_track or Alg_seq structure.
    // When applied to an Alg_seq, events are enumerated track
    // by track with increasing indices. This operation is not
    // particularly fast on an Alg_seq.
    virtual Alg_event_ptr &operator[](int i);
    Alg_event_list() { sequence_number = 0; 
        beat_dur = 0.0; real_dur = 0.0; events_owner = NULL; type = 'e'; }
    Alg_event_list(Alg_track *owner);

    char get_type() { return type; }
    Alg_track *get_owner() { return events_owner; }

    // The destructor does not free events because they are owned
    // by a track or seq structure.
    virtual ~Alg_event_list();

    // Returns the duration of the sequence in beats or seconds
    double get_beat_dur() { return beat_dur; }
    void set_beat_dur(double d) { beat_dur = d; }
    double get_real_dur() { return real_dur; }
    void set_real_dur(double d) { real_dur = d; }

    // Events are stored in time order, so when you change the time of
    // an event, you must adjust the position. When you call set_start_time
    // on an Alg_event_list, the Alg_event_list is not modified, but the
    // Alg_track that "owns" the event is modified. If the owner is an 
    // Alg_seq, this may require searching the seq for the track containing
    // the event. This will mean a logN search of every track in the seq
    // (but if this turns out to be a problem, we can store each event's
    // track owner in the Alg_event_list.)
    virtual void set_start_time(Alg_event *event, double);
    // get text description of run-time errors detected, clear error
    const char *get_last_error_message() { return last_error_message; }
    // Implementation hint: keep a sequence number on each Alg_track that is 
    // incremented anytime there is a structural change. (This behavior is
    // inherited by Alg_seq as well.) Copy the sequence number to any
    // Alg_event_list object when it is created. Whenever you access an 
    // Alg_event_list, using operator[], assert that the Alg_event_list sequence
    // number matches the Alg_seq sequence number. This will guarantee that you
    // do not try to retain pointers to events beyond the point where the events
    // may no longer exist.
} *Alg_event_list_ptr, &Alg_event_list_ref;


// Alg_beat is used to contruct a tempo map
typedef class Alg_beat {
public:
    Alg_beat(double t, double b) {
        time = t; beat = b; }
    Alg_beat() {};
    double time;
    double beat;
} *Alg_beat_ptr;


// Alg_beats is a list of Alg_beat objects used in Alg_seq
typedef class Alg_beats {
private:
    long maxlen;
    void expand();
public:
    long len;
    Alg_beat_ptr beats;
    Alg_beat &operator[](int i) {
        assert(i >= 0 && i < len);
        return beats[i];
    }
    Alg_beats() {
        maxlen = len = 0;
        beats = NULL;
        expand();
        beats[0].time = 0;
        beats[0].beat = 0;
        len = 1;
    }
    ~Alg_beats() {
        if (beats) delete[] beats;
    }
    void insert(long i, Alg_beat_ptr beat);
} *Alg_beats_ptr;


typedef class Alg_time_map {
private:
    int refcount;
public:
    Alg_beats beats; // array of Alg_beat
    double last_tempo;
    bool last_tempo_flag;
    Alg_time_map() {
        last_tempo = ALG_DEFAULT_BPM / 60.0; // note: this value ignored until
                // last_tempo_flag is set; nevertheless, the default
                // tempo is 100.
        last_tempo_flag = true;
        refcount = 0;
    }
    Alg_time_map(Alg_time_map *map); // copy constructor
    long length() { return beats.len; }
    void show();
    long locate_time(double time);
    long locate_beat(double beat);
    double beat_to_time(double beat);
    double time_to_beat(double time);
    // Time map manipulations: it is prefered to call the corresponding
    // methods in Alg_seq. If you manipulate an Alg_time_map directly,
    // you should take care to convert all tracks that use the time map
    // to beats or seconds as appropriate: Normally if you insert a beat
    // you want tracks to be in time units and if you insert a tempo change
    // you want tracks to be in beat units.
    void insert_beat(double time, double beat);   // add a point to the map
    bool insert_tempo(double tempo, double beat); // insert a tempo change
    // get the tempo starting at beat
    double get_tempo(double beat);
    // set the tempo over a region
    bool set_tempo(double tempo, double start_beat, double end_beat);
    bool stretch_region(double b0, double b1, double dur);
    void cut(double start, double len, bool units_are_seconds);
    void trim(double start, double end, bool units_are_seconds);
    void paste(double start, Alg_track *tr);
    // insert a span of time. If start is at a tempo change, then
    // the span of time runs at the changed tempo
    void insert_time(double start, double len);
    // insert a span of beats. If start is at a tempo change, the
    // tempo change takes effect before the inserted beats
    void insert_beats(double start, double len);
    void dereference() {
        if (--refcount <= 0) delete this;
    }
    void reference() {
        refcount++;
    }
} *Alg_time_map_ptr;


// Aligner is a simple class to allow 64-bit data to be stored and
//     retrieved from Serial_buffer, where data is 32-bit aligned,
//     in an architecture-independent fashion. On some architectures,
//     read/writes of int64_t or doubles must be aligned to 8 bytes.
class Aligner {
  public:
    union {
        int64_t i64;
        double d64;
        struct {
            int32_t int32a;
            int32_t int32b;
        };
    };
    Aligner(double d) { d64 = d; }
    Aligner(int64_t i) { i64 = i; }
    Aligner(int32_t a, int32_t b) { int32a = a; int32b = b; }
};

// Serial_buffer is an abstract class with common elements of
//     Serial_read_buffer and Serial_write_buffer
class Serial_buffer {
  protected:
    char *buffer;
    char *ptr;
    long len;
  public:
    Serial_buffer() {
        buffer = NULL;
        ptr = NULL;
        len = 0;
    }
    virtual ~Serial_buffer() { }

    long get_posn() { return (long) (ptr - buffer); }
    long get_len() { return len; }
};


typedef class Serial_read_buffer : public Serial_buffer {
public:
    // note that a Serial_read_buffer is initialized for reading by
    // setting buffer, but it is not the Serial_read_buffer's responsibility
    // to delete the buffer (owner might want to reuse it), so the destructor
    // does nothing.
    virtual ~Serial_read_buffer() {  }
#if defined(_WIN32)
    // TODO: revisit warning disables now that ptr is more properly cast
    // to/from size_t (not long):
#pragma warning(disable: 546) // cast to int is OK, we only want low 7 bits
#pragma warning(disable: 4311) // type cast pointer to long warning
#endif
    void get_pad() { while (((size_t) ptr) & 3) ptr++; }
#if defined(_WIN32)
#pragma warning(default: 4311 546)
#endif
    // Prepare to read n bytes from buf. The caller must manage buf: it is
    // valid until reading is finished, and it is caller's responsibility
    // to free buf when it is no longer needed.
    void init_for_read(void *buf, long n) {
        buffer = (char *) buf;
        ptr = (char *) buf;
        len = n;
    }
    char get_char() { return *ptr++; }
    void unget_chars(int n) { ptr -= n; } // undo n get_char() calls
    double get_double() {
        int32_t a = get_int32();
        int32_t b = get_int32();
        Aligner aligner(a, b);
        return aligner.d64; }
    int32_t get_int32() { int32_t i = *((int32_t *) ptr); ptr += 4; return i; }
    int64_t get_int64();
    float get_float() { float f = *((float *) ptr); ptr += 4; return f; }
    const char *get_string() { char *s = ptr; char *fence = buffer + len;
                         ptr += strlen(s);
                         assert(ptr < fence);
                         while (*ptr++) assert(ptr < fence);
                         get_pad();
                         return s; }
    void check_input_buffer(long needed) {
        assert(get_posn() + needed <= len); }
} *Serial_read_buffer_ptr;


typedef class Serial_write_buffer: public Serial_buffer {
  public:
    // Note: allegro.cpp declares one static instance of Serial_buffer to 
    // reduce large memory (re)allocations when serializing tracks for UNDO.
    // This destructor will only run when the program exits, which will only
    // add overhead to the exit process, but it will eliminate an incorrect
    // report of memory leakage from automation that doesn't know better. -RBD
    virtual ~Serial_write_buffer() {
        if (buffer) delete [] buffer;
    }
    void init_for_write() { ptr = buffer; }
    // store_long writes a long at a given offset
    void store_int32(long offset, int32_t value) {
        assert(offset <= get_posn() - sizeof(value));
        int32_t *loc = (int32_t *) (buffer + offset);
        *loc = value;
    }
    void check_buffer(long needed);
    void set_string(const char *s) { 
        char *fence = buffer + len;
        assert(ptr < fence);
        // two brackets surpress a g++ warning, because this is an
        // assignment operator inside a test.
        while ((*ptr++ = *s++)) assert(ptr < fence);
        // TODO: revisit warning disables because now we are more
        //    properly casting pointer to size_t (not long) and back -RBD
        // 4311 is type cast pointer to long warning
        // 4312 is type cast long to pointer warning
#if defined(_WIN32)
#pragma warning(disable: 4311 4312)
#endif
        assert((char *)(((size_t) (ptr + 3)) & ~3) <= fence);
#if defined(_WIN32)
#pragma warning(default: 4311 4312)
#endif
        pad(); }
    void set_int32(int32_t v) { *((int32_t *) ptr) = v; ptr += 4; }
    void set_double(double v) {
        Aligner aligner(v);
        set_int32(aligner.int32a);
        set_int32(aligner.int32b); }
    void set_float(float v) { *((float *) ptr) = v; ptr += 4; }
    void set_char(char v) { *ptr++ = v; }
#if defined(_WIN32)
    // TODO: reassess warning disables now that pointer is more properly
    // cast to/from size_t (not long):
#pragma warning(disable: 546) // cast to int is OK, we only want low few bits
#pragma warning(disable: 4311) // type cast pointer to long warning
#endif
    void pad() { while (((size_t) ptr) & 3) set_char(0); }
#if defined(_WIN32)
#pragma warning(default: 4311 546)
#endif
    void *to_heap(long *len) {
        *len = get_posn();
        char *newbuf = new char[*len];
        memcpy(newbuf, buffer, *len);
        return newbuf;
    }
} *Serial_write_buffer_ptr;

typedef class Alg_seq *Alg_seq_ptr;

typedef class Alg_track : public Alg_event_list {
protected:
    Alg_time_map *time_map;
    bool units_are_seconds;
    // char *get_string(char **p, long *b);  -- these seem to be orphaned
    // int32_t get_int32(char **p, long *b); -- declarations. Maybe they
    // double get_double(char **p, long *b); -- were intended for 
    // float get_float(char **p, long *b);   -- serialization. Delete them?
    static Serial_read_buffer ser_read_buf;
    static Serial_write_buffer ser_write_buf;
    void serialize_parameter(Alg_parameter *parm);
    // *buffer_ptr points to binary data, bytes_ptr points to how many
    // bytes have been used so far, len is length of binary data
    void unserialize_parameter(Alg_parameter_ptr parm_ptr);
public:
    void serialize_track();
    void unserialize_track();
    virtual Alg_event_ptr &operator[](int i) {
        assert(i >= 0 && i < len);
        return events[i];
    }
    Alg_track() { units_are_seconds = false; time_map = NULL; 
                  set_time_map(NULL); type = 't'; }
    // initialize empty track with a time map
    Alg_track(Alg_time_map *map, bool seconds); 

    Alg_event_ptr copy_event(Alg_event_ptr event); // make a complete copy
    
    Alg_track(Alg_track &track);  // copy constructor, does not copy time_map
    // copy constructor: event_list is copied, map is installed and referenced
    Alg_track(Alg_event_list_ref event_list, Alg_time_map_ptr map, 
              bool units_are_seconds);
    virtual ~Alg_track() { // note: do not call set_time_map(NULL)!
        if (time_map) time_map->dereference(); time_map = NULL; }

    // Returns a buffer containing a serialization of the
    // file.  It will be an ASCII representation unless text is true.
    // *buffer gets a newly allocated buffer pointer. The caller must free it.
    // *len gets the length of the serialized track
    virtual void serialize(void **buffer, long *bytes);

    // Try to read from a memory buffer.  Automatically guess
    // whether it's MIDI or text.
    static Alg_track *unserialize(void *buffer, long len);

    // If the track is really an Alg_seq and you need to access an
    // Alg_seq method, coerce to an Alg_seq with this function:
    Alg_seq_ptr to_alg_seq() {
        return (get_type() == 's' ? (Alg_seq_ptr) this : NULL); }

    // Are we using beats or seconds?
    bool get_units_are_seconds() { return units_are_seconds; }
    // Change units 
    virtual void convert_to_beats();
    virtual void convert_to_seconds();
    void set_dur(double dur);
    double get_dur() { return (units_are_seconds ? real_dur : beat_dur); }

    // Every Alg_track may have an associated time_map. If no map is
    // specified, or if you set_time_map(NULL), then the behavior 
    // should be as if there is a constant tempo of 100 beats/minute
    // (this constant is determined by ALG_DEFAULT_BPM).
    // Recommendation: create a static global tempo map object. When
    // any operation that needs a tempo map gets NULL, use the global
    // tempo map. (Exception: any operation that would modify the
    // tempo map should raise an error -- you don't want to change the
    // default tempo map.)
    virtual void set_time_map(Alg_time_map *map);
    Alg_time_map *get_time_map() { return time_map; }

    // Methods to create events. The returned event is owned by the caller.
    // Use delete to get rid of it unless you call add() -- see below.
    //
    Alg_note *create_note(double time, int channel, int identifier, 
                           float pitch, float loudness, double duration);
    // Note: after create_update(), caller should use set_*_value() to
    // initialize the attribute/value pair:
    Alg_update *create_update(double time, int channel, int identifier);
    // Adds a new event - it is automatically inserted into the
    // correct order in the sequence based on its timestamp.
    // The ownership passes from the caller to this Alg_seq. The
    // event is not copied.
    virtual void add(Alg_event *event) { insert(event); }

    //
    // Editing regions
    //

    // Deletes the notes that start within the given region
    // and returns them in a new sequence.  The start times
    // of the notes in the returned sequence are shifted
    // by -t.  The notes after the region get shifted over
    // to fill the gap. In an Alg_seq, the tempo track is edited
    // in a similar way
    // and the cut tempo information is retained in the new seq.
    // ONLY NOTES THAT START WITHIN THE REGION ARE CUT unless
    // "all" is true in which case all notes that intersect
    // the region are copied. CUT NOTES
    // MAY EXTEND BEYOND THE DURATION OF THE RESULTING SEQ.
    // The return type is the same as this (may be Alg_seq).
    // All times including len are interpreted according to 
    // units_are_seconds in the track.
    virtual Alg_track *cut(double t, double len, bool all);

    // Like cut() but doesn't remove the notes from the original
    // sequence. The Alg_events are copied, not shared. ONLY EVENTS
    // THAT START WITHIN THE REGION ARE COPIED unless "all" is true
    // in which case all notes that intersect the region are
    // copied. COPIED NOTES MAY
    // EXTEND BEYOND THE DURATION OF THE RESULTING SEQ.
    // The return type is the same as this (may be Alg_seq).
    virtual Alg_track *copy(double t, double len, bool all);

    // Inserts a sequence in the middle, shifting some notes
    // over by the duration of the seq, which is first converted
    // to the same units (seconds or beats) as this. (This makes
    // a differece because the pasted data may change the tempo,
    // and notes that overlap the borders will then experience
    // a tempo change.)
    // THE SEQ PARAMETER IS NOT MODIFIED, AND Alg_event's ARE
    // COPIED, NOT SHARED.
    // The type of seq must be Alg_seq if seq is an Alg_seq, or
    // Alg_track if seq is an Alg_track or an Alg_event_list.
    virtual void paste(double t, Alg_event_list *seq); // Shifts notes

    // Merges two sequences with a certain offset. The offset is
    // interpreted as either beats or seconds according to the 
    // current units of this, and seq is converted to the same
    // units as this. Except for a possible conversion to beats
    // or seconds, the tempo track of seq (if any) is ignored. 
    // (There is no way to merge tempo tracks.)
    // THE SEQ PARAMETER IS NOT MODIFIED, AND Alg_event's ARE
    // COPIED, NOT SHARED.
    // The type of seq must be Alg_seq if seq is an Alg_seq, or
    // Alg_track if seq is an Alg_track or an Alg_event_list.
    virtual void merge(double t, Alg_event_list_ptr seq);

    // Deletes and shifts notes to fill the gap. The tempo track
    // is also modified accordingly. ONLY EVENTS THAT START WITHIN
    // THE REGION ARE DELETED unless "all" is true, in which case
    // all notes that intersect the region are cleared.
    // NOTES THAT EXTEND FROM BEFORE THE
    // REGION INTO THE REGION RETAIN THEIR DURATION IN EITHER
    // BEATS OR SECONDS ACCORDING TO THE CURRENT UNITS OF this.
    virtual void clear(double t, double len, bool all);

    // Deletes notes but doesn't shift.  If the "all" argument
    // is true, deletes all notes that intersect the range at all,
    // not just those that start within it. The tempo track is 
    // not affected.
    virtual void silence(double t, double len, bool all);

    // Simply shifts notes past time t over by len, which is given
    // in either beats or seconds according to the units of this.
    // The resulting interveal (t, t+len) may in fact contain notes
    // that begin before t. The durations of notes are not changed.
    // If this is an Alg_seq, the tempo track is expanded at t also.
    virtual void insert_silence(double t, double len);

    //
    // Accessing for screen display
    //

    // A useful generic function to retrieve only certain
    // types of events.  The masks should be bit-masks defined
    // somewhere else. Part of the mask allows us to search for 
    // selected events. If this is an Alg_seq, search all tracks
    // (otherwise, call track[i].find())
    // If channel_mask == 0, accept ALL channels, otherwise
    // accept only channels < 32 where corresponding bit is set in
    // channel_mask.
    virtual Alg_event_list *find(double t, double len, bool all,
                                 int32_t channel_mask, int32_t event_type_mask);

    virtual void set_in_use(bool flag) { in_use = flag; }
    //
    // MIDI playback
    //
    // See Alg_iterator
} *Alg_track_ptr, &Alg_track_ref;


// Alg_time_sig represents a single time signature;
// although not recommended, time_signatures may have arbitrary
// floating point values, e.g. 4.5 beats per measure
typedef class Alg_time_sig {
public:
    double beat; // when does this take effect?
    double num;  // what is the "numerator" (top number?)
    double den;  // what is the "denominator" (bottom number?)
    Alg_time_sig(double b, double n, double d) {
        beat = b; num = n; den = d;
    }
    Alg_time_sig() {
        beat = 0; num = 0; den = 0;
    }
    void beat_to_measure(double beat, double *measure, double *m_beat,
                         double *num, double *den);

} *Alg_time_sig_ptr;


// Alg_time_sigs is a dynamic array of time signatures
//
// The default (empty) time_sigs has 4/4 time at beat 0.
// Each time_sig object in time_sigs represents the beginning
// of a measure. If there is a beat missing, e.g. in the first
// measure, you can represent this by inserting another 
// time_sig at the next measure beginning. Each time_sig implies
// an infinite sequence of full measures until the next time_sig.
// If you insert a time_sig and one already exist near the same
// beat, the old one is replaced, thus re-barring every measure
// until the next time_sig.
class Alg_time_sigs {
private:
    long maxlen;
    void expand(); // make more space
    long len;
    Alg_time_sig_ptr time_sigs;
public:
    Alg_time_sigs() {
        maxlen = len = 0;
        time_sigs = NULL;
    }
    Alg_time_sig &operator[](int i) { // fetch a time signature
        assert(i >= 0 && i < len);
        return time_sigs[i];
    }
    ~Alg_time_sigs() {
        if (time_sigs) delete[] time_sigs;
    }
    void show();
    long length() { return len; }
    int find_beat(double beat);
    // get the number of beats per measure starting at beat
    double get_bar_len(double beat);
    void insert(double beat, double num, double den, bool force = false);
    void cut(double start, double end, double dur); // remove from start to end
    void trim(double start, double end); // retain just start to end
    void paste(double start, Alg_seq *seq);
    void insert_beats(double beat, double len); // insert len beats at beat
    // find the nearest beat (see Alg_seq::nearest_beat) to beat
    double nearest_beat(double beat);
};


// a sequence of Alg_events objects
typedef class Alg_tracks {
private:
    long maxlen;
    void expand();
    void expand_to(int new_max);
    long len;
public:
    Alg_track_ptr *tracks; // tracks is array of pointers
    Alg_track &operator[](int i) {
        assert(i >= 0 && i < len);
        return *tracks[i];
    }
    long length() { return len; }
    Alg_tracks() {
        maxlen = len = 0;
        tracks = NULL;
    }
    ~Alg_tracks();
    // Append a track to tracks. This Alg_tracks becomes the owner of track.
    void append(Alg_track_ptr track);
    void add_track(int track_num, Alg_time_map_ptr time_map, bool seconds);
    void reset();
    void set_in_use(bool flag); // handy to set in_use flag on all tracks
} *Alg_tracks_ptr;


typedef enum {
    alg_no_error = 0,      // no error reading Allegro or MIDI file
    alg_error_open = -800, // could not open Allegro or MIDI file
    alg_error_syntax   // something found in the file that could not be parsed;
    // generally you should ignore syntax errors or look at the printed error 
    // messages because there are some things in standard midi files that we do
    // not handle; (maybe we should only set alg_error_syntax when there is a
    // real problem with the file as opposed to when there is some warning
    // message for the user)
} Alg_error;


typedef struct Alg_pending_event {
    void *cookie; // client-provided sequence identifier
    Alg_events *events; // the array of events
    long index; // offset of this event
    bool note_on; // is this a note-on or a note-off (if applicable)?
    double offset; // time offset for events
    double time; // time for this event
} *Alg_pending_event_ptr;


typedef class Alg_iterator {
private:
    long maxlen;
    void expand();
    void expand_to(int new_max);
    long len;
    Alg_seq_ptr seq;
    Alg_pending_event *pending_events;
    // the next four fields are mainly for request_note_off()
    Alg_events_ptr events_ptr; // remembers events containing current event
    long index; // remembers index of current event
    void *cookie; // remembers the cookie associated with next event
    double offset;
    void show();
    bool earlier(int i, int j);
    void insert(Alg_events_ptr events, long index, bool note_on, 
                void *cookie, double offset);
    // returns the info on the next pending event in the priority queue
    bool remove_next(Alg_events_ptr &events, long &index, bool &note_on,
                     void *&cookie, double &offset, double &time);
public:
    bool note_off_flag; // remembers if we are iterating over note-off
                        // events as well as note-on and update events
    long length() { return len; }
    Alg_iterator(Alg_seq_ptr s, bool note_off) {
        seq = s;
        note_off_flag = note_off;
        maxlen = len = 0;
        pending_events = NULL;
    }
    // Normally, iteration is over the events in the one sequence used
    // to instatiate the iterator (see above), but with this method, you
    // can add more sequences to the iteration. Events are returned in
    // time order, so effectively sequence events are merged.
    // The optional offset is added to each event time of sequence s
    // before merging/sorting. You should call begin_seq() for each
    // sequence to be included in the iteration unless you call begin()
    // (see below).
    void begin_seq(Alg_seq_ptr s, void *cookie = NULL, double offset = 0.0);
    ~Alg_iterator();
    // Prepare to enumerate events in order. If note_off_flag is true, then
    // iteration_next will merge note-off events into the sequence. If you
    // call begin(), you should not normally call begin_seq(). See above.
    void begin(void *cookie = NULL) { begin_seq(seq, cookie); }
    // return next event (or NULL). If iteration_begin was called with
    // note_off_flag = true, and if note_on is not NULL, then *note_on
    // is set to true when the result value represents a note-on or update.
    // (With note_off_flag, each Alg_note event is returned twice, once
    // at the note-on time, with *note_on == true, and once at the note-off
    // time, with *note_on == false. If a cookie_ptr is passed, then the
    // cookie corresponding to the event is stored at that address
    // If end_time is 0, iterate through the entire sequence, but if
    // end_time is non_zero, stop iterating at the last event before end_time
    Alg_event_ptr next(bool *note_on = NULL, void **cookie_ptr = NULL,
                       double *offset_ptr = NULL, double end_time = 0); 
    // Sometimes, the caller wants to receive note-off events for a subset
    // of the notes, typically the notes that are played and need to be
    // turned off. In this case, when a note is turned on, the client
    // should call request_note_off(). This will insert a note-off into
    // the queue for the most recent note returned by next(). 
    void request_note_off();
    void end();   // clean up after enumerating events
} *Alg_iterator_ptr;


// An Alg_seq is an array of Alg_events, each a sequence of Alg_event, 
// with a tempo map and a sequence of time signatures
//
typedef class Alg_seq : public Alg_track {
protected:
    Alg_iterator_ptr pending; // iterator used internally by Alg_seq methods
    void serialize_seq();
    Alg_error error; // error code set by file readers
    // an internal function used for writing Allegro track names
    Alg_event_ptr write_track_name(std::ostream &file, int n, 
                                   Alg_events &events);
public:
    int channel_offset_per_track; // used to encode track_num into channel
    Alg_tracks track_list;       // array of Alg_events
    Alg_time_sigs time_sig;
    int beat_x;
    void basic_initialization() {
        error = alg_no_error;
        units_are_seconds = true; type = 's';
        channel_offset_per_track = 0;
        add_track(0); // default is one empty track
    }        
    Alg_seq() {
        basic_initialization();
    }
    // copy constructor -- if track is an Alg_seq, make a copy; if
    //    track is just an Alg_track, the track becomes track 0
    Alg_seq(Alg_track_ref track) { seq_from_track(track); }
    Alg_seq(Alg_track_ptr track) { seq_from_track(*track); }
    void seq_from_track(Alg_track_ref tr);
    // create from file:
    Alg_seq(std::istream &file, bool smf, double *offset_ptr = NULL);
    // create from filename
    Alg_seq(const char *filename, bool smf, double *offset_ptr = NULL);
    virtual ~Alg_seq();
    int get_read_error() { return error; }
    void serialize(void **buffer, long *bytes);
    void copy_time_sigs_to(Alg_seq *dest); // a utility function
    void set_time_map(Alg_time_map *map);

    // encode sequence structure into contiguous, moveable memory block
    // address of newly allocated memory is assigned to *buffer, which must
    // be freed by caller; the length of data is assigned to *len
    void unserialize_seq();

    // write an ascii representation to file
    void write(std::ostream &file, bool in_secs, double offset = 0.0);
    // returns true on success
    bool write(const char *filename, double offset = 0.0);
    void smf_write(std::ostream &file);
    bool smf_write(const char *filename);

    // Returns the number of tracks
    int tracks();

    // create a track
    void add_track(int track_num) { 
        track_list.add_track(track_num, get_time_map(), units_are_seconds); 
    }

    // Return a particular track. This Alg_seq owns the track, so the
    // caller must not delete the result.
    Alg_track_ptr track(int);

    virtual Alg_event_ptr &operator[](int i);

    virtual void convert_to_seconds();
    virtual void convert_to_beats();

    Alg_track_ptr cut_from_track(int track_num, double start, double dur, 
                                 bool all);
    Alg_seq *cut(double t, double len, bool all);
    void insert_silence_in_track(int track_num, double t, double len);
    void insert_silence(double t, double len);
    Alg_track_ptr copy_track(int track_num, double t, double len, bool all);
    Alg_seq *copy(double start, double len, bool all);
    void paste(double start, Alg_seq *seq);
    virtual void clear(double t, double len, bool all);
    virtual void merge(double t, Alg_event_list_ptr seq);
    virtual void silence(double t, double len, bool all);
    void clear_track(int track_num, double start, double len, bool all);
    void silence_track(int track_num, double start, double len, bool all);
    Alg_event_list_ptr find_in_track(int track_num, double t, double len,
                                     bool all, int32_t channel_mask, 
                                     int32_t event_type_mask);

    // find index of first score event after time
    long seek_time(double time, int track_num);
    bool insert_beat(double time, double beat);
    // return the time of the beat nearest to time, also returns beat
    // number through beat. This will correspond to an integer number
    // of beats from the nearest previous time signature or 0.0, but
    // since time signatures need not be on integer beat boundaries
    // the beat location may not be on an integer beat (beat locations
    // are measured from the beginning which is beat 0.
    double nearest_beat_time(double time, double *beat);
    // warning: insert_tempo may change representation from seconds to beats
    bool insert_tempo(double bpm, double beat);
    // change the duration from b0 to b1 (beats) to dur (seconds) by
    // scaling the intervening tempos
    bool stretch_region(double b0, double b1, double dur);
    // add_event takes a pointer to an event on the heap. The event is not
    // copied, and this Alg_seq becomes the owner and freer of the event.
    void add_event(Alg_event_ptr event, int track_num);
    void add(Alg_event_ptr event) { assert(false); } // call add_event instead
    // get the tempo starting at beat
    double get_tempo(double beat);
    bool set_tempo(double bpm, double start_beat, double end_beat);

    // get the bar length in beats starting at beat
    double get_bar_len(double beat);
    void set_time_sig(double beat, double num, double den);
    void beat_to_measure(double beat, long *measure, double *m_beat,
                         double *num, double *den);
    // void set_events(Alg_event_ptr *events, long len, long max);
    void merge_tracks();    // move all track data into one track
    void set_in_use(bool flag); // set in_use flag on all tracks
} *Alg_seq_ptr, &Alg_seq_ref;


// see Alg_seq::Alg_seq() constructors that read from files
// the following are for internal library implementation and are
// moved to *_internal.h header files.
//Alg_seq_ptr alg_read(std::istream &file, Alg_seq_ptr new_seq);
//Alg_seq_ptr alg_smf_read(std::istream &file, Alg_seq_ptr new_seq);
#endif
