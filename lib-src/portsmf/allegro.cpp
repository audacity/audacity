// Allegro: music representation system, with
//      extensible in-memory sequence structure
//      upward compatible with MIDI
//      implementations in C++ and Serpent
//      external, text-based representation
//      compatible with Aura
//
/* CHANGE LOG:
04 apr 03 -- fixed bug in add_track that caused infinite loop
*/

#include "assert.h"
#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "memory.h"
#include <iostream>
#include <fstream>
using namespace std;
#include "allegro.h"
#include "algrd_internal.h"
#include "algsmfrd_internal.h"
// #include "trace.h" -- only needed for debugging
#include "math.h"

#define STREQL(x, y) (strcmp(x, y) == 0)
#define MAX(x, y) ((x) > (y) ? (x) : (y))

// 4311 is type cast ponter to long warning
// 4996 is warning against strcpy
// 4267 is size_t to long warning
#pragma warning(disable: 4311 4996 4267)
Alg_atoms symbol_table;
Serial_read_buffer Alg_track::ser_read_buf; // declare the static variables
Serial_write_buffer Alg_track::ser_write_buf; 

bool within(double d1, double d2, double epsilon)
{
    d1 -= d2;
    return d1 < epsilon && d1 > -epsilon;
}


char *heapify(const char *s)
{
    char *h = new char[strlen(s) + 1];
    strcpy(h, s);
    return h;
}


void Alg_atoms::expand()
{
    maxlen = (maxlen + 5);   // extra growth for small sizes
    maxlen += (maxlen >> 2); // add 25%
    char **new_atoms = new Alg_attribute[maxlen];
    // now do copy
    memcpy(new_atoms, atoms, len * sizeof(Alg_attribute));
    if (atoms) delete[] atoms;
    atoms = new_atoms;
}


// insert_new -- insert an attribute name and type
//
// attributes are stored as a string consisting of the type
// (a char) followed by the attribute name. This makes it
// easy to retrieve the type or the name or both.
//
Alg_attribute Alg_atoms::insert_new(const char *name, char attr_type)
{
    if (len == maxlen) expand();
    char *h = new char[strlen(name) + 2];
    strcpy(h + 1, name);
    *h = attr_type;
    atoms[len++] = h;
    return h;
}


Alg_attribute Alg_atoms::insert_attribute(Alg_attribute attr)
{
    for (int i = 0; i < len; i++) {
        if (STREQL(attr, atoms[i])) {
            return atoms[i];
        }
    }
    return insert_new(attr + 1, attr[0]);
}


Alg_attribute Alg_atoms::insert_string(const char *name)
{
    char attr_type = name[strlen(name) - 1];
    for (int i = 0; i < len; i++) {
        if (attr_type == atoms[i][0] &&
            STREQL(name, atoms[i] + 1)) {
            return atoms[i];
        }
    }
    return insert_new(name, attr_type);
}


void Alg_parameter::copy(Alg_parameter_ptr parm)
{
    *this = *parm; // copy all fields
    // if the value is a string, copy the string
    if (attr_type() == 's') {
        s = heapify(s);
    }
}


void Alg_parameter::show()
{
    switch (attr[0]) {
    case 'r':
        printf("%s:%g", attr_name(), r);
        break;
    case 's':
        printf("%s:%s", attr_name(), s);
        break;
    case 'i':
        printf("%s:%d", attr_name(), i);
        break;
    case 'l':
        printf("%s:%s", attr_name(), (l ? "t" : "f"));
        break;
    case 'a':
        printf("%s:%s", attr_name(), a);
        break;
    }
}


Alg_parameter::~Alg_parameter()
{
    if (attr_type() == 's' && s) {
        delete[] s;
    }
}


void Alg_parameters::insert_real(Alg_parameters **list, char *name, double r)
{
    Alg_parameters_ptr a = new Alg_parameters(*list);
    *list = a;
    a->parm.set_attr(symbol_table.insert_string(name));
    a->parm.r = r;
    assert(a->parm.attr_type() == 'r');
}


void Alg_parameters::insert_string(Alg_parameters **list, char *name, char *s)
{
    Alg_parameters_ptr a = new Alg_parameters(*list);
    *list = a;
    a->parm.set_attr(symbol_table.insert_string(name));
    // string is deleted when parameter is deleted
    a->parm.s = heapify(s);
    assert(a->parm.attr_type() == 's');
}


void Alg_parameters::insert_integer(Alg_parameters **list, char *name, long i)
{
    Alg_parameters_ptr a = new Alg_parameters(*list);
    *list = a;
    a->parm.set_attr(symbol_table.insert_string(name));
    a->parm.i = i;
    assert(a->parm.attr_type() == 'i');
}


void Alg_parameters::insert_logical(Alg_parameters **list, char *name, bool l)
{
    Alg_parameters_ptr a = new Alg_parameters(*list);
    *list = a;
    a->parm.set_attr(symbol_table.insert_string(name));
    a->parm.l = l;
    assert(a->parm.attr_type() == 'l');
}


void Alg_parameters::insert_atom(Alg_parameters **list, char *name, char *s)
{
    Alg_parameters_ptr a = new Alg_parameters(*list);
    *list = a;
    a->parm.set_attr(symbol_table.insert_string(name));
    a->parm.a = symbol_table.insert_string(s);
    assert(a->parm.attr_type() == 'a');
}


Alg_parameters *Alg_parameters::remove_key(Alg_parameters **list, char *name)
{
    while (*list) {
        if (STREQL((*list)->parm.attr_name(), name)) {
            Alg_parameters_ptr p = *list;
            *list = p->next;
            p->next = NULL;
            return p; // caller should free this pointer
        }
        list = &((*list)->next);
    }
    return NULL;
}


Alg_parameter_ptr Alg_parameters::find(Alg_attribute *attr)
{
    assert(attr);
    Alg_parameters_ptr temp = this;
    while (temp) {
        if (temp->parm.attr == *attr) {
            return &(temp->parm);
        }
    }
    return NULL;
}


int Alg_event::get_type_code()
{
    if (!is_note()) {
        const char* attr = get_attribute();
        if (STREQL(attr, "gater"))         // volume change
            return ALG_GATE;
        if (STREQL(attr, "bendr"))         // pitch bend     
            return ALG_BEND;
        if (strncmp(attr, "control", 7) == 0)      // control change
            // note that midi control changes have attributes of the form
            // "control<n>" where n is the decimal number (as a character string)
            // of the midi controller, e.g. control2 is the breath controller.
            // We don't check for decimal numbers in the range 0-127, so any
            // attribute that begins with "control" is an ALG_CONTROL:
            return ALG_CONTROL;
        if (STREQL(attr, "programi"))      // program change
            return ALG_PROGRAM;
        if (STREQL(attr, "pressurer"))    // pressure change
            return ALG_PRESSURE;
        if (STREQL(attr, "keysigi"))       // key signature  
            return ALG_KEYSIG;
        if (STREQL(attr, "timesig_numi"))  // time signature numerator
            return ALG_TIMESIG_NUM;
        if (STREQL(attr, "timesig_deni"))  // time signature denominator
            return ALG_TIMESIG_DEN;
        return ALG_OTHER;
    }
    return ALG_NOTE; // it is a note
}


void Alg_event::set_parameter(Alg_parameter_ptr new_parameter)
{
    Alg_parameter_ptr parm;
    if (is_note()) {
        Alg_note_ptr note = (Alg_note_ptr) this;
        parm = note->parameters->find(&(new_parameter->attr));
        if (!parm) {
            note->parameters = new Alg_parameters(note->parameters);
            parm = &(note->parameters->parm);
        }
    } else { // update
        Alg_update_ptr update = (Alg_update_ptr) this;
        parm = &(update->parameter);
    }
    parm->copy(new_parameter); // copy entire parameter
}


void Alg_event::set_string_value(char *a, char *value)
{
    assert(a); // must be non-null
    Alg_attribute attr = symbol_table.insert_string(a);
    assert(attr[0] == 's');
    Alg_parameter parm;
    parm.set_attr(attr);
    parm.s = value;
    set_parameter(&parm);
    parm.s = NULL; // do this to prevent string from being freed
}


void Alg_event::set_real_value(char *a, double value)
{
    assert(a); // must be non-null
    // attr is like a, but it has the type code prefixed for
    // fast lookup, and it is a unique string in symbol_table
    // e.g. a="attackr" -> attr="rattackr"
    Alg_attribute attr = symbol_table.insert_string(a);
    assert(attr[0] == 'r');
    Alg_parameter parm;
    parm.set_attr(attr);
    parm.r = value;
    set_parameter(&parm);
    // since type is 'r' we don't have to NULL the string
}


void Alg_event::set_logical_value(char *a, bool value)
{
    assert(a); // must be non-null
    Alg_attribute attr = symbol_table.insert_string(a);
    assert(attr[0] == 'l');
    Alg_parameter parm;
    parm.set_attr(attr);
    parm.l = value;
    set_parameter(&parm);
    // since type is 'l' we don't have to NULL the string
}


void Alg_event::set_integer_value(char *a, long value)
{
    assert(a); // must be non-null
    Alg_attribute attr = symbol_table.insert_string(a);
    assert(attr[0] == 'i');
    Alg_parameter parm;
    parm.set_attr(attr);
    parm.i = value;
    set_parameter(&parm);
    // since tpye is 'i' we don't have to NULL the string
}


void Alg_event::set_atom_value(char *a, char *value)
{
    assert(a); // must be non-null
    Alg_attribute attr = symbol_table.insert_string(a);
    assert(attr[0] == 'a');
    Alg_parameter parm;
    parm.set_attr(attr);
    parm.a = value;
    set_parameter(&parm);
    /* since type is 'a' we don't have to null the string */
}


float Alg_event::get_pitch()
{
    assert(is_note());
    Alg_note* note = (Alg_note *) this;
    return note->pitch;
}


float Alg_event::get_loud()
{
    assert(is_note());
    Alg_note* note = (Alg_note *) this;
    return note->loud;
}


double Alg_event::get_start_time()
{
    assert(is_note());
    Alg_note* note = (Alg_note *) this;
    return note->time;
}


double Alg_event::get_end_time()
{
    assert(is_note());
    Alg_note* note = (Alg_note *) this;
    return note->time + note->dur;
}


double Alg_event::get_duration()
{
    assert(is_note());
    Alg_note* note = (Alg_note *) this;
    return note->dur;
}


void Alg_event::set_pitch(float p)
{
    assert(is_note());
    Alg_note* note = (Alg_note *) this;
    note->pitch = p;
}

void Alg_event::set_loud(float l)
{
    assert(is_note());
    Alg_note *note = (Alg_note *) this;
    note->loud = l;
}


void Alg_event::set_duration(double d)
{
    assert(is_note());
    Alg_note* note = (Alg_note *) this;
    note->dur = d;
}


bool Alg_event::has_attribute(char *a)
{
    assert(is_note());
    assert(a); // must be non-null
    Alg_note* note = (Alg_note *) this;
    Alg_attribute attr = symbol_table.insert_string(a);
    Alg_parameter_ptr parm = note->parameters->find(&attr);
    return parm != NULL;
}


char Alg_event::get_attribute_type(char *a)
{
    assert(is_note());
    assert(a);
    return a[strlen(a) - 1];
}


char *Alg_event::get_string_value(char *a, char *value)
{
    assert(is_note());
    assert(a); // must be non-null
    Alg_note* note = (Alg_note *) this;
    Alg_attribute attr = symbol_table.insert_string(a);
    assert(a[0] == 's'); // must be of type string
    Alg_parameter_ptr parm = note->parameters->find(&attr);
    if (parm) return parm->s;
    return value;
}


double Alg_event::get_real_value(char *a, double value)
{	
    assert(is_note());
    assert(a);
    Alg_note* note = (Alg_note *) this;
    Alg_attribute attr = symbol_table.insert_string(a);
    assert(a[0] == 'r'); // must be of type real
    Alg_parameter_ptr parm = note->parameters->find(&attr);
    if (parm) return parm->r;
    return value;
}


bool Alg_event::get_logical_value(char *a, bool value)
{	
    assert(is_note());
    assert(a);
    Alg_note* note = (Alg_note *) this;
    Alg_attribute attr = symbol_table.insert_string(a);
    assert(a[0] == 'l'); // must be of type logical
    Alg_parameter_ptr parm = note->parameters->find(&attr);
    if (parm) return parm->l;
    return value;
}


long Alg_event::get_integer_value(char *a, long value)
{	
    assert(is_note());
    assert(a);
    Alg_note* note = (Alg_note *) this;
    Alg_attribute attr = symbol_table.insert_string(a);
    assert(a[0] == 'i'); // must be of type integer
    Alg_parameter_ptr parm = note->parameters->find(&attr);
    if (parm) return parm->i;
    return value;
}


char *Alg_event::get_atom_value(char *a, char *value)
{	
    assert(is_note());
    assert(a);
    Alg_note* note = (Alg_note *) this;
    Alg_attribute attr = symbol_table.insert_string(a);
    assert(a[0] == 'a'); // must be of type atom
    Alg_parameter_ptr parm = note->parameters->find(&attr);
    if (parm) return parm->a;
    // if default is a string, convert to an atom (unique
    // string in symbol table) and return it
    return (value == NULL ? NULL :
              symbol_table.insert_string(value));
}


void Alg_event::delete_attribute(char *a)
{
    assert(is_note());
    Alg_note* note = (Alg_note *) this;
    Alg_parameters::remove_key(&(note->parameters), a);
}


const char *Alg_event::get_attribute()
// Note: this returns a string, not an Alg_attribute
{
    assert(is_update());
    Alg_update* update = (Alg_update *) this;
    return update->parameter.attr_name();
}


char Alg_event::get_update_type()
{
    assert(is_update());
    Alg_update* update = (Alg_update *) this;
    return update->parameter.attr_type();
}


char *Alg_event::get_string_value()
{
    assert(is_update());
    Alg_update* update = (Alg_update *) this;
    assert(get_update_type() == 's');
    return update->parameter.attr_name();
}


double Alg_event::get_real_value()
{
    assert(is_update());
    Alg_update* update = (Alg_update *) this;
    assert(get_update_type() == 'r');
    return update->parameter.r;
}


bool Alg_event::get_logical_value()
{
    assert(is_update());
    Alg_update* update = (Alg_update *) this;
    assert(get_update_type() == 'l');
    return update->parameter.l;
}


long Alg_event::get_integer_value()
{
    assert(is_update());
    Alg_update* update = (Alg_update *) this;
    assert(get_update_type() == 'i');
    return update->parameter.i;
}


char *Alg_event::get_atom_value()
{
    assert(is_update());
    Alg_update* update = (Alg_update *) this;
    assert(get_update_type() == 'a');
    return update->parameter.a;
}


bool Alg_event::overlap(double t, double len, bool all)
{
    // event starts within region
    if (time >= t && time <= t + len - ALG_EPS)
        return true;
    if (all && is_note()) {
        double dur = ((Alg_note_ptr) this)->dur;
        // note ends within region
        if (time < t && time + dur - ALG_EPS > t)
            return true;
    }
    // does not overlap
    return false;
}


Alg_note::Alg_note(Alg_note_ptr note)
{
    *this = *note; // copy all fields
    // parameters is now a shared pointer. We need to copy the 
    // parameters
    Alg_parameters_ptr next_param_ptr = parameters;
    while (next_param_ptr) {
        Alg_parameters_ptr new_params = new Alg_parameters(next_param_ptr->next);
        new_params->parm.copy(&(next_param_ptr->parm)); // copy the attribute and value
        next_param_ptr = new_params->next;
    }
}


Alg_note::~Alg_note()
{
    while (parameters) {
        Alg_parameters_ptr to_delete = parameters;
        parameters = parameters->next;
        delete to_delete;
    }
}


void Alg_note::show()
{
    printf("Alg_note: time %g, chan %d, dur %g, key %d, "
           "pitch %g, loud %g, attributes ",
           time, chan, dur, key, pitch, loud);
    Alg_parameters_ptr parms = parameters;
    while (parms) {
        parms->parm.show();
        printf(" ");
        parms = parms->next;
    }
    printf("\n");
}


Alg_update::Alg_update(Alg_update_ptr update)
{
    *this = *update; // copy all fields
    // parameter requires careful copy to possibly duplicate string value:
    this->parameter.copy(&(update->parameter));
}


void Alg_update::show()
{
    printf("Alg_update: ");
    parameter.show();
    printf("\n");
}


void Alg_events::expand()
{
    maxlen = (maxlen + 5);   // extra growth for small sizes
    maxlen += (maxlen >> 2); // add 25%
    Alg_event_ptr *new_events = new Alg_event_ptr[maxlen];
    // now do copy
    memcpy(new_events, events, len * sizeof(Alg_event_ptr));
    if (events) delete[] events;
    events = new_events;
}


void Alg_events::insert(Alg_event_ptr event)
{
    if (maxlen <= len) {
        expand();
    }
    // Note: if the new event is the last one, the assignment
    // events[i] = event; (below) will never execute, so just
    // in case, we do the assignment here. events[len] will
    // be replaced during the memmove() operation below if
    // this is not the last event.
    events[len] = event;
    len++;
    // find insertion point: (this could be a binary search)
    for (int i = 0; i < len; i++) {
        if (events[i]->time > event->time) {
            // insert event at i
            memmove(&events[i + 1], &events[i], 
                    sizeof(Alg_event_ptr) * (len - i - 1));
            events[i] = event;
            return;
        }
    }
}

Alg_event_ptr Alg_events::uninsert(long index)
{
    assert(0 <= index && index < len);
    Alg_event_ptr event = events[index];
    printf("memmove: %x from %x (%d)\n", events + index, events + index + 1,
            sizeof(Alg_event_ptr) * (len - index - 1));
    memmove(events + index, events + index + 1,
            sizeof(Alg_event_ptr) * (len - index - 1));
    len--;
    return event;
}


void Alg_events::append(Alg_event_ptr event)
{
    if (maxlen <= len) {
        expand();
    }
    events[len++] = event;
    // keep track of last note_off time
    if (event->is_note()) {
        Alg_note_ptr note = (Alg_note_ptr) event;
        double note_off = note->time + note->dur;
        if (note_off > last_note_off)
            last_note_off = note_off;
    }
}


Alg_events::~Alg_events()
{
    // individual events are not deleted, only the array
    if (events) {
        delete[] events;
    }
}


Alg_event_list::Alg_event_list(Alg_track *owner)
{
        events_owner = owner;
        sequence_number = owner->sequence_number; 
        beat_dur = 0.0; real_dur = 0.0; type = 'e';
}


Alg_event_ptr &Alg_event_list::operator [](int i) 
{
    assert(i >= 0 && i < len);
    return events[i];
}


Alg_event_list::~Alg_event_list()
{
    // note that the events contained in the list are not destroyed
}


void Alg_event_list::set_start_time(Alg_event *event, double t)
{
    // For Alg_event_list, find the owner and do the update there
    // For Alg_track, change the time and move the event to the right place
    // For Alg_seq, find the track and do the update there
    
    long index, i;
    Alg_track_ptr track_ptr;
    if (type == 'e') { // this is an Alg_event_list
        // make sure the owner has not changed its event set
        assert(events_owner && 
               sequence_number == events_owner->sequence_number);
        // do the update on the owner
        events_owner->set_start_time(event, t);
        return;
    } else if (type == 't') { // this is an Alg_track
        // find the event in the track
        track_ptr = (Alg_track_ptr) this;
        // this should be a binary search since events are in time order
        // probably there should be member function to do the search
        for (index = 0; index < length(); index++) {
            if ((*track_ptr)[index] == event) goto found_event;
        }
    } else { // type == 's', an Alg_seq
        Alg_seq_ptr seq = (Alg_seq_ptr) this;
        for (i = 0; i < seq->tracks(); i++) {
            track_ptr = seq->track(i);
            // if you implemented binary search, you could call it
            // instead of this loop too.
            for (index = 0; index < track_ptr->length(); index++) {
                if ((*track_ptr)[index] == event) goto found_event;
            }
        }
    }
    assert(false); // event not found seq or track!
  found_event:
    // at this point, track[index] == event
    // we could be clever and figure out exactly what notes to move
    // but it is simpler to just remove the event and reinsert it:
    track_ptr->uninsert(index);
    event->time = t;
    track_ptr->insert(event);
}


void Alg_beats::expand()
{
    maxlen = (maxlen + 5);   // extra growth for small sizes
    maxlen += (maxlen >> 2); // add 25%
    Alg_beat_ptr new_beats = new Alg_beat[maxlen];
    // now do copy
    memcpy(new_beats, beats, len * sizeof(Alg_beat));
    if (beats) delete[] beats;
    beats = new_beats;
}


void Alg_beats::insert(long i, Alg_beat_ptr beat)
{
    assert(i >= 0 && i <= len);
    if (maxlen <= len) {
        expand();
    }
    memmove(&beats[i + 1], &beats[i], sizeof(Alg_beat) * (len - i));
    memcpy(&beats[i], beat, sizeof(Alg_beat));
    len++;
}


Alg_time_map::Alg_time_map(Alg_time_map *map)
{
    refcount = 0;
    assert(map->beats[0].beat == 0 && map->beats[0].time == 0);
    assert(map->beats.len > 0);
    // new_beats[0] = map->beats[0]; 
       // this is commented because
       // both new_beats[0] and map->beats[0] should be (0, 0)
    for (int i = 1; i < map->beats.len; i++) {
        beats.insert(i, &map->beats[i]);
    }
    last_tempo = map->last_tempo;
    last_tempo_flag = map->last_tempo_flag;
}


void Alg_time_map::show()
{
    printf("Alg_time_map: ");
    for (int i = 0; i < beats.len; i++) {
        Alg_beat &b = beats[i];
        printf("(%g, %g) ", b.time, b.beat);
    }
    printf("last tempo: %g\n", last_tempo);
}


long Alg_time_map::locate_time(double time)
{
    int i = 0;
    while ((i < beats.len) && (time > beats[i].time)) {
        i++;
    }
    return i;
}


long Alg_time_map::locate_beat(double beat)
{
    int i = 0;
    while ((i < beats.len) && (beat > beats[i].beat)) {
        i++;
    }
    return i;
}


double Alg_time_map::beat_to_time(double beat)
{
    Alg_beat_ptr mbi;
    Alg_beat_ptr mbi1;
    if (beat <= 0) {
        return beat;
    }
    int i = locate_beat(beat);
    if (i == beats.len) {
        if (last_tempo_flag) {
            return beats[i - 1].time + 
                   (beat - beats[i - 1].beat) / last_tempo;
        } else if (i == 1) {
            return beat * 60.0 / ALG_DEFAULT_BPM;
                // so we use that as default allegro tempo too
        } else {
            mbi = &beats[i - 2];
            mbi1 = &beats[i - 1];
        }
    } else {
        mbi = &beats[i - 1];
        mbi1 = &beats[i];
    }
    // whether w extrapolate or interpolate, the math is the same
    double time_dif = mbi1->time - mbi->time;
    double beat_dif = mbi1->beat - mbi->beat;
    return mbi->time + (beat - mbi->beat) * time_dif / beat_dif;
}


double Alg_time_map::time_to_beat(double time)
{
    Alg_beat_ptr mbi;
    Alg_beat_ptr mbi1;
    if (time <= 0.0) return time;
    int i = locate_time(time);
    if (i == beats.len) {
        if (last_tempo_flag) {
            return beats[i - 1].beat + 
                   (time - beats[i - 1].time) * last_tempo;
        } else if (i == 1) {
            return time * (ALG_DEFAULT_BPM / 60.0);
        } else {
            mbi = &beats[i - 2];
            mbi1 = &beats[i - 1];
        }
    } else {
        mbi = &beats[i - 1];
        mbi1 = & beats[i];
    }
    double time_dif = mbi1->time - mbi->time;
    double beat_dif = mbi1->beat - mbi->beat;
    return mbi->beat + (time - mbi->time) * beat_dif / time_dif;
}


void Alg_time_map::insert_beat(double time, double beat)
{
    int i = locate_time(time); // i is insertion point
    if (i < beats.len && within(beats[i].time, time, 0.000001)) {
        // replace beat if time is already in the map
        beats[i].beat = beat;
    } else {
        Alg_beat point;
        point.beat = beat;
        point.time = time;
        beats.insert(i, &point);
    }
    // beats[i] contains new beat
    // make sure we didn't generate a zero tempo.
    // if so, space beats by one microbeat as necessary
    long j = i;
    if (j == 0) j = 1; // do not adjust beats[0]
    while (j < beats.len &&
        beats[j - 1].beat + 0.000001 >= beats[j].beat) {
        beats[j].beat = beats[j - 1].beat + 0.000001;
        j++;
    }
}


bool Alg_time_map::insert_tempo(double tempo, double beat)
{
    tempo = tempo / 60.0; // convert to beats per second
    // change the tempo at the given beat until the next beat event
    if (beat < 0) return false;
    double time = beat_to_time(beat);
    long i = locate_time(time);
    if (i >= beats.len || !within(beats[i].time, time, 0.000001)) {
        insert_beat(time, beat);
    }
    // now i is index of beat where tempo will change
    if (i == beats.len - 1) {
        last_tempo = tempo;
        // printf("last_tempo to %g\n", last_tempo);
        last_tempo_flag = true;
    } else { // adjust all future beats
        // compute the difference in beats
        double diff = beats[i + 1].beat - beats[i].beat;
        // convert beat difference to seconds at new tempo
        diff = diff / tempo;
        // figure out old time difference:
        double old_diff = beats[i + 1].time - time;
        // compute difference too
        diff = diff - old_diff;
        // apply new_diff to score and beats
        while (i < beats.len) {
            beats[i].time = beats[i].time + diff;
            i++;
        }
    }
    return true;
}


bool Alg_time_map::set_tempo(double tempo, double start_beat, double end_beat)
{
    if (start_beat >= end_beat) return false;
    // algorithm: insert a beat event if necessary at start_beat
    // and at end_beat
    // delete intervening map elements
    // change the tempo
    insert_beat(beat_to_time(start_beat), start_beat);
    insert_beat(beat_to_time(end_beat), end_beat);
    long start_x = locate_beat(start_beat) + 1;
    long stop_x = locate_beat(end_beat);
    while (stop_x < beats.len) {
        beats[start_x] = beats[stop_x];
        start_x++;
        stop_x++;
    }
    beats.len = start_x; // truncate the map to new length
    return insert_tempo(tempo, start_beat);
}


void Alg_time_map::trim(double start, double end, bool units_are_seconds)
{
    // extract the time map from start to end and shift to time zero
    // start and end are time in seconds if units_are_seconds is true
    int i = 0; // index into beats
    int start_index; // index of first breakpoint after start
    int count = 1;
    double initial_beat = start;
    double final_beat = end;
    if (units_are_seconds) {
        initial_beat = time_to_beat(start);
        final_beat = time_to_beat(end);
    } else {
        start = beat_to_time(initial_beat);
        end = beat_to_time(final_beat);
    }
    while (i < length() && beats[i].time < start) i++;
    // now i is index into beats of the first breakpoint after start
    // beats[0] is (0,0) and remains that way
    // copy beats[start_index] to beats[1], etc.
    // skip any beats at or near (start,initial_beat), using count
    // to keep track of how many entries there are
    start_index = i;
    while (i < length() && beats[i].time < end) {
        if (beats[i].time - start > ALG_EPS &&
            beats[i].beat - initial_beat > ALG_EPS) {
            beats[i].time = beats[i].time - start;
            beats[i].beat = beats[i].beat - initial_beat;
            beats[i - start_index + 1] = beats[i];
            count = count + 1;
        } else {
            start_index = start_index + 1;
        }
        i = i + 1;
    }
    // set last tempo data
    // we last examined beats[i-1] and copied it to
    //   beats[i - start_index]. Next tempo should come
    //   from beats[i] and store in beats[i - start_index + 1]
    // case 1: there is at least one breakpoint beyond end
    //         => interpolate to put a breakpoint at end
    // case 2: no more breakpoints => set last tempo data
    if (i < length()) {
        // we know beats[i].time >= end, so case 1 applies
        beats[i - start_index + 1].time = end - start;
        beats[i - start_index + 1].beat = final_beat - initial_beat;
        count = count + 1;
    }
    // else we'll just use stored last tempo (if any)
    beats.len = count;
}


void Alg_time_map::cut(double start, double len, bool units_are_seconds)
{
    // remove portion of time map from start to start + len, 
    // shifting the tail left by len. start and len are in whatever
    // units the score is in. If you cut the time_map as well as cut 
    // the tracks of the sequence, then sequences will preserve the
    // association between tempo changes and events
    double end = start + len;
    double initial_beat = start;
    double final_beat = end;
    int i = 0;

    if (units_are_seconds) {
        initial_beat = time_to_beat(start);
        final_beat = time_to_beat(end);
    } else {
        start = beat_to_time(initial_beat);
        end = beat_to_time(final_beat);
        len = end - start;
    }
    double beat_len = final_beat - initial_beat;

    while (i < length() && beats[i].time < start - ALG_EPS) {
        i = i + 1;
    }

    // if no beats exist at or after start, just return; nothing to cut
    if (i == length()) return;

    // now i is index into beats of the first breakpoint on or 
    // after start, insert (start, initial_beat) in map
    if (i < length() && within(beats[i].time, start, ALG_EPS)) {
        // perterb time map slightly (within alg_eps) to place
        // break point exactly at the start time
        beats[i].time = start;
        beats[i].beat = initial_beat;
    } else {
        Alg_beat point(start, initial_beat);
        beats.insert(i, &point);
    }
    // now, we're correct up to beats[i] and beats[i] happens at start.
    // find first beat after end so we can start shifting from there
    i = i + 1;
    int start_index = i;
    while (i < length() && beats[i].time < end + ALG_EPS) i++;
    // now beats[i] is the next point to be included in beats
    // but from i onward, we must shift by (-len, -beat_len)
    while (i < length()) {
        Alg_beat &b = beats[i];
        b.time = b.time - len;
        b.beat = b.beat - beat_len;
        beats[start_index] = b;
        i = i + 1;
        start_index = start_index + 1;
    }
    beats.len = start_index;
}


void Alg_time_map::paste(double beat, Alg_track *tr)
{
    // insert a given time map at a given time and dur (in beats)
    Alg_time_map_ptr from_map = tr->get_time_map();
    // printf("time map paste\nfrom map\n");
    // from_map->show();
    // printf("to map\n");
    // show();
    Alg_beats &from = from_map->beats;
    double time = beat_to_time(beat);
    // Locate the point at which dur occurs
    double dur = tr->get_beat_dur();
    double tr_end_time = from_map->beat_to_time(dur);
    // add offset to make room for insert
    int i = locate_beat(beat);
    while (i < length()) {
        beats[i].beat += dur;
        beats[i].time += tr_end_time;
        i++;
    }
    // printf("after opening up\n");
    // show();
    // insert point at beginning and end of paste
    insert_beat(time, beat);
    // printf("after beginning point insert\n");
    // show();
    // insert_beat(time + tr_end_time, beat + dur);
    // printf("after ending point insert\n");
    // show();
    int j = from_map->locate_beat(dur);
    for (i = 0; i < j; i++) {
        insert_beat(from[i].time + time,  // shift by time
                    from[i].beat + beat); // and beat
    }
    // printf("after inserts\n");
    show();
}


void Alg_time_map::insert_time(double start, double len)
{
    // find time,beat pair that determines tempo at start
    // compute beat offset = (delta beat / delta time) * len
    // add len,beat offset to each following Alg_beat
    // show();
    int i = locate_time(start); // start <= beats[i].time
    if (beats[i].time == start) i++; // start < beats[i].time
    // case 1: between beats
    if (i > 0 && i < length()) {
        double beat_offset = len * (beats[i].beat - beats[i-1].beat) / 
                                   (beats[i].time - beats[i-1].time);
        while (i < length()) {
            beats[i].beat += beat_offset;
            beats[i].time += len;
            i++;
        }
    } // otherwise, last tempo is in effect; nothing to do
    // printf("time_map AFTER INSERT\n");
    // show();
}


void Alg_time_map::insert_beats(double start, double len)
{
    int i = locate_beat(start); // start <= beats[i].beat
    if (beats[i].beat == start) i++;
    if (i > 0 && i < length()) {
        double time_offset = len * (beats[i].time - beats[i-1].time) / 
                                   (beats[i].beat - beats[i-1].beat);
        while (i < length()) {
            beats[i].time += time_offset;
            beats[i].beat += len;
            i++;
        }
    } // otherwise, last tempo is in effect; nothing to do
    // printf("time_map AFTER INSERT\n");
    // show();
}


Alg_track::Alg_track(Alg_time_map *map, bool seconds)
{
    type = 't';
    time_map = NULL;
    units_are_seconds = seconds;
    set_time_map(map);
}


Alg_event_ptr Alg_track::copy_event(Alg_event_ptr event)
{
    Alg_event *new_event;
    if (event->is_note()) {
        new_event = new Alg_note((Alg_note_ptr) event);
    } else { // update
        new_event = new Alg_update((Alg_update_ptr) event);
    }
    return new_event;
}


Alg_track::Alg_track(Alg_track &track)
{
    type = 't';
    time_map = NULL;
    for (int i = 0; i < track.length(); i++) {
      append(copy_event(track.events[i]));
    }
    set_time_map(track.time_map);
    units_are_seconds = track.units_are_seconds;
}


Alg_track::Alg_track(Alg_event_list_ref event_list, Alg_time_map_ptr map,
                     bool units_are_seconds)
{
    type = 't';
    time_map = NULL;
    for (int i = 0; i < event_list.length(); i++) {
        append(copy_event(event_list[i]));
    }
    set_time_map(map);
    this->units_are_seconds = units_are_seconds;
}


void Alg_track::serialize(void **buffer, long *bytes)
{
    // first determine whether this is a seq or a track.
    // if it is a seq, then we will write the time map and a set of tracks
    // if it is a track, we just write the track data and not the time map
    //
    // The code will align doubles on ALIGN boundaries, and longs and
    // floats are aligned to multiples of 4 bytes.
    //
    // The format for a seq is:
    //   'ALGS' -- indicates that this is a sequence
    //   long length of all seq data in bytes starting with 'ALGS'
    //   long channel_offset_per_track
    //   long units_are_seconds
    //   time_map:
    //      double last_tempo
    //      long last_tempo_flag
    //      long len -- number of tempo changes
    //      for each tempo change (Alg_beat):
    //         double time
    //         double beat
    //   time_sigs:
    //      long len -- number of time_sigs
    //      long pad
    //      for each time signature:
    //         double beat
    //         double num
    //         double den
    //   tracks:
    //      long len -- number of tracks
    //      long pad
    //      for each track:
    //         'ALGT' -- indicates this is a track
    //         long length of all track data in bytes starting with 'ALGT'
    //         long units_are_seconds
    //         double beat_dur
    //         double real_dur
    //         long len -- number of events
    //         for each event:
    //            long selected
    //            long type
    //            long key
    //            long channel
    //            double time
    //            if this is a note:
    //               double pitch
    //               double dur
    //               double loud
    //               long len -- number of parameters
    //               for each parameter:
    //                  char attribute[] with zero pad to ALIGN
    //                  one of the following, depending on type:
    //                     double r
    //                     char s[] terminated by zero
    //                     long i
    //                     long l
    //                     char a[] terminated by zero
    //               zero pad to ALIGN
    //            else if this is an update
    //               (same representation as parameter above)
    //               zero pad to ALIGN
    //
    // The format for a track is given within the Seq format above
    assert(get_type() == 't');
    ser_write_buf.init_for_write();
    serialize_track();
    *buffer = ser_write_buf.to_heap(bytes); 
}


void Alg_seq::serialize(void **buffer, long *bytes)
{	
    assert(get_type() == 's');
    ser_write_buf.init_for_write();
    serialize_seq();
    *buffer = ser_write_buf.to_heap(bytes); 
}


void Serial_write_buffer::check_buffer(long needed)
{
    if (len < (ptr - buffer) + needed) { // do we need more space?
        long new_len = len * 2; // exponential growth is important
        // initially, length is zero, so bump new_len to a starting value
        if (new_len == 0) new_len = 1024;
         // make sure new_len is as big as needed
        if (needed > new_len) new_len = needed;
        char *new_buffer = new char[new_len]; // allocate space
        ptr = new_buffer + (ptr - buffer); // relocate ptr to new buffer
        if (len > 0) { // we had a buffer already
            memcpy(new_buffer, buffer, len); // copy from old buffer
            delete buffer; // free old buffer
        }
        buffer = new_buffer; // update buffer information
        len = new_len;
    }
}


void Alg_seq::serialize_seq()
{
    int i; // loop counters
    // we can easily compute how much buffer space we need until we
    // get to tracks, so expand at least that much
    long needed = 64 + 16 * time_map->beats.len + 24 * time_sig.length();
    ser_write_buf.check_buffer(needed);
    ser_write_buf.set_char('A');
    ser_write_buf.set_char('L');
    ser_write_buf.set_char('G');
    ser_write_buf.set_char('S');
    long length_offset = ser_write_buf.get_posn();
    ser_write_buf.set_int32(0); // leave room to come back and write length
    ser_write_buf.set_int32(channel_offset_per_track);
    ser_write_buf.set_int32(units_are_seconds);
    ser_write_buf.set_double(beat_dur);
    ser_write_buf.set_double(real_dur);
    ser_write_buf.set_double(time_map->last_tempo);
    ser_write_buf.set_int32(time_map->last_tempo_flag);
    ser_write_buf.set_int32(time_map->beats.len);
    for (i = 0; i < time_map->beats.len; i++) {
        ser_write_buf.set_double(time_map->beats[i].time);
        ser_write_buf.set_double(time_map->beats[i].beat);
    }
    ser_write_buf.set_int32(time_sig.length());
    ser_write_buf.pad();
    for (i = 0; i < time_sig.length(); i++) {
        ser_write_buf.set_double(time_sig[i].beat);
        ser_write_buf.set_double(time_sig[i].num);
        ser_write_buf.set_double(time_sig[i].den);
    }
    ser_write_buf.set_int32(tracks());
    ser_write_buf.pad(); 
    for (i = 0; i < tracks(); i++) {
        track(i)->serialize_track();
    }
    // do not include ALGS, include padding at end
    ser_write_buf.store_long(length_offset, ser_write_buf.get_posn() - length_offset);
}


void Alg_track::serialize_track()
{
    // to simplify the code, copy from parameter addresses to locals
    int j;
    ser_write_buf.check_buffer(32);
    ser_write_buf.set_char('A');
    ser_write_buf.set_char('L');
    ser_write_buf.set_char('G');
    ser_write_buf.set_char('T');
    long length_offset = ser_write_buf.get_posn(); // save location for track length
    ser_write_buf.set_int32(0); // room to write track length
    ser_write_buf.set_int32(units_are_seconds);
    ser_write_buf.set_double(beat_dur);
    ser_write_buf.set_double(real_dur);
    ser_write_buf.set_int32(len);
    for (j = 0; j < len; j++) {
        ser_write_buf.check_buffer(24);
        Alg_event *event = (*this)[j];
        ser_write_buf.set_int32(event->get_selected());
        ser_write_buf.set_int32(event->get_type());
        ser_write_buf.set_int32(event->get_identifier());
        ser_write_buf.set_int32(event->chan);
        ser_write_buf.set_double(event->time);
        if (event->is_note()) {
            ser_write_buf.check_buffer(20);
            Alg_note *note = (Alg_note *) event;
            ser_write_buf.set_float(note->pitch);
            ser_write_buf.set_float(note->loud);
            ser_write_buf.set_double(note->dur);
            long parm_num_offset = ser_write_buf.get_posn();
            long parm_num = 0;
            ser_write_buf.set_int32(0); // placeholder for no. parameters
            Alg_parameters_ptr parms = note->parameters;
            while (parms) {
                serialize_parameter(&(parms->parm));
                parms = parms->next;
                parm_num++;
            }
            ser_write_buf.store_long(parm_num_offset, parm_num);
        } else {
            assert(event->is_update());
            Alg_update *update = (Alg_update *) event;
            serialize_parameter(&(update->parameter));
        }
        ser_write_buf.check_buffer(7); // maximum padding possible
        ser_write_buf.pad();
    }
    // write length, not including ALGT, including padding at end
    ser_write_buf.store_long(length_offset, ser_write_buf.get_posn() - length_offset);
}


void Alg_track::serialize_parameter(Alg_parameter *parm)
{
    // add eight to account for name + zero end-of-string and the
    // possibility of adding 7 padding bytes
    long len = strlen(parm->attr_name()) + 8;
    ser_write_buf.check_buffer(len);
    ser_write_buf.set_string(parm->attr_name());
    ser_write_buf.pad();
    switch (parm->attr_type()) {
    case 'r':
        ser_write_buf.check_buffer(8);
        ser_write_buf.set_double(parm->r);
        break;
    case 's':
        ser_write_buf.check_buffer(strlen(parm->s) + 1);
        ser_write_buf.set_string(parm->s);
        break;
    case 'i':
        ser_write_buf.check_buffer(4);
        ser_write_buf.set_int32(parm->i);
        break;
    case 'l':
        ser_write_buf.check_buffer(4);
        ser_write_buf.set_int32(parm->l);
        break;
    case 'a':
        ser_write_buf.check_buffer(strlen(parm->a) + 1);
        ser_write_buf.set_string(parm->a);
        break;
    }
}



Alg_track *Alg_track::unserialize(void *buffer, long len)
{
    assert(len > 8);
    ser_read_buf.init_for_read(buffer, len);
    bool alg = ser_read_buf.get_char() == 'A' &&
               ser_read_buf.get_char() == 'L' &&
               ser_read_buf.get_char() == 'G';
    assert(alg);
    char c = ser_read_buf.get_char();
    if (c == 'S') {
        Alg_seq *seq = new Alg_seq;
        ser_read_buf.unget_chars(4); // undo get_char() of A,L,G,S
        seq->unserialize_seq();
        return seq;
    } else {
        assert(c == 'T');
        Alg_track *track = new Alg_track;
        ser_read_buf.unget_chars(4); // undo get_char() of A,L,G,T
        track->unserialize_track();
        return track;
    }
}


#pragma warning(disable: 4800) // long to bool performance warning

/* Note: this Alg_seq must have a default initialized Alg_time_map.
 * It will be filled in with data from the ser_read_buf buffer.
 */
void Alg_seq::unserialize_seq()
{
    ser_read_buf.check_input_buffer(48);
    bool algs = (ser_read_buf.get_char() == 'A') &&
                (ser_read_buf.get_char() == 'L') &&
                (ser_read_buf.get_char() == 'G') &&
                (ser_read_buf.get_char() == 'S');
    assert(algs);
    long len = ser_read_buf.get_int32();
    assert(ser_read_buf.get_len() >= len);
    channel_offset_per_track = ser_read_buf.get_int32();
    units_are_seconds = ser_read_buf.get_int32() != 0;
    beat_dur = ser_read_buf.get_double();
    real_dur = ser_read_buf.get_double();
    // no need to allocate an Alg_time_map since it's done during initialization
    time_map->last_tempo = ser_read_buf.get_double();
    time_map->last_tempo_flag = ser_read_buf.get_int32() != 0;
    long beats = ser_read_buf.get_int32();
    ser_read_buf.check_input_buffer(beats * 16 + 4);
    int i;
    for (i = 0; i < beats; i++) {
        double time = ser_read_buf.get_double();
        double beat = ser_read_buf.get_double();
        time_map->insert_beat(time, beat);
        // printf("time_map: %g, %g\n", time, beat);
    }
    long time_sig_len = ser_read_buf.get_int32();
    ser_read_buf.get_pad();
    ser_read_buf.check_input_buffer(time_sig_len * 24 + 8);
    for (i = 0; i < time_sig_len; i++) {
        double beat = ser_read_buf.get_double();
        double num = ser_read_buf.get_double();
        double den = ser_read_buf.get_double();
        time_sig.insert(beat, num, den);
    }
    long tracks_num = ser_read_buf.get_int32();
    ser_read_buf.get_pad();
    add_track(tracks_num - 1); // create tracks_num tracks
    for (i = 0; i < tracks_num; i++) {
        track(i)->unserialize_track();
    }
    // assume seq started at beginning of buffer. len measures
    // bytes after 'ALGS' header, so add 4 bytes and compare to
    // current buffer position -- they should agree
    assert(ser_read_buf.get_posn() == len + 4);
}


void Alg_track::unserialize_track()
{
    ser_read_buf.check_input_buffer(32);
    bool algt = (ser_read_buf.get_char() == 'A') &&
                (ser_read_buf.get_char() == 'L') &&
                (ser_read_buf.get_char() == 'G') &&
                (ser_read_buf.get_char() == 'T');
    assert(algt);
    long offset = ser_read_buf.get_posn(); // stored length does not include 'ALGT'
    long bytes = ser_read_buf.get_int32();
    assert(bytes <= ser_read_buf.get_len() - offset);
    units_are_seconds = (bool) ser_read_buf.get_int32();
    beat_dur = ser_read_buf.get_double();
    real_dur = ser_read_buf.get_double();
    int event_count = ser_read_buf.get_int32();
    for (int i = 0; i < event_count; i++) {
        ser_read_buf.check_input_buffer(24);
        long selected = ser_read_buf.get_int32();
        char type = (char) ser_read_buf.get_int32();
        long key = ser_read_buf.get_int32();
        long channel = ser_read_buf.get_int32();
        double time = ser_read_buf.get_double();
        if (type == 'n') {
            ser_read_buf.check_input_buffer(20);
            float pitch = ser_read_buf.get_float();
            float loud = ser_read_buf.get_float();
            double dur = ser_read_buf.get_double();
            Alg_note *note = 
                    create_note(time, channel, key, pitch, loud, dur);
            note->set_selected(selected != 0);
            long param_num = ser_read_buf.get_int32();
            int j;
            // this builds a list of parameters in the correct order
            // (although order shouldn't matter)
            Alg_parameters_ptr *list = &note->parameters;
            for (j = 0; j < param_num; j++) {
                *list = new Alg_parameters(NULL);
                unserialize_parameter(&((*list)->parm));
                list = &((*list)->next);
            }
            append(note);
        } else {
            assert(type == 'u');
            Alg_update *update = create_update(time, channel, key);
            update->set_selected(selected != 0);
            unserialize_parameter(&(update->parameter));
            append(update);
        }
        ser_read_buf.get_pad();
    }
    assert(offset + bytes == ser_read_buf.get_posn());
}


void Alg_track::unserialize_parameter(Alg_parameter_ptr parm_ptr)
{
    char *attr = ser_read_buf.get_string();
    parm_ptr->attr = symbol_table.insert_string(attr);
    switch (parm_ptr->attr_type()) {
    case 'r':
        ser_read_buf.check_input_buffer(8);
        parm_ptr->r = ser_read_buf.get_double();
        break;
    case 's':
        parm_ptr->s = heapify(ser_read_buf.get_string());
        break;
    case 'i':
        ser_read_buf.check_input_buffer(4);
        parm_ptr->i = ser_read_buf.get_int32();
        break;
    case 'l':
        ser_read_buf.check_input_buffer(4);
        parm_ptr->l = ser_read_buf.get_int32() != 0;
        break;
    case 'a':
        parm_ptr->a = symbol_table.insert_attribute(ser_read_buf.get_string());
        break;
    }
}

#pragma warning(default: 4800)

void Alg_track::set_time_map(Alg_time_map *map)
{
    if (time_map) time_map->dereference();
    if (map == NULL) {
        time_map = new Alg_time_map(); // new default map
        time_map->reference();
    } else {
        time_map = map;
        time_map->reference();
    }
}


void Alg_track::convert_to_beats()
// modify all times and durations in notes to beats
{
    if (units_are_seconds) {
        units_are_seconds = false;
        long i;

        for (i = 0; i < length(); i++) {
            Alg_event_ptr e = events[i];
            double beat = time_map->time_to_beat(e->time);
            if (e->is_note()) {
                Alg_note_ptr n = (Alg_note_ptr) e;
                n->dur = time_map->time_to_beat(n->time + n->dur) - beat;
            }
            e->time = beat;
        }
    }
}


void Alg_track::convert_to_seconds()
// modify all times and durations in notes to seconds
{
    if (!units_are_seconds) {
        last_note_off = time_map->beat_to_time(last_note_off);
        units_are_seconds = true;
        long i;
        for (i = 0; i < length(); i++) {
            Alg_event_ptr e = events[i];
            double time = time_map->beat_to_time(e->time);
            if (e->is_note()) {
                Alg_note_ptr n = (Alg_note_ptr) e;
                n->dur = time_map->beat_to_time(n->time + n->dur) - time;
            }
            e->time = time;
        }
    }
}


void Alg_track::set_dur(double duration)
{
    // set beat_dur and real_dur
    if (units_are_seconds) {
        set_real_dur(duration);
        set_beat_dur(time_map->time_to_beat(duration));
    } else {
        set_beat_dur(duration);
        set_real_dur(time_map->beat_to_time(duration));
    }
}


Alg_note *Alg_track::create_note(double time, int channel, int identifier, 
                           float pitch, float loudness, double duration)
{
    Alg_note *note = new Alg_note();
    note->time = time;
    note->chan = channel;
    note->set_identifier(identifier);
    note->pitch = pitch;
    note->loud = loudness;
    note->dur = duration;
    return note;
}


Alg_update *Alg_track::create_update(double time, int channel, int identifier)
{
    Alg_update *update = new Alg_update();
    update->time = time;
    update->chan = channel;
    update->set_identifier(identifier);
    return update;
}


Alg_track_ptr Alg_track::cut(double t, double len, bool all)
{
    // since we are translating notes in time, do not copy or use old timemap
    Alg_track_ptr track = new Alg_track();
    track->units_are_seconds = units_are_seconds;
    if (units_are_seconds) {
        track->set_real_dur(len);
        track->set_beat_dur(time_map->time_to_beat(t + len) -
                            time_map->time_to_beat(t));
    } else {
        track->set_beat_dur(len);
        track->set_real_dur(time_map->beat_to_time(t + len) -
                            time_map->beat_to_time(t));
    }
    int i;
    int new_len = 0;
    int change = 0;
    for (i = 0; i < length(); i++) {
        Alg_event_ptr event = events[i];
        if (event->overlap(t, len, all)) {
            event->time -= t;
            track->append(event);
            change = 1;
        } else { // if we're not cutting this event, move it to 
                 // eliminate the gaps in events left by cut events
            events[new_len] = event;
            // adjust times of events after t + len
            if (event->time > t + len - ALG_EPS) {
                event->time -= len;
                change = 1;
            }
            new_len++;
        }
    }
    // Alg_event_lists based on this track become invalid
    sequence_number += change;
    this->len = new_len; // adjust length since we removed events
    return track;
}


Alg_track_ptr Alg_track::copy(double t, double len, bool all)
{
    // since we are translating notes in time, do not copy or use old timemap
    Alg_track_ptr track = new Alg_track();
    track->units_are_seconds = units_are_seconds;
    if (units_are_seconds) {
         track->set_real_dur(len);
         track->set_beat_dur(time_map->time_to_beat(t + len) - 
                             time_map->time_to_beat(t));
    } else {
        track->set_beat_dur(len);
        track->set_real_dur(time_map->beat_to_time(t + len) -
                            time_map->beat_to_time(t));
    }
    int i;
    for (i = 0; i < length(); i++) {
        Alg_event_ptr event = events[i];
        if (event->overlap(t, len, all)) {
            Alg_event_ptr new_event = copy_event(event);
            new_event->time -= t;
            track->append(new_event);
        }
    }
    return track;
}


void Alg_track::paste(double t, Alg_event_list *seq)
{
    assert(get_type() == 't');
    // seq can be an Alg_event_list, an Alg_track, or an Alg_seq
    // if it is an Alg_event_list, units_are_seconds must match
    bool prev_units_are_seconds;
    if (seq->get_type() == 'e') {
        assert(seq->get_owner()->get_units_are_seconds() == units_are_seconds);
    } else { // make it match
        Alg_track_ptr tr = (Alg_track_ptr) seq;
        prev_units_are_seconds = tr->get_units_are_seconds();
        if (units_are_seconds) tr->convert_to_seconds();
        else tr->convert_to_beats();
    }
    double dur = (units_are_seconds ? seq->get_real_dur() : 
                                      seq->get_beat_dur());

    // Note: in the worst case, seq may contain notes
    // that start almost anytime up to it's duration,
    // so the simplest algorithm is simply a sequence
    // of inserts. If this turns out to be too slow,
    // we can do a merge sort in the case that seq
    // is an Alg_track (if it's an Alg_event_list, we
    // are not guaranteed that the events are in time
    // order, but currently, only a true seq is allowed)

    int i;
    for (i = 0; i < length(); i++) {
        if (events[i]->time > t - ALG_EPS) {
            events[i]->time += dur;
        }
    }
    for (i = 0; i < seq->length(); i++) {
        Alg_event *new_event = copy_event((*seq)[i]);
        new_event->time += t;
        insert(new_event);
    }
    // restore track units to what they were before
    if (seq->get_type() != 'e') {
        Alg_track_ptr tr = (Alg_track_ptr) seq;
        if (prev_units_are_seconds) tr->convert_to_seconds();
        else tr->convert_to_beats();
    }

}


void Alg_track::merge(double t, Alg_event_list_ptr seq)
{
    Alg_event_list_ref s = *seq;
    for (int i = 0; i < s.length(); i++) {
        Alg_event *new_event;
        if (s[i]->is_note()) {
            new_event = new Alg_note((Alg_note_ptr) s[i]);
        } else {
            new_event = new Alg_update((Alg_update_ptr) s[i]);
        }
        new_event->time += t;
        insert(new_event);
    }
}


void Alg_track::clear(double t, double len, bool all)
{
    int i;
    int move_to = 0;
    for (i = 0; i < length(); i++) {
        Alg_event_ptr event = events[i];
        if (event->overlap(t, len, all)) {
            delete events[i];
        } else { // if we're not clearing this event, move it to 
                 // eliminate the gaps in events left by cleared events
            events[move_to] = event;
            // adjust times of events after t + len. This test is based
            // on the one in Alg_event::overlap() for consistency.
            if (event->time > t + len - ALG_EPS && event->time > t)
                event->time -= len;
            move_to++;
        }
    }
    if (move_to != this->len) { // we cleared at least one note
        sequence_number++; // Alg_event_lists based on this track become invalid
    }
    this->len = move_to; // adjust length since we removed events
}


void Alg_track::silence(double t, double len, bool all)
{
    int i;
    int move_to = 0;
    for (i = 0; i < length(); i++) {
        Alg_event_ptr event = events[i];
        if (event->overlap(t, len, all)) {
            delete events[i];
        } else { // if we're not clearing this event, move it to 
                 // eliminate the gaps in events left by cleared events
        events[move_to] = event;
            move_to++;
        }
    }
    if (move_to != this->len) { // we cleared at least one note
        sequence_number++; // Alg_event_lists based on this track become invalid
    }
    this->len = move_to; // adjust length since we removed events
}


void Alg_track::insert_silence(double t, double len)
{
    int i;
    for (i = 0; i < length(); i++) {
        Alg_event_ptr event = events[i];
        if (event->time > t - ALG_EPS) event->time += len;
    }
}


Alg_event_list *Alg_track::find(double t, double len, bool all,
                         long channel_mask, long event_type_mask)
{
    int i;
    Alg_event_list *list = new Alg_event_list(this);
    if (units_are_seconds) { // t and len are seconds
        list->set_real_dur(len);
        list->set_beat_dur(get_time_map()->time_to_beat(t + len) - 
                           get_time_map()->time_to_beat(t));
    } else { // t and len are beats
        list->set_real_dur(get_time_map()->beat_to_time(t + len) -
                           get_time_map()->beat_to_time(t));
        list->set_beat_dur(len);
    }
    for (i = 0; i < length(); i++) {
        Alg_event_ptr event = events[i];
        if (event->overlap(t, len, all)) {
            if ((channel_mask == 0 || 
                 (event->chan < 32 && 
                  (channel_mask & (1 << event->chan)))) &&
                ((event_type_mask == 0 ||
                  (event_type_mask & (1 << event->get_type_code()))))) {
                list->append(event);
            }
        }
    }
    return list;
}


void Alg_time_sigs::expand()
{
    assert(maxlen >= len);
    maxlen = (maxlen + 5);   // extra growth for small sizes
    maxlen += (maxlen >> 2); // add 25%
    Alg_time_sig_ptr new_time_sigs = new Alg_time_sig[maxlen];
    // now do copy
    memcpy(new_time_sigs, time_sigs, len * sizeof(Alg_time_sig));
    if (time_sigs)
       delete[] time_sigs;
    time_sigs = new_time_sigs;
}


void Alg_time_sigs::insert(double beat, double num, double den)
{
    // find insertion point:
    for (int i = 0; i < len; i++) {
        if (within(time_sigs[i].beat, beat, ALG_EPS)) {
            // overwrite location i with new info
            time_sigs[i].beat = beat;
            time_sigs[i].num = num;
            time_sigs[i].den = den;
            return;
        } else if (time_sigs[i].beat > beat) {
            if ((i > 0 && // check if redundant with prev. time sig
                 time_sigs[i - 1].num == num &&
                 time_sigs[i - 1].den == den &&
                 within(fmod(beat - time_sigs[i - 1].beat,
                             4 * time_sigs[i-1].num / time_sigs[i-1].den),
                             0, ALG_EPS)) ||
                // check if redundant with implied initial 4/4 time sig:
                (i == 0 && num == 4 && den == 4 &&
                 within(fmod(beat, 4), 0, ALG_EPS))) {
                return; // redundant inserts are ignored here
            }
            // make room for new event
            if (maxlen <= len) expand();
            // insert new event at i
            memmove(&time_sigs[i + 1], &time_sigs[i], 
                    sizeof(Alg_time_sig) * (len - i));
            time_sigs[i].beat = beat;
            time_sigs[i].num = num;
            time_sigs[i].den = den;
            len++;
            return;
        }
    }
    // if we fall out of loop, then this goes at end
    if (maxlen <= len) expand();
    time_sigs[len].beat = beat;
    time_sigs[len].num = num;
    time_sigs[len].den = den;
    len++;
}


void Alg_time_sigs::show()
{
    printf("Alg_time_sig: ");
    for (int i = 0; i < len; i++) {
        printf("(%g: %g/%g) ", time_sigs[i].beat, time_sigs[i].num, time_sigs[i].den);
    }
    printf("\n");
}


int Alg_time_sigs::find_beat(double beat)
{
    // index where you would insert a new time signature at beat
    int i = 0;
    while (i < len && time_sigs[i].beat < beat - ALG_EPS) i++;
    return i;
}


void Alg_time_sigs::cut(double start, double end)
{
    // remove time_sig's from start to start+len -- these must be
    //   in beats (not seconds)
    // now rewrite time_sig[]: copy from i_in to i_out (more or less)
    int i_in = 0;
    int i_out = 0;
    // first, figure out where to begin cut region
    i_in = find_beat(start);
    i_out = i_in;
    // scan to end of cut region
    while (i_in < len && time_sigs[i_in].beat < end) {
        i_in = i_in + 1;
    }
    // change time_sig at start if necessary
    //    there's a time_sig that was skipped if i_in > i_out. 
    //    if that's true and the next time change is at end, we're
    //    ok because it will be copied, but if the next time change
    //    is after end, then maybe we should insert a time change
    //    corresponding to what's in effect at end. We can skip this
    //    insert if it corresponds to whatever is in effect at start
    if (i_in > i_out && i_in < len && 
        time_sigs[i_in].beat > end + ALG_EPS &&
        (i_out == 0 || time_sigs[i_out - 1].num != time_sigs[i_in - 1].num ||
         time_sigs[i_out - 1].den != time_sigs[i_in - 1].den)) {
        time_sigs[i_out] = time_sigs[i_in - 1];
        time_sigs[i_out].beat = start;
    }
    // scan from end to len(time_sig)
    while (i_in < length()) {
        Alg_time_sig &ts = time_sigs[i_in];
        ts.beat = ts.beat - (end - start);
        time_sigs[i_out] = ts;
        i_in = i_in + 1;
        i_out = i_out + 1;
    }
    len = i_out;
}


void Alg_time_sigs::trim(double start, double end)
{
    // remove time_sig's not in [start, start+end)
    // units must be in beats (not seconds)
    // copy from i_in to i_out as we scan time_sig array
    int i_in = 0;
    int i_out = 0;
    // first, skip time signatures up to start
    i_in = find_beat(start);
    // put time_sig at start if necessary
    // if 0 < i_in < len, then the time sig at i_in is either
    // at start or after start. 
    //     If after start, then insert time sig at i_in-1 at 0. 
    //     Otherwise, we'll pick up time sig at i_in below.
    // If 0 == i_in < len, then the time sig at i_in is either
    // at start or after start.
    //     If after start, then time sig at 0 is 4/4, but that's the
    //          default, so do nothing.
    //     Otherwise, we'll pick up time sig at i_in below.
    // If 0 < i_in == len, then insert time_sig at i_in-1 at start
    // If 0 == i_in == len, then 4/4 default applies and we're done.
    // 
    // So the conditions for inserting time_sig[in_i-1] at 0 are:
    //     (0 < i_in < len and time_sig[i] > start+ALG_EPS) OR
    //     (0 < i_in == len)
    // We can rewrite this to 
    //     (0 < i_in) && ((i_in < len && time_sig[i_in].beat > start + ALG_EPS) ||
    //                    (i_in == len)))
    //     
    if (0 < i_in && ((i_in < len && time_sigs[i_in].beat > start + ALG_EPS) ||
                     (i_in == len))) {
        time_sigs[0] = time_sigs[i_in - 1];
        time_sigs[0].beat = 0.0;
        i_out = 1;
    }
    // scan to end of cut region
    while (i_in < len && time_sigs[i_in].beat < end - ALG_EPS) {
        Alg_time_sig &ts = time_sigs[i_in];
        ts.beat = ts.beat - start;
        time_sigs[i_out] = ts;
        i_in++;
        i_out++;
    }
    len = i_out;
}


void Alg_time_sigs::paste(double start, Alg_seq *seq)
{
    // printf("time_sig::insert before paste\n");
    // show();
    Alg_time_sigs &from = seq->time_sig;
    // printf("time_sig::insert from\n");
    // from.show();
    // insert time signatures from seq into this time_sigs at start
    if (len == 0 && from.len == 0) {
        return; // default applies
    }
    int i = find_beat(start);
    // remember the time signature at the splice point
    double num_after_splice = 4;
    double den_after_splice = 4; // default
    // three cases: 
    //  1) time sig at splice is at i-1
    //     for this, we must have len>0 & i>0
    //     two sub-cases:
    //       A) i < len && time_sig[i].beat > start
    //       B) i == len
    //  2) time_sig at splice is at i
    //     for this, i < len && time_sig[i].beat ~= start
    //  3) time_sig at splice is default 4/4
    if (len > 0 && i > 0 &&
        ((i < len && time_sigs[i].beat > start + ALG_EPS) ||
         (i == len))) {
        num_after_splice = time_sigs[i-1].num;
        den_after_splice = time_sigs[i-1].den;
    } else if (i < len && time_sigs[i].beat <= start + ALG_EPS) {
        num_after_splice = time_sigs[i].num;
        den_after_splice = time_sigs[i].den;
    }
    // i is where insert will go, time_sig[i].beat > start
    // begin by adding duration to time_sig's at i and above
    // move time signatures forward by duration of seq
    double dur = seq->get_beat_dur();
    while (i < len) {
        time_sigs[i].beat += dur;
        i++;
    }
    //printf("time_sig::insert after making space\n");
    //show();
    // now insert initial time_signature at start. This may create
    // an extra measure if seq does not begin on a measure boundary
    insert(start, 4, 4); // in case seq uses default starting signature
    //printf("time_sig::insert after 4/4 at start\n");
    //show();
    // insert time signatures from seq offset by start
    for (i = 0; i < from.length(); i++) {
        insert(start + from[i].beat, from[i].num, from[i].den);
    }
    //printf("time_sig::insert after pasting in sigs\n");
    //show();
    // now insert time signature at end of splice
    insert(start + dur, num_after_splice, den_after_splice);
    //printf("time_sig::insert after sig at end of splice\n");
    //show();
}


void Alg_time_sigs::insert_beats(double beat, double len)
{
    int i;
    // find the time_sig entry in effect at t
    for (i = 0; i < len; i++) {
        if (time_sigs[i].beat < beat + ALG_EPS) {
            break;
        }
    }
    // now, increase beat times by len
    for (; i < len; i++) {
        time_sigs[i].beat += len;
    }
}


Alg_tracks::~Alg_tracks()
{
    reset();
}


void Alg_tracks::expand_to(int new_max)
{
    maxlen = new_max;
    Alg_track_ptr *new_tracks = new Alg_track_ptr[maxlen];
    // now do copy
    memcpy(new_tracks, tracks, len * sizeof(Alg_track_ptr));
    if (tracks) {
        delete[] tracks;
    }
    tracks = new_tracks;
}	


void Alg_tracks::expand()
{
    maxlen = (maxlen + 5);   // extra growth for small sizes
    maxlen += (maxlen >> 2); // add 25%
    expand_to(maxlen);
}


void Alg_tracks::append(Alg_track_ptr track)
{
    if (maxlen <= len) {
        expand();
    }
    tracks[len] = track;
    len++;
}


void Alg_tracks::add_track(int track_num, Alg_time_map_ptr time_map,
                           bool seconds)
    // Create a new track at index track_num.
    // If track already exists, this call does nothing.
    // If highest previous track is not at track_num-1, then
    // create tracks at len, len+1, ..., track_num.
{
    assert(track_num >= 0);
    if (track_num == maxlen) {
        // use eponential growth to insert tracks sequentially
        expand();
    } else if (track_num > maxlen) {
        // grow to exact size for random inserts
        expand_to(track_num + 1);
    }
    if (track_num < len) return; // don't add if already there
    while (len <= track_num) {
        tracks[len] = new Alg_track(time_map, seconds);
        //printf("allocated track at %d (%x, this %x) = %x\n", len, 
        //       &(tracks[len]), this, tracks[len]);
        len++;
    }
}


void Alg_tracks::reset()
{
    // all track events are incorporated into the seq,
    // so all we need to delete are the arrays of pointers
    for (int i = 0; i < len; i++) {
        // printf("deleting track at %d (%x, this %x) = %x\n", i, &(tracks[i]), 
        //       this, tracks[i]);
        delete tracks[i];
    }
    if (tracks) delete [] tracks;
    tracks = NULL;
    len = 0;
    maxlen = 0;               // Modified by Ning Hu Nov.19 2002
}


void Alg_iterator::expand_to(int new_max)
{
    maxlen = new_max;
    Alg_pending_event_ptr new_pending_events = new Alg_pending_event[maxlen];
    // now do copy
    memcpy(new_pending_events, pending_events, 
           len * sizeof(Alg_pending_event));
    if (pending_events) {
        delete[] pending_events;
    }
    pending_events = new_pending_events;
}	


void Alg_iterator::expand()
{
    maxlen = (maxlen + 5);   // extra growth for small sizes
    maxlen += (maxlen >> 2); // add 25%
    expand_to(maxlen);
}


Alg_iterator::~Alg_iterator()
{
    if (pending_events) {
        delete[] pending_events;
    }
}


/* in the heap, the children of N are (N+1)*2 and (N+1)*2-1, so
 * the parent of N is (N+1)/2-1. This would be easier if arrays
 * were 1-based instead of 0-based
 */
#define HEAP_PARENT(loc) ((((loc) + 1) / 2) - 1)
#define FIRST_CHILD(loc) (((loc) * 2) + 1)

void Alg_iterator::show()
{
    for (int i = 0; i < len; i++) {
        Alg_pending_event_ptr p = &(pending_events[i]);
        printf("    %d: %p[%d] on %d\n", i, p->events, p->index, p->note_on);
    }
}


bool Alg_iterator::earlier(int i, int j)
// see if event i is earlier than event j
{
    Alg_pending_event_ptr p_i = &(pending_events[i]);
    Alg_event_ptr e_i = (*(p_i->events))[p_i->index];
    double t_i = (p_i->note_on ? e_i->time : e_i->get_end_time());
    
    Alg_pending_event_ptr p_j = &(pending_events[j]);
    Alg_event_ptr e_j = (*(p_j->events))[p_j->index];
    double t_j = (p_j->note_on ? e_j->time : e_j->get_end_time());

    if (t_i < t_j) return true;
    // not sure if this case really exists or this is the best rule, but
    // we want to give precedence to note-off events
    else if (t_i == t_j && pending_events[j].note_on) return true;
    return false;
}


void Alg_iterator::insert(Alg_events_ptr events, long index, bool note_on)
{
    if (len == maxlen) expand();
    pending_events[len].events = events;
    pending_events[len].index = index;
    pending_events[len].note_on = note_on;
    int loc = len;
    int loc_parent = HEAP_PARENT(loc);
    len++;
    // sift up:
    while (loc > 0 &&
           earlier(loc, loc_parent)) {
        // swap loc with loc_parent
        Alg_pending_event temp = pending_events[loc];
        pending_events[loc] = pending_events[loc_parent];
        pending_events[loc_parent] = temp;
        loc = loc_parent;
        loc_parent = HEAP_PARENT(loc);
    }
    //    printf("After insert:"); show();
}

bool Alg_iterator::remove_next(Alg_events_ptr &events, long &index, 
                              bool &note_on)
{
    if (len == 0) return false; // empty!
    events = pending_events[0].events;
    index = pending_events[0].index;
    note_on = pending_events[0].note_on;
    len--;
    pending_events[0] = pending_events[len];
    // sift down
    long loc = 0;
    long loc_child = FIRST_CHILD(loc);
    while (loc_child < len) {
        if (loc_child + 1 < len) {
            if (earlier(loc_child + 1, loc_child)) {
                loc_child++;
            }
        }
        if (earlier(loc_child, loc)) {
            Alg_pending_event temp = pending_events[loc];
            pending_events[loc] = pending_events[loc_child];
            pending_events[loc_child] = temp;
            loc = loc_child;
            loc_child = FIRST_CHILD(loc);
        } else {
            loc_child = len;
        }
    }
    //    printf("After remove:"); show();
    return true;
}


Alg_seq::Alg_seq(const char *filename, bool smf)
{
    basic_initialization();
    ifstream inf(filename, smf ? ios::binary | ios::in : ios::in);
    if (inf.fail()) {
        error = alg_error_open;
        return;
    }
    if (smf) {
        error = alg_smf_read(inf, this);
    } else {
        error = alg_read(inf, this);
    }
    inf.close();
}


Alg_seq::Alg_seq(istream &file, bool smf)
{
    basic_initialization();
    if (smf) {
        error = alg_smf_read(file, this);
    } else {
        error = alg_read(file, this);
    }
}

void Alg_seq::seq_from_track(Alg_track_ref tr)
{
    type = 's';
    // copy everything
    set_beat_dur(tr.get_beat_dur());
    set_real_dur(tr.get_real_dur());
    // copy time_map
    set_time_map(new Alg_time_map(tr.get_time_map()));
    units_are_seconds = tr.get_units_are_seconds();

    if (tr.get_type() == 's') {
        Alg_seq_ref s = *(tr.to_alg_seq());
        channel_offset_per_track = s.channel_offset_per_track;
        add_track(s.tracks() - 1);
        // copy each track
        for (int i = 0; i < tracks(); i++) {
            Alg_track_ref from_track = *(s.track(i));
            Alg_track_ref to_track = *(track(i));
            to_track.set_beat_dur(from_track.get_beat_dur());
            to_track.set_real_dur(from_track.get_real_dur());
            if (from_track.get_units_are_seconds()) 
                to_track.convert_to_seconds();
            for (int j = 0; j < from_track.length(); j++) {
                Alg_event_ptr event = copy_event(from_track[j]);
                to_track.append(event);
            }
        }
    } else if (tr.get_type() == 't') {
        add_track(0);
        channel_offset_per_track = 0;
        Alg_track_ptr to_track = track(0);
        to_track->set_beat_dur(tr.get_beat_dur());
        to_track->set_real_dur(tr.get_real_dur());
        for (int j = 0; j < tr.length(); j++) {
            Alg_event_ptr event = copy_event(tr[j]);
            to_track->append(event);
        }
    } else {
        assert(false); // expected track or sequence
    }
}


int Alg_seq::tracks()
{
    return track_list.length();
}


Alg_track_ptr Alg_seq::track(int i)
{
    assert(0 <= i && i < track_list.length());
    return &(track_list[i]);
}

#pragma warning(disable: 4715) // ok not to return a value here

Alg_event_ptr &Alg_seq::operator[](int i) 
{
    int ntracks = track_list.length();
    int tr = 0;
    while (tr < ntracks) {
        Alg_track *a_track = track(tr);
        if (a_track && i < a_track->length()) {
            return (*a_track)[i];
        } else if (a_track) {
            i -= a_track->length();
        }
        tr++;
    }
    assert(false); // out of bounds
}
#pragma warning(default: 4715)


void Alg_seq::convert_to_beats()
{
    if (!units_are_seconds) return;
    for (int i = 0; i < tracks(); i++) {
        track(i)->convert_to_beats();
    }
    // note that the Alg_seq inherits units_are_seconds from an
    // empty track. Each track also has a (redundant) field called
    // units are seconds. These should always be consistent.
    units_are_seconds = false;
}


void Alg_seq::convert_to_seconds()
{
    if (units_are_seconds) return;
    //printf("convert_to_seconds, tracks %d\n", tracks());
    //printf("last_tempo of seq: %g on map %x\n", 
    //       get_time_map()->last_tempo, get_time_map());
    for (int i = 0; i < tracks(); i++) {
        //printf("last_tempo of track %d: %g on %x\n", i,
        //       track(i)->get_time_map()->last_tempo, 
        //       track(i)->get_time_map());
        track(i)->convert_to_seconds();
    }
    // update our copy of last_note_off (which may or may not be valid)
    last_note_off = time_map->beat_to_time(last_note_off);
    // note that the Alg_seq inherits units_are_seconds from an
    // empty track. Each track also has a (redundant) field called
    // units are seconds. These should always be consistent.
    units_are_seconds = true;
}


Alg_track_ptr Alg_seq::cut_from_track(int track_num, double start, 
                                      double dur, bool all)
{
    assert(track_num >= 0 && track_num < tracks());
    Alg_track_ptr tr = track(track_num);
    return tr->cut(start, dur, all);
}


void Alg_seq::copy_time_sigs_to(Alg_seq *dest)
{
    // copy time signatures
    for (int i = 0; i < time_sig.length(); i++) {
        dest->time_sig.insert(time_sig[i].beat, time_sig[i].num, 
                              time_sig[i].den);
    }
}


void Alg_seq::set_time_map(Alg_time_map *map)
{
    Alg_track::set_time_map(map);
    for (int i = 0; i < tracks(); i++) {
        track(i)->set_time_map(map);
    }
}


Alg_seq_ptr Alg_seq::cut(double start, double len, bool all)
    // return sequence from start to start+len and modify this
    // sequence by removing that time-span
{
    // fix parameters to fall within existing sequence
    if (start > get_dur()) return NULL; // nothing to cut
    if (start < 0) start = 0; // can't start before sequence starts
    if (start + len > get_dur()) // can't cut after end:
        len = get_dur() - start; 

    Alg_seq_ptr result = new Alg_seq();
    Alg_time_map_ptr map = new Alg_time_map(get_time_map());
    result->set_time_map(map);
    copy_time_sigs_to(result);
    result->units_are_seconds = units_are_seconds;
    result->track_list.reset();

    for (int i = 0; i < tracks(); i++) {
        Alg_track_ptr cut_track = cut_from_track(i, start, len, all);
        result->track_list.append(cut_track);
        // initially, result->last_note_off is zero. We want to know the
        // maximum over all cut_tracks, so compute that here:
        result->last_note_off = MAX(result->last_note_off, 
                                    cut_track->last_note_off);
        // since we're moving to a new sequence, change the track's time_map
        result->track_list[i].set_time_map(map);
    }

    // put units in beats to match time_sig's. Note that we need
    // two different end times. For result, we want the time of the
    // last note off, but for cutting out the time signatures in this,
    // we use len.
    double ts_start = start;
    double ts_end = start + len;
    double ts_last_note_off = start + result->last_note_off;
    if (units_are_seconds) {
        ts_start = time_map->time_to_beat(ts_start);
        ts_end = time_map->time_to_beat(ts_end);
        ts_last_note_off = time_map->time_to_beat(ts_last_note_off);
    }
    // result is shifted from start to 0 and has length len, but
    // time_sig and time_map are copies from this. Adjust time_sig,
    // time_map, and duration fields in result. The time_sig and 
    // time_map data is retained out to last_note_off so that we have
    // information for the entire duration of all the notes, even though
    // this might extend beyond the duration of the track. (Warning:
    // no info is retained for notes with negative times.)
    result->time_sig.trim(ts_start, ts_last_note_off);
    result->time_map->trim(start, start + result->last_note_off, 
                           result->units_are_seconds);
    // even though there might be notes sticking out beyond len, the
    // track duration is len, not last_note_off. (Warning: if all is
    // true, there may also be notes at negative offsets. These times
    // cannot be mapped between beat and time representations, so there
    // may be subtle bugs or unexpected behaviors in that case.)
    result->set_dur(len);

    // we sliced out a portion of each track, so now we need to
    // slice out the corresponding sections of time_sig and time_map
    // as well as to adjust the duration.
    time_sig.cut(ts_start, ts_end);
    time_map->cut(start, len, units_are_seconds);
    set_dur(get_dur() - len);

    return result;
}


void Alg_seq::insert_silence_in_track(int track_num, double t, double len)
{
    Alg_track_ptr tr = track(track_num);
    tr->insert_silence(t, len);
}


void Alg_seq::insert_silence(double t, double len)
{
    for (int i = 0; i < tracks(); i++) {
        insert_silence_in_track(i, t, len);
    }
    double t_beats = t;
    double len_beats = len;
    // insert into time_sig array; use time_sig_paste,
    // which requires us to build a simple time_sig array
    if (units_are_seconds) {
        time_map->insert_time(t, len);
        t_beats = time_map->time_to_beat(t);
        len_beats = time_map->time_to_beat(t + len) - t_beats;
    } else {
        time_map->insert_beats(t_beats, len_beats);
    }
    if (time_sig.length() > 0) {
        time_sig.insert_beats(t_beats, len_beats);
    }
}


Alg_track_ptr Alg_seq::copy_track(int track_num, double t, double len, bool all)
{
    return track_list[track_num].copy(t, len, all);
}


Alg_seq *Alg_seq::copy(double start, double len, bool all)
{
    // fix parameters to fall within existing sequence
    if (start > get_dur()) return NULL; // nothing to copy
    if (start < 0) start = 0; // can't copy before sequence starts
    if (start + len > get_dur()) // can't copy after end:
        len = get_dur() - start; 

    // return (new) sequence from start to start + len
    Alg_seq_ptr result = new Alg_seq();
    Alg_time_map_ptr map = new Alg_time_map(get_time_map());
    result->set_time_map(map);
    copy_time_sigs_to(result);
    result->units_are_seconds = units_are_seconds;
    result->track_list.reset();

    for (int i = 0; i < tracks(); i++) {
        Alg_track_ptr copy = copy_track(i, start, len, all);
        result->track_list.append(copy);
        result->last_note_off = MAX(result->last_note_off, 
                                    copy->last_note_off);
        // since we're copying to a new seq, change the track's time_map
        result->track_list[i].set_time_map(map);
    }

    // put units in beats to match time_sig's. Note that we need
    // two different end times. For result, we want the time of the
    // last note off, but for cutting out the time signatures in this,
    // we use len.
    double ts_start = start;
    double ts_end = start + len;
    double ts_last_note_off = start + result->last_note_off;
    if (units_are_seconds) {
        ts_start = time_map->time_to_beat(ts_start);
        ts_end = time_map->time_to_beat(ts_end);
        ts_last_note_off = time_map->time_to_beat(ts_last_note_off);
    }

    result->time_sig.trim(ts_start, ts_last_note_off);
    result->time_map->trim(start, start + result->last_note_off,
                           units_are_seconds);
    result->set_dur(len);
    return result;
}


void Alg_seq::paste(double start, Alg_seq *seq)
{
    // insert seq at time; open up space for it
    // to manipulate time map, we need units as beats
    // save original form so we can convert back if necessary
    bool units_should_be_seconds = units_are_seconds;
    bool seq_units_should_be_seconds = seq->get_units_are_seconds();
    if (units_are_seconds) {
        start = time_map->time_to_beat(start);
        convert_to_beats();
    }
    seq->convert_to_beats();

    // do a paste on each track
    int i;
    for (i = 0; i < seq->tracks(); i++) {
        if (i >= tracks()) {
            add_track(i);
        }
        track(i)->paste(start, seq->track(i));
    }
    // make sure all tracks were opened up for an insert, even if
    // there is nothing to insert
    while (i < tracks()) {
        track(i)->insert_silence(start, seq->get_dur());
        i++;
    }
    // paste in tempo track
    time_map->paste(start, seq);
    // paste in time signatures
    time_sig.paste(start, seq);
    set_dur(get_beat_dur() + seq->get_dur());
    assert(!seq->units_are_seconds && !units_are_seconds);
    if (units_should_be_seconds) {
        convert_to_seconds();
    }
    if (seq_units_should_be_seconds) {
        seq->convert_to_seconds();
    }
}


void Alg_seq::merge(double t, Alg_event_list_ptr seq)
{
    // seq must be an Alg_seq:
    assert(seq->get_type() == 's');
    Alg_seq_ptr s = (Alg_seq_ptr) seq;
    for (int i = 0; i < s->tracks(); i++) {
        if (tracks() <= i) add_track(i);
        track(i)->merge(t, s->track(i));
    }
}


void Alg_seq::silence_track(int track_num, double start, double len, bool all)
{
    // remove events in [time, time + len) and close gap
    Alg_track_ptr tr = track(track_num);
    tr->silence(start, len, all);
}


void Alg_seq::silence(double t, double len, bool all)
{
    for (int i = 0; i < tracks(); i++) {
        silence_track(i, t, len, all);
    }
}


void Alg_seq::clear_track(int track_num, double start, double len, bool all)
{
    // remove events in [time, time + len) and close gap
    Alg_track_ptr tr = track(track_num);
    tr->clear(start, len, all);
}


void Alg_seq::clear(double start, double len, bool all)
{
    // Fix parameters to fall within existing sequence
    if (start > get_dur()) return; // nothing to cut
    if (start < 0) start = 0; // can't start before sequence starts
    if (start + len > get_dur()) // can't cut after end:
        len = get_dur() - start;

    for (int i = 0; i < tracks(); i++)
        clear_track(i, start, len, all);

    // Put units in beats to match time_sig's.
    double ts_start = start;
    double ts_end = start + len;
    if (units_are_seconds) {
        ts_start = time_map->time_to_beat(ts_start);
        ts_end = time_map->time_to_beat(ts_end);
    }

    // we sliced out a portion of each track, so now we need to
    // slice out the corresponding sections of time_sig and time_map
    // as well as to adjust the duration.
    time_sig.cut(ts_start, ts_end);
    time_map->cut(start, len, units_are_seconds);
    set_dur(get_dur() - len);
}


Alg_event_list_ptr Alg_seq::find_in_track(int track_num, double t, double len,
                                          bool all, long channel_mask, 
                                          long event_type_mask)
{
    return track(track_num)->find(t, len, all, channel_mask, event_type_mask);
}


Alg_seq::~Alg_seq()
{
    int i, j;
    // Tracks does not delete Alg_events elements
    for (j = 0; j < track_list.length(); j++) {
        Alg_track &notes = track_list[j];
        // Alg_events does not delete notes 
        for (i = 0; i < notes.length(); i++) {
            Alg_event_ptr event = notes[i];
            delete event;
        }
    }
}


long Alg_seq::seek_time(double time, int track_num)
// find index of first score event after time
{
    long i;
    Alg_events &notes = track_list[track_num];
    for (i = 0; i < notes.length(); i++) {
        if (notes[i]->time > time) {
            break;
        }
    }
    return i;
}


bool Alg_seq::insert_beat(double time, double beat)
// insert a time,beat pair
// return true or false (false indicates an error, no update)
// it is an error to imply a negative tempo or to insert at
// a negative time
{
    if (time < 0 || beat < 0) return false;
    if (time == 0.0 && beat > 0)
        time = 0.000001; // avoid infinite tempo, offset time by 1us
    if (time == 0.0 && beat == 0.0)
        return true; // (0,0) is already in the map!
    convert_to_beats(); // beats are invariant when changing tempo
    time_map->insert_beat(time, beat);
    return true;
}


bool Alg_seq::insert_tempo(double bpm, double beat)
{
    double bps = bpm / 60.0; // convert to beats per second
    // change the tempo at the given beat until the next beat event
    if (beat < 0) return false;
    convert_to_beats(); // beats are invariant when changing tempo
    double time = time_map->beat_to_time(beat);
    long i = time_map->locate_time(time);
    if (i >= time_map->beats.len || !within(time_map->beats[i].time, time, 0.000001)) {
        insert_beat(time, beat);
    }
    // now i is index of beat where tempo will change
    if (i == time_map->beats.len - 1) {
        time_map->last_tempo = bps;
        time_map->last_tempo_flag = true;
    } else { // adjust all future beats
        // compute the difference in beats
        double diff = time_map->beats[i + 1].beat - time_map->beats[i].beat;
        // convert beat difference to seconds at new tempo
        diff = diff / bps;
        // figure out old time difference:
        double old_diff = time_map->beats[i + 1].time - time;
        // compute difference too
        diff = diff - old_diff;
        // apply new_diff to score and beats
        while (i < time_map->beats.len) {
            time_map->beats[i].time = time_map->beats[i].time + diff;
            i++;
        }
    }
    return true;
}


void Alg_seq::add_event(Alg_event_ptr event, int track_num)
    // add_event puts an event in a given track (track_num). 
    // The track must exist. The time and duration of the
    // event are interpreted according to whether the Alg_seq 
    // is currently in beats or seconds (see convert_to_beats())
{
    track_list[track_num].insert(event);
/*
    if (event->is_note()) {
        Alg_note_ptr n = (Alg_note_ptr) event;
        trace("note %d at %g for %g\n", n->get_identifier(), n->time, n->dur);
    }
 */
}


bool Alg_seq::set_tempo(double bpm, double start_beat, double end_beat)
// set tempo from start_beat to end_beat
{
    // this is an optimization, the test is repeated in Alg_time_seq::set_tempo()
    if (start_beat >= end_beat) return false;
    bool units_should_be_seconds = units_are_seconds;
    convert_to_beats();
    bool result = time_map->set_tempo(bpm, start_beat, end_beat);
    if (units_should_be_seconds) convert_to_seconds();
    return result;
}


void Alg_seq::set_time_sig(double beat, double num, double den)
{
    time_sig.insert(beat, num, den);
}


void Alg_seq::beat_to_measure(double beat, long *measure, double *m_beat,
                          double *num, double *den)
{
    // return [measure, beat, num, den]
    double m = 0; // measure number
    double bpm;
    int tsx;
    bpm = 4;
    // assume 4/4 if no time signature
    double prev_beat = 0;
    double prev_num = 4;
    double prev_den = 4;

    if (beat < 0) beat = 0; // negative measures treated as zero

    for (tsx = 0; tsx < time_sig.length(); tsx++) {
        if (time_sig[tsx].beat <= beat) {
            // round m up to an integer (but allow for a small
            // numerical inaccuracy)
            m = m + (long) (0.99 + (time_sig[tsx].beat - prev_beat) / bpm);
            bpm = time_sig[tsx].num * 4 / time_sig[tsx].den;
            prev_beat = time_sig[tsx].beat;
            prev_num = time_sig[tsx].num;
            prev_den = time_sig[tsx].den;
        } else {
            m = m + (beat - prev_beat) / bpm;
            *measure = (long) m;
            *m_beat = (m - *measure) * bpm;
            *num = prev_num;
            *den = prev_den;
            return;
        }
    }
    // if we didn't return yet, compute after last time signature
    Alg_time_sig initial(0, 4, 4);
    Alg_time_sig &prev = initial;
    if (tsx > 0) { // use last time signature
        prev = time_sig[time_sig.length() - 1];
    }
    bpm = prev.num * 4 / prev.den;
    m = m + (beat - prev.beat) / bpm;
    *measure = (long) m;
    *m_beat = (m - *measure) * bpm;
    *num = prev.num;
    *den = prev.den;
}

/*
void Alg_seq::set_events(Alg_event_ptr *events, long len, long max)
{
    convert_to_seconds(); // because notes are in seconds
    notes.set_events(events, len, max);
}
*/


void Alg_iterator::begin(bool note_off_flag)
{
    // keep an array of indexes into tracks
    printf("new pending\n");
    int i;
    for (i = 0; i < seq->track_list.length(); i++) {
        if (seq->track_list[i].length() > 0) {
            insert(&(seq->track_list[i]), 0, true);
        }
    }    
}


Alg_event_ptr Alg_iterator::next(bool *note_on)
    // return the next event in time from any track
{
    Alg_events_ptr events_ptr;
    long index;
    bool on;
    if (!remove_next(events_ptr, index, on)) {
        return NULL;
    }
    if (note_on) *note_on = on;
    Alg_event_ptr event = (*events_ptr)[index];
    if (on) {
        if (note_off_flag && event->is_note()) {
            // this was a note-on, so insert pending note-off
            insert(events_ptr, index, false);
        }
        // for both notes and updates, insert next event (at index + 1)
        index++;
        if (index < events_ptr->length()) {
            insert(events_ptr, index, true);
        }
    }
    return event;
}


void Alg_iterator::end()
{
}


void Alg_seq::merge_tracks()
{
    long sum = 0;
    long i;
    for (i = 0; i < track_list.length(); i++) {
        sum = sum + track(i)->length();
    }
    // preallocate array for efficiency:
    Alg_event_ptr *notes = new Alg_event_ptr[sum];
    Alg_iterator iterator(this, false);
    iterator.begin();
    long notes_index = 0;

    Alg_event_ptr event;
    while (event = iterator.next()) {
        notes[notes_index++] = event;
    }
    track_list.reset(); // don't need them any more
    add_track(0);
    track(0)->set_events(notes, sum, sum);
    iterator.end();
}


// sr_letter_to_type = {"i": 'Integer', "r": 'Real', "s": 'String',
//                     "l": 'Logical', "a": 'Symbol'}


