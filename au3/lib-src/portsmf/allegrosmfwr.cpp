// allegrosmfwr.cpp -- Allegro Standard Midi File Write

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include <string>
#include <iostream>
#include <fstream>
using namespace std;
#include "allegro.h"

// event_queue is a list element that keeps track of pending
// things to write to a track, including note-ons, note-offs,
// updates, tempo changes, and time signatures
//
class event_queue{
public:
  char type;//'n' for note, 'o' for off, 's' for time signature,
            // 'c' for tempo changes
  double time;
  long index; //of the event in mSeq->notes
  class event_queue *next;
  event_queue(char t, double when, long x, class event_queue *n) {
        type = t; time = when; index = x; next = n; }
};


class Alg_smf_write {
public:
    Alg_smf_write(Alg_seq_ptr seq);
    ~Alg_smf_write();
    long channels_per_track; // used to encode track number into chan field
    // chan is actual_channel + channels_per_track * track_number
    // default is 100, set this to 0 to merge all tracks to 16 channels

    void write(ostream &file /* , midiFileFormat = 1 */);

private:
    long previous_divs; // time in ticks of most recently written event

    void write_track(int i);
    void write_tempo(int divs, int tempo);
    void write_tempo_change(int i);
    void write_time_signature(int i);
    void write_note(Alg_note_ptr note, bool on);
    void write_update(Alg_update_ptr update);
    void write_text(Alg_update_ptr update, char type);
    void write_binary(int type_byte, const char *msg);
    void write_midi_channel_prefix(Alg_update_ptr update);
    void write_smpteoffset(Alg_update_ptr update, char *s);
    void write_data(int data);
    int to_midi_channel(int channel);
    int to_track(int channel);

    ostream *out_file;

    Alg_seq_ptr seq;

    int num_tracks; // number of tracks not counting tempo track
    int division; // divisions per quarter note, default = 120
    int initial_tempo;

    int timesig_num; // numerator of time signature
    int timesig_den; // denominator of time signature
    double timesig_when; // time of time signature

    int keysig;          // number of sharps (+) or flats (-), -99 for undefined
    char keysig_mode; // 'M' or 'm' for major/minor
    double keysig_when;    // time of key signature

    void write_delta(double event_time);
    void write_varinum(int num);
    void write_16bit(int num);
    void write_24bit(int num);
    void write_32bit(int num);
};

#define ROUND(x) (int) ((x)+0.5)

Alg_smf_write::Alg_smf_write(Alg_seq_ptr a_seq)
{
    out_file = NULL;

    // at 100bpm (a nominal tempo value), we would like a division
    // to represent 1ms of time. So
    // d ticks/beat * 100 beats/min = 60,000 ms/min * 1 tick/ms
    // solving for d, d = 600
    division = 600;         // divisions per quarter note
    timesig_num = timesig_den = 0; // initially undefined
    keysig = -99;
    keysig_mode = 0;
    initial_tempo = 500000;

    seq = a_seq;

    previous_divs = 0; // used to compute deltas for midifile
}


Alg_smf_write::~Alg_smf_write()
{
}


// sorting is quite subtle due to rounding
// For example, suppose times from a MIDI file are exact, but in
// decimal round to TW0.4167 Q0.3333. Since the time in whole notes
// rounded up, this note will start late. Even though the duration
// rounded down, the amount is 1/4 as much because units are quarter
// notes. Therefore, the total roundup is 0.0001 beats. This is 
// enough to cause the note to sort later in the queue, perhaps
// coming after a new note-on on the same pitch, and resulting in 
// a turning on-off, on-off into on, on, off, off if data is moved
// to Allegro (ascii) format with rounding and then back to SMF.
//
// The solution here is to consider things that round to the same
// tick to be simultaneous. Then, be sure to deal with note-offs
// before note-ons. We're going to do that by using event_queue
// times that are rounded to the nearest tick time. Except note-offs
// are going to go in with times that are 1/4 tick earlier so they
// get scheduled first, but still end up on the same tick.
//
event_queue* push(event_queue *queue, event_queue *event)
{
    // printf("push: %.6g, %c, %d\n", event->time, event->type, event->index);
    if (queue == NULL) {
        event->next = NULL;
        return event;
    }
   
    event_queue *marker1 = NULL;
    event_queue *marker2 = queue;
    while (marker2 != NULL && marker2->time <= event->time) {
        marker1 = marker2;
        marker2 = marker2->next;
    }
    event->next = marker2;
    if (marker1 != NULL) {
        marker1->next=event;
        return queue;
    } else return event;
}


void print_queue(event_queue *q)
{
    printf("Printing queue. . .\n");
    event_queue *q2=q;
    while (q2) {
        printf("%c at %f ;", q2->type, q2->time);
        q2 = q2->next;
    }
    printf("\nDone printing.\n");
}


void Alg_smf_write::write_note(Alg_note_ptr note, bool on)
{
    double event_time = (on ? note->time : note->time + note->dur);
    write_delta(event_time);

    //printf("deltaDivisions: %d, beats elapsed: %g, on? %c\n", deltaDivisions, note->time, on);

    char chan = char(note->chan & 15);
    int pitch = int(note->pitch + 0.5);
    if (pitch < 0) {
          pitch = pitch % 12;
    } else if (pitch > 127) {
        pitch = (pitch % 12) + 120; // put pitch in 10th octave
        if (pitch > 127) pitch -= 12; // or 9th octave
    }
    out_file->put(0x90 + chan);
    out_file->put(pitch);
    if (on) {
        int vel = (int) note->loud;
        if (vel <= 0) vel = 1;
        write_data(vel);
    } else out_file->put(0); // note-off indicated by velocty zero
}


void Alg_smf_write::write_midi_channel_prefix(Alg_update_ptr update)
{
   if (update->chan >= 0) { // write MIDI Channel Prefix
        write_delta(update->time);
        out_file->put('\xFF'); // Meta Event
        out_file->put('\x20'); // Type code for MIDI Channel Prefix
        out_file->put(1); // length
        out_file->put(to_midi_channel(update->chan));
        // one thing odd about the Std MIDI File spec is that once
        // you turn on MIDI Channel Prefix, there seems to be no
        // way to cancel it unless a non-Meta event shows up. We
        // don't do any analysis to avoid assigning channels to
        // meta events.
    }
}


void Alg_smf_write::write_text(Alg_update_ptr update, char type)
{
    write_midi_channel_prefix(update);
    write_delta(update->time);
    out_file->put('\xFF');
    out_file->put(type);
    out_file->put((char) strlen(update->parameter.s));
    *out_file << update->parameter.s;
}


void Alg_smf_write::write_smpteoffset(Alg_update_ptr update, char *s)
{
    write_midi_channel_prefix(update);
    write_delta(update->time);
    out_file->put('\xFF'); // meta event
    out_file->put('\x54'); // smpte offset type code
    out_file->put(5); // length
    for (int i = 0; i < 5; i++) *out_file << s[i];
}


// write_data - limit data to the range of [0...127] and write it
void Alg_smf_write::write_data(int data)
{
    if (data < 0) data = 0;
    else if (data > 0x7F) data = 0x7F;

    out_file->put(data);
}


int Alg_smf_write::to_midi_channel(int channel)
{
    // allegro track number is stored as multiple of 100
    // also mask off all but 4 channel bits just in case
    if (channels_per_track > 0) channel %= channels_per_track;
    return channel & 0xF;
}


int Alg_smf_write::to_track(int channel)
{
    if (channel == -1) return 0;
    return channel / channels_per_track;
}


static char hex_to_nibble(char c)
{
    if (isalpha(c)) {
        return 10 + (toupper(c) - 'A');
    } else {
        return c - '0';
    }
}


static char hex_to_char(const char *s)
{
    return (hex_to_nibble(s[0]) << 4) + hex_to_nibble(s[1]);
}


void Alg_smf_write::write_binary(int type_byte, const char *msg)
{
    int len = strlen(msg) / 2;
    out_file->put(type_byte);
    write_varinum(len);
    for (int i = 0; i < len; i++) {
        out_file->put(hex_to_char(msg));
        msg += 2;
    }
}


void Alg_smf_write::write_update(Alg_update_ptr update)
{
    const char *name = update->parameter.attr_name();

    /****Non-Meta Events****/
    if (!strcmp(name, "pressurer")) {
        write_delta(update->time);
        if (update->get_identifier() < 0) { // channel pressure message
            out_file->put(0xD0 + to_midi_channel(update->chan));
            write_data((int)(update->parameter.r * 127));
        } else { // just 1 key -- poly pressure
            out_file->put(0xA0 + to_midi_channel(update->chan));
            write_data(update->get_identifier());
            write_data((int)(update->parameter.r * 127));
        }
    } else if (!strcmp(name, "programi")) {
        write_delta(update->time);
        out_file->put(0xC0 + to_midi_channel(update->chan));
        write_data(update->parameter.i);
    } else if (!strcmp(name, "bendr")) {
        int temp = ROUND(0x2000 * (update->parameter.r + 1));
        if (temp > 0x3fff) temp = 0x3fff; // 14 bits maximum
        if (temp < 0) temp = 0;
        int c1 = temp & 0x7F; // low 7 bits
        int c2 = temp >> 7;   // high 7 bits
        write_delta(update->time);
        out_file->put(0xE0 + to_midi_channel(update->chan));
        write_data(c1);
        write_data(c2);
    } else if (!strncmp(name, "control", 7) && 
               update->parameter.attr_type() == 'r') {
      int ctrlnum = atoi(name + 7);
      int val = ROUND(update->parameter.r * 127);
      write_delta(update->time);
      out_file->put(0xB0 + to_midi_channel(update->chan));
      write_data(ctrlnum);
      write_data(val);
    } else if (!strcmp(name, "sysexs") &&
               update->parameter.attr_type() == 's') {
        const char *s = update->parameter.s;
        if (s[0] && s[1] && toupper(s[0]) == 'F' && s[1] == '0') {
            s += 2; // skip the initial "F0" byte in message: it is implied
        }
        write_delta(update->time);
        write_binary(0xF0, s);
    } else if (!strcmp(name, "sqspecifics") &&
               update->parameter.attr_type() == 's') {
        const char *s = update->parameter.s;
        write_delta(update->time);
        out_file->put('\xFF');
        write_binary(0x7F, s);

    /****Text Events****/
    } else if (!strcmp(name, "texts")) {
        write_text(update, 0x01);
    } else if (!strcmp(name, "copyrights")) {
        write_text(update, 0x02);
    } else if (!strcmp(name, "seqnames") || !strcmp(name, "tracknames")) {
        write_text(update, 0x03);
    } else if (!strcmp(name, "instruments")) {
        write_text(update, 0x04);
    } else if (!strcmp(name, "lyrics")) {
        write_text(update, 0x05);
    } else if (!strcmp(name, "markers")) {
        write_text(update, 0x06);
    } else if (!strcmp(name, "cues")) {
        write_text(update, 0x07);
    } else if (!strcmp(name, "miscs")) {
        write_text(update, 0x08);

    /****Other Events****/
    } else if (!strcmp(name, "smpteoffsets")) {
#define decimal(p) (((p)[0] - '0') * 10 + ((p)[1] - '0'))
        // smpteoffset is specified as "24fps:00h:10m:00s:11.00f"
        // the following simple parser does not reject all badly
        // formatted strings, but it should parse good strings ok
        const char *s = update->parameter.s;
        int len = strlen(s);
        char smpteoffset[5];
        if (len < 24) return; // not long enough, must be bad format
        int fps;
        if (s[0] == '2') {
            if (s[1] == '4') fps = 0;
            else if (s[1] == '5') fps = 1;
            else if (s[1] == '9') {
                fps = 2;
                if (len != 27) return; // not right length
                s += 3; // cancel effect of longer string
            }
        } else fps = 3;
        s += 6;   int hours = decimal(s);
        s += 4;   int mins = decimal(s);
        s += 4;   int secs = decimal(s);
        s += 4;   int frames = decimal(s);
        s += 3;   int subframes = decimal(s);
        smpteoffset[0] = (fps << 6) + hours;
        smpteoffset[1] = mins;
        smpteoffset[2] = secs;
        smpteoffset[3] = frames;
        smpteoffset[4] = subframes;
        write_smpteoffset(update, smpteoffset);

    // key signature is special because it takes two events in the Alg_seq
    // structure to make one midi file event. When we encounter one or 
    // the other event, we'll just record it in the Alg_smf_write object.
    // After both events are seen, we write the data. (See below.)
    } else if (!strcmp(name, "keysigi")) {
        keysig = update->parameter.i;
        keysig_when = update->time;
    } else if (!strcmp(name, "modea")) {
        if (!strcmp(alg_attr_name(update->parameter.a), "major"))
            keysig_mode = 'M';
        else keysig_mode = 'm';
        keysig_when = update->time;
    }
    if (keysig != -99 && keysig_mode) { // write when both are defined
        write_delta(keysig_when);
        out_file->put('\xFF');
        out_file->put('\x59');
        out_file->put(2);
        // mask off high bits so that this value appears to be positive
        // i.e. -1 -> 0xFF (otherwise, write_data will clip -1 to 0)
        out_file->put(keysig & 0xFF);
        out_file->put(keysig_mode == 'm');
        keysig = -99;
        keysig_mode = false;
    }
    //printf("Update: %s, key: %g\n", update->parameter.attr_name(), update->key);
}


// see notes on event_queue::push, TICK_TIME converts from beat to
// the number of the nearest tick. The second parameter is an offset in
// quarter ticks. By scheduling with -1, note-offs should get dispatched
// first. Note that TICK_TIME only determines the order of events, so
// it is ok to change units from beats to ticks, saving a divide.
#define TICK_TIME(t, o) (ROUND((t) * division) + 0.25 * (o))

void Alg_smf_write::write_track(int i)
{
    int j = 0; // note index
    Alg_events &notes = seq->track_list[i];
    event_queue *pending = NULL;
    if (notes.length() > 0) {
        pending = new event_queue('n', TICK_TIME(notes[j]->time, 0), 0, NULL);
    }
    if (i == 0) { // track 0 may have tempo and timesig info
        if (seq->get_time_map()->last_tempo_flag || seq->get_time_map()->beats.len > 0) {
            pending = push(pending, new event_queue('c', 0.0, 0, NULL));
        }
        if (seq->time_sig.length() > 0) {
            pending = push(pending, new event_queue('s', 
                           TICK_TIME(seq->time_sig[0].beat, 0), 0, NULL));
        }
    }
    while (pending) {
        event_queue *current = pending;
        pending = pending->next;
        if (current->type == 'n') {
            Alg_note_ptr n = (Alg_note_ptr) notes[current->index];
            if (n->is_note()) {
                write_note(n, true);
                pending = push(pending, new event_queue('o',
                      TICK_TIME(n->time + n->dur, -1), current->index, NULL));
            } else if (n->is_update()) {
                Alg_update_ptr u = (Alg_update_ptr) n;
                write_update(u);
            }
            int next = current->index + 1;
            if (next < notes.length()) {
                current->time = TICK_TIME(notes[next]->time, 0);
                current->index = next;
                pending = push(pending, current);
            }
        } else if (current->type == 'o') { //note-off
            Alg_note_ptr n = (Alg_note_ptr) notes[current->index];
            write_note(n, false);
            delete current;
        } else if (current->type == 'c') { // tempo change
            write_tempo_change(current->index);
            current->index++; // -R
            if (current->index < seq->get_time_map()->beats.len) {
                current->time = 
                    TICK_TIME(seq->get_time_map()->
                              beats[current->index].beat, 0);
                pending = push(pending, current);
            } else {
                delete current;
            }
        } else if (current->type == 's') { // time sig
            write_time_signature(current->index);
            current->index++;
            if (current->index < seq->time_sig.length()) {
                current->time = 
                    TICK_TIME(seq->time_sig[current->index].beat, 0);
                pending = push(pending, current);
            } else {
                delete current;
            }
        }
    }
}


void Alg_smf_write::write_tempo(int divs, int tempo)
{
    //    printf("Inserting tempo %f after %f clocks.\n", tempo, delta);
    write_varinum(divs - previous_divs);
    previous_divs = divs;
    out_file->put('\xFF');
    out_file->put('\x51');
    out_file->put('\x03');
    write_24bit((int)tempo);
}


void Alg_smf_write::write_tempo_change(int i)
    //  i is index of tempo map
{
    // extract tempo map
    Alg_beats &b = seq->get_time_map()->beats;
    double tempo;
    long divs;
    if (i < seq->get_time_map()->beats.len - 1) {
        tempo = 1000000 * ((b[i+1].time - b[i].time) / 
                           (b[i+1].beat - b[i].beat));
        divs = ROUND(b[i].beat * division);
        write_tempo(divs, ROUND(tempo));
    } else if (seq->get_time_map()->last_tempo_flag) { // write the final tempo
        divs = ROUND(division * b[i].beat);
        tempo = (1000000.0 / seq->get_time_map()->last_tempo);
        write_tempo(divs, ROUND(tempo));
    }    
}


void Alg_smf_write::write_time_signature(int i)
{
    Alg_time_sigs &ts = seq->time_sig;
    write_delta(ts[i].beat);
    // write the time signature
    out_file->put('\xFF');
    out_file->put('\x58');  // time signature
    out_file->put('\x04');     // length of message
    out_file->put(ROUND(ts[i].num));
    int den = ROUND(ts[i].den);
    int den_byte = 0;
    while (den > 1) { // compute the log2 of denominator
        den_byte++;
        den >>= 1;
    }
    out_file->put(den_byte);
    out_file->put(24); // clocks per quarter
    out_file->put(8);  // 32nd notes per 24 clocks
}



void Alg_smf_write::write(ostream &file)
{
    int track_len_offset;
    int track_end_offset;
    int track_len;

    out_file = &file;

    // Header
    file << "MThd";

    write_32bit(6); // chunk length

    write_16bit(1); // format 1 MIDI file

    write_16bit(seq->tracks()); // number of tracks
    write_16bit(division); // divisions per quarter note


    // write_ all tracks
    seq->convert_to_beats();
    int i;
    for (i = 0; i < seq->tracks(); i++) {
        previous_divs = 0;
        *out_file << "MTrk";
        track_len_offset = out_file->tellp();
        write_32bit(0); // track len placeholder
        
        write_track(i);

        // End of track event
        write_varinum(0);           // delta time
        out_file->put('\xFF');
        out_file->put('\x2F');
        out_file->put('\x00');

        // Go back and write in the length of the track
        track_end_offset = out_file->tellp();
        track_len = track_end_offset - track_len_offset - 4;
        out_file->seekp(track_len_offset);
        write_32bit(track_len);
        out_file->seekp(track_end_offset);
    }
}


void Alg_smf_write::write_16bit(int num)
{
    out_file->put((num & 0xFF00) >> 8);
    out_file->put(num & 0xFF);
}

void Alg_smf_write::write_24bit(int num)
{
    out_file->put((num & 0xFF0000) >> 16);
    out_file->put((num & 0xFF00) >> 8);
    out_file->put((num & 0xFF));
}

void Alg_smf_write::write_32bit(int num)
{
    out_file->put((num & 0xFF000000) >> 24);
    out_file->put((num & 0xFF0000) >> 16);
    out_file->put((num & 0xFF00) >> 8);
    out_file->put((num & 0xFF));
}


void Alg_smf_write::write_delta(double event_time)
{
    // divisions is ideal absolute time in divisions
    long divisions = ROUND(division * event_time);
    long delta_divs = divisions - previous_divs;
    write_varinum(delta_divs);
    previous_divs = divisions;    
}


void Alg_smf_write::write_varinum(int value)
{
  if(value<0) value=0;//this line should not have to be here!
  int buffer;

  buffer = value & 0x7f;
  while ((value >>= 7) > 0) {
    buffer <<= 8;
    buffer |= 0x80;
    buffer += (value & 0x7f);
  }

  for(;;) {
    out_file->put(buffer);
    if (buffer & 0x80)
      buffer >>= 8;
    else
      break;
  }
}


void Alg_seq::smf_write(ostream &file)
{
    Alg_smf_write writer(this);
    writer.write(file);
}

bool Alg_seq::smf_write(const char *filename)
{
    ofstream outf(filename, ios::binary | ios::out);
    if (outf.fail()) return false;
    smf_write(outf);
    outf.close();
    return true;
}
