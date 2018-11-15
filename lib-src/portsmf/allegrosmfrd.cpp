// midifile reader

#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "assert.h"
#include <string>
#include <fstream>
#include "allegro.h"
#include "algsmfrd_internal.h"
#include "mfmidi.h"
#include "trace.h"

using namespace std;

typedef class Alg_note_list {
public:
    Alg_note_ptr note;
    class Alg_note_list *next;
    Alg_note_list(Alg_note_ptr n, class Alg_note_list *list) { 
        note = n; next = list; }
} *Alg_note_list_ptr;


class Alg_midifile_reader: public Midifile_reader {
public:
    istream *file;
    Alg_seq_ptr seq;
    int divisions;
    Alg_note_list_ptr note_list;
    Alg_track_ptr track;
    int track_number; // the number of the (current) track
    // chan is actual_channel + channel_offset_per_track * track_num +
    //                          channel_offset_per_track * port 
    long channel_offset_per_track; // used to encode track number into channel
        // default is 0, set this to 0 to merge all tracks to 16 channels
    long channel_offset_per_port; // used to encode port number into channel
        // default is 16, set to 0 to ignore port prefix meta events
    // while reading, this is channel_offset_per_track * track_num
    int channel_offset;

    Alg_midifile_reader(istream &f, Alg_seq_ptr new_seq) {
        file = &f;
        note_list = NULL;
        seq = new_seq;
        channel_offset_per_track = 0;
        channel_offset_per_port = 16;
        track_number = -1; // no tracks started yet, 1st will be #0
        meta_channel = -1;
        port = 0;
    }
    // delete destroys the seq member as well, so set it to NULL if you
    // copied the pointer elsewhere
    ~Alg_midifile_reader();
    // the following is used to load the Alg_seq from the file:
    bool parse();

    void set_nomerge(bool flag) { Mf_nomerge = flag; }
    void set_skipinit(bool flag) { Mf_skipinit = flag; }
    long get_currtime() { return Mf_currtime; }

protected:
    int meta_channel; // the channel for meta events, set by MIDI chan prefix
    int port; // value from the portprefix meta event

    double get_time();
    void update(int chan, int key, Alg_parameter_ptr param);
    void *Mf_malloc(size_t size) { return malloc(size); }
    void Mf_free(void *obj, size_t size) { free(obj); }
    /* Methods to be called while processing the MIDI file. */
    void Mf_starttrack();
    void Mf_endtrack();
    int Mf_getc();
    void Mf_chanprefix(int chan);
    void Mf_portprefix(int port);
    void Mf_eot();
    void Mf_error(const char *);
    void Mf_header(int,int,int);
    void Mf_on(int,int,int);
    void Mf_off(int,int,int);
    void Mf_pressure(int,int,int);
    void Mf_controller(int,int,int);
    void Mf_pitchbend(int,int,int);
    void Mf_program(int,int);
    void Mf_chanpressure(int,int);
    void binary_msg(int len, unsigned char *msg, const char *attr_string);
    void Mf_sysex(int,unsigned char*);
    void Mf_arbitrary(int,unsigned char*);
    void Mf_metamisc(int,int,unsigned char*);
    void Mf_seqnum(int);
    void Mf_smpte(int,int,int,int,int);
    void Mf_timesig(int,int,int,int);
    void Mf_tempo(int);
    void Mf_keysig(int,int);
    void Mf_sqspecific(int,unsigned char*);
    void Mf_text(int,int,unsigned char*);
};


Alg_midifile_reader::~Alg_midifile_reader()
{
    while (note_list) {
        Alg_note_list_ptr to_be_freed = note_list;
        note_list = note_list->next;
        delete to_be_freed;
    }
    finalize(); // free Mf reader memory
}


bool Alg_midifile_reader::parse()
{
    channel_offset = 0;
    seq->convert_to_beats();
    midifile();
    seq->set_real_dur(seq->get_time_map()->beat_to_time(seq->get_beat_dur()));
    return midifile_error != 0;
}


void Alg_midifile_reader::Mf_starttrack()
{
    // printf("starting new track\n");
    // create a new track that will share the sequence time map
    // since time is in beats, the seconds parameter is false
    track_number++;
    seq->add_track(track_number); // make sure track exists
    track = seq->track(track_number); // keep pointer to current track
    meta_channel = -1;
    port = 0;
}


void Alg_midifile_reader::Mf_endtrack()
{
    // note: track is already part of seq, so do not add it here
    // printf("finished track, length %d number %d\n", track->len, track_num / 100);
    channel_offset += seq->channel_offset_per_track;
    track = NULL;
    double now = get_time();
    if (seq->get_beat_dur() < now) seq->set_beat_dur(now);
    meta_channel = -1;
    port = 0;
}


int Alg_midifile_reader::Mf_getc()
{
    return file->get();
}


void Alg_midifile_reader::Mf_chanprefix(int chan)
{
    meta_channel = chan;
}


void Alg_midifile_reader::Mf_portprefix(int p)
{
    port = p;
}


void Alg_midifile_reader::Mf_eot()
{
    meta_channel = -1;
    port = 0;
}


void Alg_midifile_reader::Mf_error(const char *msg)
{
    fprintf(stdout, "Midifile reader error: %s\n", msg);
}


void Alg_midifile_reader::Mf_header(int format, int ntrks, int division)
{
    if (format > 1) {
        char msg[80];
#pragma warning(disable: 4996) // msg is long enough
        sprintf(msg, "file format %d not implemented", format);
#pragma warning(default: 4996)
        Mf_error(msg);
    }
    divisions = division;
}


double Alg_midifile_reader::get_time()
{
    double beat = ((double) get_currtime()) / divisions;
    return beat;
}


void Alg_midifile_reader::Mf_on(int chan, int key, int vel)
{
    assert(!seq->get_units_are_seconds());
    if (vel == 0) {
        Mf_off(chan, key, vel);
        return;
    }
    Alg_note_ptr note = new Alg_note();
    note_list = new Alg_note_list(note, note_list);
    /*    trace("on: %d at %g\n", key, get_time()); */
    note->time = get_time();
    note->chan = chan + channel_offset + port * channel_offset_per_port;
    note->dur = 0;
    note->set_identifier(key);
    note->pitch = (float) key;
    note->loud = (float) vel;
    track->append(note);
    meta_channel = -1;
}


void Alg_midifile_reader::Mf_off(int chan, int key, int vel)
{
    double time = get_time();
    Alg_note_list_ptr *p = &note_list;
    while (*p) {
        if ((*p)->note->get_identifier() == key &&
            (*p)->note->chan == 
                    chan + channel_offset + port * channel_offset_per_port) {
            (*p)->note->dur = time - (*p)->note->time;
            // trace("updated %d dur %g\n", (*p)->note->key, (*p)->note->dur);
            Alg_note_list_ptr to_be_freed = *p;
            *p = to_be_freed->next;
            delete to_be_freed;
        } else {
            p = &((*p)->next);
        }
    }
    meta_channel = -1;
}


void Alg_midifile_reader::update(int chan, int key, Alg_parameter_ptr param)
{
    Alg_update_ptr update = new Alg_update;
    update->time = get_time();
    update->chan = chan;
    if (chan != -1) {
        update->chan = chan + channel_offset + port * channel_offset_per_port;
    }
    update->set_identifier(key);
    update->parameter = *param;
    // prevent the destructor from destroying the string twice!
    // the new Update takes the string from param
    if (param->attr_type() == 's') param->s = NULL;
    track->append(update);
}


void Alg_midifile_reader::Mf_pressure(int chan, int key, int val)
{
    Alg_parameter parameter;
    parameter.set_attr(symbol_table.insert_string("pressurer"));
    parameter.r = val / 127.0;
    update(chan, key, &parameter);
    meta_channel = -1;
}


void Alg_midifile_reader::Mf_controller(int chan, int control, int val)
{
    Alg_parameter parameter;
    char name[32];
#pragma warning(disable: 4996) // name is long enough
    sprintf(name, "control%dr", control);
#pragma warning(default: 4996)
    parameter.set_attr(symbol_table.insert_string(name));
    parameter.r = val / 127.0;
    update(chan, -1, &parameter);
    meta_channel = -1;
}


void Alg_midifile_reader::Mf_pitchbend(int chan, int c1, int c2)
{
    Alg_parameter parameter;
    parameter.set_attr(symbol_table.insert_string("bendr"));
    parameter.r = ((c2 << 7) + c1) / 8192.0 - 1.0;
    update(chan, -1, &parameter);
    meta_channel = -1;
}


void Alg_midifile_reader::Mf_program(int chan, int program)
{
    Alg_parameter parameter;
    parameter.set_attr(symbol_table.insert_string("programi"));
    parameter.i = program;
    update(chan, -1, &parameter);
    meta_channel = -1;
}


void Alg_midifile_reader::Mf_chanpressure(int chan, int val)
{
    Alg_parameter parameter;
    parameter.set_attr(symbol_table.insert_string("pressurer"));
    parameter.r = val / 127.0;
    update(chan, -1, &parameter);
    meta_channel = -1;
}


void Alg_midifile_reader::binary_msg(int len, unsigned char *msg, 
                                     const char *attr_string)
{
    Alg_parameter parameter;
    char *hexstr = new char[len * 2 + 1];
    for (int i = 0; i < len; i++) {
#pragma warning(disable: 4996) // hexstr is long enough
        sprintf(hexstr + 2 * i, "%02x", (0xFF & msg[i]));
#pragma warning(default: 4996)
    }
    parameter.s = hexstr;
    parameter.set_attr(symbol_table.insert_string(attr_string));
    update(meta_channel, -1, &parameter);
}


void Alg_midifile_reader::Mf_sysex(int len, unsigned char *msg)
{
    // sysex messages become updates with attribute sysexs and a hex string
    binary_msg(len, msg, "sysexs");
}


void Alg_midifile_reader::Mf_arbitrary(int len, unsigned char *msg)
{
    Mf_error("arbitrary data ignored");
}


void Alg_midifile_reader::Mf_metamisc(int type, int len, unsigned char *msg)
{
    char text[128];
#pragma warning(disable: 4996) // text is long enough
    sprintf(text, "metamsic data, type 0x%x, ignored", type);
#pragma warning(default: 4996)
    Mf_error(text);
}


void Alg_midifile_reader::Mf_seqnum(int n)
{
    Mf_error("seqnum data ignored");
}


static const char *fpsstr[4] = {"24", "25", "29.97", "30"};

void Alg_midifile_reader::Mf_smpte(int hours, int mins, int secs,
                                   int frames, int subframes)
{
    // string will look like "24fps:01h:27m:07s:19.00f"
    // 30fps (drop frame) is notated as "29.97fps"
    char text[32];
    int fps = (hours >> 6) & 3;
    hours &= 0x1F;
#pragma warning(disable: 4996) // text is long enough
    sprintf(text, "%sfps:%02dh:%02dm:%02ds:%02d.%02df", 
            fpsstr[fps], hours, mins, secs, frames, subframes);
#pragma warning(default: 4996)
    Alg_parameter smpteoffset;
    smpteoffset.s = heapify(text);
    smpteoffset.set_attr(symbol_table.insert_string("smpteoffsets"));
    update(meta_channel, -1, &smpteoffset);
    // Mf_error("SMPTE data ignored");
}


void Alg_midifile_reader::Mf_timesig(int i1, int i2, int i3, int i4)
{
    seq->set_time_sig(double(get_currtime()) / divisions, i1, 1 << i2);
}


void Alg_midifile_reader::Mf_tempo(int tempo)
{
    double beat = get_currtime();
    beat = beat / divisions; // convert to quarters
    // 6000000 us/min / n us/beat => beat / min
    double bpm = 60000000.0 / tempo;
    seq->insert_tempo(bpm, beat);
}


void Alg_midifile_reader::Mf_keysig(int key, int mode)
{
    Alg_parameter key_parm;
    key_parm.set_attr(symbol_table.insert_string("keysigi"));
    // use 0 for C major, 1 for G, -1 for F, etc., that is,
    // the number of sharps, where flats are negative sharps
    key_parm.i = key; //<<<---- fix this
    // use -1 to mean "all channels"
    update(meta_channel, -1, &key_parm);
    Alg_parameter mode_parm;
    mode_parm.set_attr(symbol_table.insert_string("modea"));
    mode_parm.a = (mode == 0 ? symbol_table.insert_string("major") :
                               symbol_table.insert_string("minor"));
    update(meta_channel, -1, &mode_parm);
}


void Alg_midifile_reader::Mf_sqspecific(int len, unsigned char *msg)
{
    // sequencer specific messages become updates with attribute sqspecifics
    // and a hex string for the value
    binary_msg(len, msg, "sqspecifics");
}


char *heapify2(int len, unsigned char *s)
{
    char *h = new char[len + 1];
    memcpy(h, s, len);
    h[len] = 0;
    return h;
}


void Alg_midifile_reader::Mf_text(int type, int len, unsigned char *msg)
{
    Alg_parameter text;
    text.s = heapify2(len, msg);
    const char *attr = "miscs";
    if (type == 1) attr = "texts";
    else if (type == 2) attr = "copyrights";
    else if (type == 3) 
        attr = (track_number == 0 ? "seqnames" : "tracknames");
    else if (type == 4) attr = "instruments";
    else if (type == 5) attr = "lyrics";
    else if (type == 6) attr = "markers";
    else if (type == 7) attr = "cues";
    text.set_attr(symbol_table.insert_string(attr));
    update(meta_channel, -1, &text);
}


// parse file into a seq. 
Alg_error alg_smf_read(istream &file, Alg_seq_ptr new_seq)
{
    assert(new_seq);
    Alg_midifile_reader ar(file, new_seq);
    bool err = ar.parse();
    ar.seq->set_real_dur(ar.seq->get_time_map()->
                         beat_to_time(ar.seq->get_beat_dur()));
    return (err ? alg_error_syntax : alg_no_error);
}
