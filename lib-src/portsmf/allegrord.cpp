#include "assert.h"
#include "stdlib.h"
#include "string.h"
#include "ctype.h"
#include "trace.h"
#include <string>
#include <fstream>
#include <algorithm>
#include "strparse.h"
#include "allegro.h"
#include "algrd_internal.h"

using namespace std;

#define streql(s1, s2) (strcmp(s1, s2) == 0)
#define field_max 80

class Alg_reader {
public:
    istream *file;
    string input_line;
    int line_no;
    String_parse line_parser;
    bool line_parser_flag;
    string field;
    bool error_flag;
    Alg_seq_ptr seq;
    double tsnum;
    double tsden;
    double offset;
    bool offset_found;

    Alg_reader(istream *a_file, Alg_seq_ptr new_seq);
    void readline();
    Alg_parameters_ptr process_attributes(Alg_parameters_ptr attributes,
                                          double time);
    bool parse();
    long parse_chan(string &field);
    long parse_int(string &field);
    int find_real_in(string &field, int n);
    double parse_real(string &field);
    void parse_error(string &field, long offset, const char *message);
    double parse_dur(string &field, double base);
    double parse_after_dur(double dur, string &field, int n, double base);
    double parse_loud(string &field);
    long parse_key(string &field);
    double parse_pitch(string &field);
    long parse_after_key(int key, string &field, int n);
    long find_int_in(string &field, int n);
    bool parse_attribute(string &field, Alg_parameter_ptr parm);
    bool parse_val(Alg_parameter_ptr param, string &s, int i);
    bool check_type(char type_char, Alg_parameter_ptr param);
};


double Alg_reader::parse_pitch(string &field)
{
    if (isdigit(field[1])) {
        int last = find_real_in(field, 1);
        string real_string = field.substr(1, last - 1);
        return atof(real_string.c_str());
    } else {
        return (double) parse_key(field);
    }
}


// it is the responsibility of the caller to delete
// the seq
Alg_reader::Alg_reader(istream *a_file, Alg_seq_ptr new_seq)
{
    file = a_file; // save the file
    line_parser_flag = false;
    line_no = 0;
    tsnum = 4; // default time signature
    tsden = 4;
    seq = new_seq;
    offset = 0.0;
    offset_found = false;
}


Alg_error alg_read(istream &file, Alg_seq_ptr new_seq, double *offset_ptr)
    // read a sequence from allegro file
{
    assert(new_seq);
    Alg_reader alg_reader(&file, new_seq);
    bool err = alg_reader.parse();
    if (!err && offset_ptr) {
        *offset_ptr = alg_reader.offset;
    }
    return (err ? alg_error_syntax : alg_no_error);
}


void Alg_reader::readline()
{
    // a word about memory management: this Alg_reader has a
    // member variable input_line that holds a line of input
    // it is reused for each line. input_line is parsed by
    // line_parser, which holds a reference to input_line
    line_parser_flag = false;
    if (getline(*file, input_line)) {
        line_parser.init(&input_line);
        line_parser_flag = true;
        error_flag = false;
    }
}


Alg_parameters_ptr Alg_reader::process_attributes(
        Alg_parameters_ptr attributes, double time)
{
    // print "process_attributes:", attributes
    bool ts_flag = false;
    if (attributes) {
        Alg_parameters_ptr a;
        bool in_seconds = seq->get_units_are_seconds();
        if ((a = Alg_parameters::remove_key(&attributes, "tempor"))) {
            double tempo = a->parm.r;
            seq->insert_tempo(tempo, seq->get_time_map()->time_to_beat(time));
        }
        if ((a = Alg_parameters::remove_key(&attributes, "beatr"))) {
            double beat = a->parm.r;
            seq->insert_beat(time, beat);
        }
        if ((a = Alg_parameters::remove_key(&attributes, "timesig_numr"))) {
            tsnum = a->parm.r;
            ts_flag = true;
        }
        if ((a = Alg_parameters::remove_key(&attributes, "timesig_denr"))) {
            tsden = a->parm.r;
            ts_flag = true;
        }
        if (ts_flag) {
            seq->set_time_sig(seq->get_time_map()->time_to_beat(time),
            tsnum, tsden);
        }
        if (in_seconds) seq->convert_to_seconds();
    }
    return attributes; // in case it was modified
}


bool Alg_reader::parse()
{
    int voice = 0;
    int key = 60;
    double loud = 100.0;
    double pitch = 60.0;
    double dur = 1.0;
    double time = 0.0;
    int track_num = 0;
    seq->convert_to_seconds();
    //seq->set_real_dur(0.0); // just in case it's not initialized already
    readline();
    bool valid = false; // ignore blank lines
    while (line_parser_flag) {
        bool time_flag = false;
        bool next_flag = false;
        double next;
        bool voice_flag = false;
        bool loud_flag = false;
        bool dur_flag = false;
        bool new_pitch_flag = false; // "P" syntax or "A"-"G" syntax
        double new_pitch = 0.0;
        bool new_key_flag = false;   // "K" syntax
        int new_key = 0;
        Alg_parameters_ptr attributes = NULL;
        if (line_parser.peek() == '#') {
            // look for #track
            line_parser.get_nonspace_quoted(field);
            if (streql(field.c_str(), "#track")) {
                line_parser.get_nonspace_quoted(field); // number
                field.insert(0, " "); // need char at beginning because
                // parse_int ignores the first character of the argument
                track_num = parse_int(field);
                seq->add_track(track_num);

                // maybe we have a sequence or track name
                line_parser.get_remainder(field);
                // if there is a non-space character after #track n then
                // use it as sequence or track name. Note that because we
                // skip over spaces, a sequence or track name cannot begin
                // with leading blanks. Another decision is that the name
                // must be at time zero
                if (field.length() > 0) {
                    // insert the field as sequence name or track name
                    Alg_update_ptr update = new Alg_update;
                    update->chan = -1;
                    update->time = 0;
                    update->set_identifier(-1);
                    // sequence name is whatever is on track 0
                    // other tracks have track names
                    const char *attr =
                            (track_num == 0 ? "seqnames" : "tracknames");
                    update->parameter.set_attr(
                            symbol_table.insert_string(attr));
                    update->parameter.s = heapify(field.c_str());
                    seq->add_event(update, track_num);
                }
            } else if (streql(field.c_str(), "#offset")) {
                if (offset_found) {
                    parse_error(field, 0, "#offset specified twice");
                }
                offset_found = true;
                line_parser.get_nonspace_quoted(field); // number
                field.insert(0, " "); // need char at beginning because
                // parse_real ignores first character in the argument
                offset = parse_real(field);
            }
        } else {
            // we must have a track to insert into
            if (seq->tracks() == 0) seq->add_track(0);
            line_parser.get_nonspace_quoted(field);
            char pk = line_parser.peek();
            // attributes are parsed as two adjacent nonspace_quoted tokens
            // so we have to conditionally call get_nonspace_quoted() again
            if (pk && !isspace(pk)) {
                string field2;
                line_parser.get_nonspace_quoted(field2);
                field.append(field2);
            }
            while (field[0]) {
                char first = toupper(field[0]);
                if (strchr("ABCDEFGKLPUSIQHW-", first)) {
                    valid = true; // it's a note or event
                }
                if (first == 'V') {
                    if (voice_flag) {
                        parse_error(field, 0, "Voice specified twice");
                    } else {
                        voice = parse_chan(field);
                    }
                    voice_flag = true;
                } else if (first == 'T') {
                    if (time_flag) {
                        parse_error(field, 0, "Time specified twice");
                    } else {
                        time = parse_dur(field, 0.0);
                    }
                    time_flag = true;
                } else if (first == 'N') {
                    if (next_flag) {
                        parse_error(field, 0, "Next specified twice");
                    } else {
                        next = parse_dur(field, time);
                    }
                    next_flag = true;
                } else if (first == 'K') {
                    if (new_key_flag) {
                        parse_error(field, 0, "Key specified twice");
                    } else {
                        new_key = parse_key(field);
                        new_key_flag = true;
                    }
                } else if (first == 'L') {
                    if (loud_flag) {
                        parse_error(field, 0, "Loudness specified twice");
                    } else {
                        loud = parse_loud(field);
                    }
                    loud_flag = true;
                } else if (first == 'P') {
                    if (new_pitch_flag) {
                        parse_error(field, 0, "Pitch specified twice");
                    } else {
                        new_pitch = parse_pitch(field);
                        new_pitch_flag = true;
                    }
                } else if (first == 'U') {
                    if (dur_flag) {
                        parse_error(field, 0, "Dur specified twice");
                    } else {
                        dur = parse_dur(field, time);
                        dur_flag = true;
                    }
                } else if (strchr("SIQHW", first)) {
                    if (dur_flag) {
                        parse_error(field, 0, "Dur specified twice");
                    } else {
                        // prepend 'U' to field, copy EOS too
                        field.insert((unsigned int) 0, 1, 'U');
                        dur = parse_dur(field, time);
                        dur_flag = true;
                    }
                } else if (strchr("ABCDEFG", first)) {
                    if (new_pitch_flag) {
                        parse_error(field, 0, "Pitch specified twice");
                    } else {
                        // prepend 'P' to field
                        field.insert((unsigned int) 0, 1, 'P');
                        new_pitch = parse_pitch(field);
                        new_pitch_flag = true;
                    }
                } else if (first == '-') {
                    Alg_parameter parm;
                    if (parse_attribute(field, &parm)) { // enter attribute-value pair
                        attributes = new Alg_parameters(attributes);
                        attributes->parm = parm;
                        parm.s = NULL; // protect string from deletion by destructor
                    }
                } else {
                    parse_error(field, 0, "Unknown field");
                }

                if (error_flag) {
                    field[0] = 0; // exit the loop
                } else {
                    line_parser.get_nonspace_quoted(field);
                    pk = line_parser.peek();
                    // attributes are parsed as two adjacent nonspace_quoted 
                    // tokens so we have to conditionally call 
                    // get_nonspace_quoted() again
                    if (pk && !isspace(pk)) {
                        string field2;
                        line_parser.get_nonspace_quoted(field2);
                        field.append(field2);
                    }
                }
            }
            // a case analysis:
            // Key < 128 implies pitch unless pitch is explicitly given
            // Pitch implies Key unless key is explicitly given,
            // Pitch is rounded to nearest integer to determine the Key
            //    if necessary, so MIDI files will lose the pitch fraction
            // A-G is a Pitch specification (therefore it implies Key)
            //   K60 P60 -- both are specified, use 'em
            //   K60 P60 C4 -- overconstrained, an error
            //   K60 C4 -- OK, but K60 is already implied by C4
            //   K60 -- OK, pitch is 60
            //   C4 P60 -- over constrained
            //   P60 -- OK, key is 60
            //   P60.1 -- OK, key is 60
            //   C4 -- OK, key is 60, pitch is 60
            //   <nothing> -- OK, key and pitch from before
            //   K200 P60 -- ok, pitch is 60
            //   K200 with neither P60 nor C4 uses 
            //       pitch from before

            // figure out what the key/instance is:
            if (new_key_flag) { // it was directly specified
                key = new_key;
            } else if (new_pitch_flag) {
                // pitch was specified, but key was not; get key from pitch
                key = (int) (new_pitch + 0.5); // round to integer key number
            }
            if (new_pitch_flag) {
                pitch = new_pitch;
            } else if (key < 128 && new_key_flag) {
                // no explicit pitch, but key < 128, so it implies pitch
                pitch = key;
                new_pitch_flag = true;
            }
            // now we've acquired new parameters
            // if (it is a note, then enter the note
            if (valid) {
                // change tempo or beat
                attributes = process_attributes(attributes, time);
                // if there's a duration or pitch, make a note:
                if (new_pitch_flag || dur_flag) {
                    Alg_note_ptr note_ptr = new Alg_note;
                    note_ptr->chan = voice;
                    note_ptr->time = time;
                    note_ptr->dur = dur;
                    note_ptr->set_identifier(key);
                    note_ptr->pitch = (float) pitch;
                    note_ptr->loud = (float) loud;
                    note_ptr->parameters = attributes;
                    seq->add_event(note_ptr, track_num); // sort later
                    if (seq->get_real_dur() < (time + dur)) seq->set_real_dur(time + dur);
                } else {
                    int update_key = -1;
                    // key must appear explicitly; otherwise
                    //    update applies to channel
                    if (new_key_flag) {
                        update_key = key;
                    }
                    if (loud_flag) {
                        Alg_update_ptr new_upd = new Alg_update;
                        new_upd->chan = voice;
                        new_upd->time = time;
                        new_upd->set_identifier(update_key);
                        new_upd->parameter.set_attr(symbol_table.insert_string("loudr"));
                        new_upd->parameter.r = pitch;
                        seq->add_event(new_upd, track_num);
                        if (seq->get_real_dur() < time) seq->set_real_dur(time);
                    }
                    if (attributes) {
                        while (attributes) {
                            Alg_update_ptr new_upd = new Alg_update;
                            new_upd->chan = voice;
                            new_upd->time = time;
                            new_upd->set_identifier(update_key);
                            new_upd->parameter = attributes->parm;
                            seq->add_event(new_upd, track_num);
                            Alg_parameters_ptr p = attributes;
                            attributes = attributes->next;
                            p->parm.s = NULL; // so we don't delete the string
                            delete p;
                        }
                    }
                }
                if (next_flag) {
                    time = time + next;
                } else if (dur_flag || new_pitch_flag) { // a note: incr by dur
                    time = time + dur;
                }
            }
        }
        readline();
    }
    if (!error_flag) { // why not convert even if there was an error? -RBD
        seq->convert_to_seconds(); // make sure format is correct
    }
    // real_dur is valid, translate to beat_dur
    seq->set_beat_dur((seq->get_time_map())->time_to_beat(seq->get_real_dur()));
    return error_flag;
}


long Alg_reader::parse_chan(string &field)
{
    const char *int_string = field.c_str() + 1;
    const char *msg = "Integer or - expected";
    const char *p = int_string;
    char c;
    // check that all chars in int_string are digits or '-':
    while ((c = *p++)) {
        if (!isdigit(c) && c != '-') {
            parse_error(field, p - field.c_str() - 1, msg);
            return 0;
        }
    }
    p--; // p now points to end-of-string character
    if (p - int_string == 0) {
        // bad: string length is zero
        parse_error(field, 1, msg);
        return 0;
    }
    if (p - int_string == 1 && int_string[0] == '-') {
        // special case: entire string is "-", interpret as -1
        return -1;
    }
    return atoi(int_string);
}


long Alg_reader::parse_int(string &field)
{
    const char *int_string = field.c_str() + 1;
    const char *msg = "Integer expected";
    const char *p = int_string;
    char c;
    // check that all chars in int_string are digits:
    while ((c = *p++)) {
        if (!isdigit(c)) {
            parse_error(field, p - field.c_str() - 1, msg);
            return 0;
        }
    }
    p--; // p now points to end-of-string character
    if (p - int_string == 0) {
        // bad: string length is zero
        parse_error(field, 1, msg);
        return 0;
    }
    return atoi(int_string);
}


int Alg_reader::find_real_in(string &field, int n)
{
    // scans from offset n to the end of a real constant
    bool decimal = false;
    int len = field.length();
    if (n < len && field[n] == '-') n += 1; // parse one minus sign
    for (int i = n; i < len; i++) {
        char c = field[i];
        if (!isdigit(c)) {
            if (c == '.' && !decimal) {
                decimal = true;
            } else {
                return i;
            }
        }
    }
    return len;
}


double Alg_reader::parse_real(string &field)
{
    const char *msg = "Real expected";
    int last = find_real_in(field, 1);
    string real_string = field.substr(1, last - 1);
    if (last <= 1 || last < (int) field.length()) {
       parse_error(field, 1, msg);
       return 0;
    }
    return atof(real_string.c_str());
}


void Alg_reader::parse_error(string &field, long offset, const char *message)
{
    int position = line_parser.pos - field.length() + offset;
    error_flag = true;
    puts(line_parser.str->c_str());
    for (int i = 0; i < position; i++) {
        putc(' ', stdout);
    }
    putc('^', stdout);
    printf("    %s\n", message);
}


double duration_lookup[] = { 0.25, 0.5, 1.0, 2.0, 4.0 };


double Alg_reader::parse_dur(string &field, double base)
{
    const char *msg = "Duration expected";
    const char *durs = "SIQHW";
    const char *p;
    int last;
    double dur;
    if (field.length() < 2) {
        // fall through to error message
        return -1;
    } else if (isdigit(field[1])) {
        last = find_real_in(field, 1);
        string real_string = field.substr(1, last - 1);
        dur = atof(real_string.c_str());
        // convert dur from seconds to beats
        dur = seq->get_time_map()->time_to_beat(base + dur) - 
              seq->get_time_map()->time_to_beat(base);
    } else if ((p = strchr(durs, toupper(field[1])))) {
        dur = duration_lookup[p - durs];
        last = 2;
    } else {
        parse_error(field, 1, msg);
        return 0;
    }
    dur = parse_after_dur(dur, field, last, base);
    dur = seq->get_time_map()->beat_to_time(
              seq->get_time_map()->time_to_beat(base) + dur) - base;
    return dur;
}


double Alg_reader::parse_after_dur(double dur, string &field, 
                                   int n, double base)
{
    if ((int) field.length() == n) {
        return dur;
    }
    if (toupper(field[n]) == 'T') {
        return parse_after_dur(dur * 2/3, field, n + 1, base);
    }
    if (field[n] == '.') {
        return parse_after_dur(dur * 1.5, field, n + 1, base);
    }
    if (isdigit(field[n])) {
        int last = find_real_in(field, n);
        string a_string = field.substr(n, last - n);
        double f = atof(a_string.c_str());
        return parse_after_dur(dur * f, field, last, base);
    }
    if (field[n] == '+') {
        string a_string = field.substr(n + 1);
        return dur + parse_dur(
                a_string, seq->get_time_map()->beat_to_time(
                        seq->get_time_map()->time_to_beat(base) + dur));
    }
    parse_error(field, n, "Unexpected character in duration");
    return dur;
}

struct loud_lookup_struct {
    const char *str;
    int val;
} loud_lookup[] = { {"FFF", 127}, {"FF", 120}, {"F", 110}, {"MF", 100}, 
                    {"MP", 90}, {"P", 80}, {"PP", 70}, {"PPP", 60}, 
                    {NULL, 0} };


double Alg_reader::parse_loud(string &field)
{
    const char *msg = "Loudness expected";
    if (isdigit(field[1])) {
        return parse_int(field);
    } else {
        string dyn = field.substr(1);
        transform(dyn.begin(), dyn.end(), dyn.begin(), ::toupper);
        for (int i = 0; loud_lookup[i].str; i++) {
            if (streql(loud_lookup[i].str, dyn.c_str())) {
                return (double) loud_lookup[i].val;
            }
        }
    }
    parse_error(field, 1, msg);
    return 100.0;
}


int key_lookup[] = {21, 23, 12, 14, 16, 17, 19};


// the field can be K<number> or K[A-G]<number> or P[A-G]<number>
// (this can be called from parse_pitch() to handle [A-G])
// Notice that the routine ignores the first character: K or P
//
long Alg_reader::parse_key(string &field)
{
    const char *msg = "Pitch expected";
    const char *pitches = "ABCDEFG";
    const char *p;
    if (isdigit(field[1])) {
        // This routine would not have been called if field = "P<number>"
        // so it must be "K<number>" so <number> must be an integer.
        return parse_int(field);
    } else if ((p = strchr(pitches, toupper(field[1])))) {
        long key = key_lookup[p - pitches];
        key = parse_after_key(key, field, 2);
        return key;
    }
    parse_error(field, 1, msg);
    return 0;
}


long Alg_reader::parse_after_key(int key, string &field, int n)
{
    if ((int) field.length() == n) {
        return key;
    }
    char c = toupper(field[n]);
    if (c == 'S') {
        return parse_after_key(key + 1, field, n + 1);
    }
    if (c == 'F') {
        return parse_after_key(key - 1, field, n + 1);
    }
    if (isdigit(field[n])) {
        int last = find_int_in(field, n);
        string octave = field.substr(n, last - n);
        int oct = atoi(octave.c_str());
        return parse_after_key(key + oct * 12, field, last);
    }
    parse_error(field, n, "Unexpected character in pitch");
    return key;
}


long Alg_reader::find_int_in(string &field, int n)
{
    while ((int) field.length() > n && isdigit(field[n])) {
        n = n + 1;
    }
    return n;
}


bool Alg_reader::parse_attribute(string &field, Alg_parameter_ptr param)
{
    int i = 1;
    while (i < (int) field.length()) {
        if (field[i] == ':') {
            string attr = field.substr(1, i - 1);
            char type_char = field[i - 1];
            if (strchr("iarsl", type_char)) {
                param->set_attr(symbol_table.insert_string(attr.c_str()));
                parse_val(param, field, i + 1);
            } else {
                parse_error(field, 0, "attribute needs to end with typecode: i,a,r,s, or l");
            }
            return !error_flag;
        }
        i = i + 1;
    }
    return false;
}


bool Alg_reader::parse_val(Alg_parameter_ptr param, string &s, int i)
{
    int len = (int) s.length();
    if (i >= len) {
        return false;
    }
    if (s[i] == '"') {
        if (!check_type('s', param)) {
            return false;
        }
        // note: (len - i) includes 2 quote characters but no EOS character
        // so total memory to allocate is (len - i) - 1
        char *r = new char[(len - i) - 1];
        strncpy(r, s.c_str() + i + 1, (len - i) - 2);
        r[(len - i) - 2] = 0; // terminate the string
        param->s = r;
    } else if (s[i] == '\'') {
        if (!check_type('a', param)) {
            return false;
        }
        string r = s.substr(i + 1, len - i - 2);
        param->a = symbol_table.insert_string(r.c_str());
    } else if (param->attr_type() == 'l') {
        if (streql(s.c_str() + i, "true") || 
            streql(s.c_str() + i, "t")) {
            param->l = true;
        } else if (streql(s.c_str() + i, "false") || 
                   streql(s.c_str() + i, "nil")) {
            param->l = false;
        } else return false;
    } else if (isdigit(s[i]) || s[i] == '-' || s[i] == '.') {
        int pos = i;
        bool period = false;
        int sign = 1;
        if (s[pos] == '-') {
            sign = -1;
            pos++;
        }
        while (pos < len) {
            if (isdigit(s[pos])) {
                ;
            } else if (!period && s[pos] == '.') {
                period = true;
            } else {
                parse_error(s, pos, "Unexpected char in number");
                return false;
            }
            pos = pos + 1;
        }
        string r = s.substr(i, len - i);
        if (period) {
            if (!check_type('r', param)) {
                return false;
            }
            param->r = atof(r.c_str());
        } else {
            if (param->attr_type() == 'r') {
                param->r = atoi(r.c_str());
            } else if (!check_type('i', param)) {
                return false;
            } else {
                param->i = atoi(r.c_str());
            }
        }
    } else {
        parse_error(s, i, "invalid value");
        return false;
    }
    return true;
}


bool Alg_reader::check_type(char type_char, Alg_parameter_ptr param)
{
    return param->attr_type() == type_char;
}


//duration_lookup = {"S": 0.5, "I": 0.5, "Q": 1, "H": 2, "W": 4}
//key_lookup = {"C": 12, "D": 14, "E": 16, "F": 17, "G": 19, "A": 21, "B": 23}

/*
def test():
    reader = Alg_reader(open("data\\test.gro", "r"))
    reader.parse()
    score = reader->seq.notes
    print "score:", score
    reader = nil
*/
