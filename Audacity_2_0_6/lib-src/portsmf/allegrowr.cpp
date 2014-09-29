// allegrowr.cpp -- write sequence to an Allegro file (text)

#include "assert.h"
#include "stdlib.h"
#include <iostream>
#include <fstream>
#include <iomanip>
#include <errno.h>
#include <string>
#include "memory.h"
using namespace std;
#include "strparse.h"
#include "allegro.h"

// Note about precision: %g prints 6 significant digits. For 1ms precision, 
// the maximum magnitude is 999.999, i.e. 1000s < 17minutes. For anything
// over 1000s, time in seconds will be printed with 10ms precision, which
// is not good. Therefore, times and durations are printed as %.4d, which 
// gives 100us precision.
// The following define allows you to change this decision:
/* #define TIMFMT "%.4d" */
#define TIMPREC 4
#define TIMFMT fixed << setprecision(TIMPREC)
#define GFMT resetiosflags(ios::floatfield) << setprecision(6)

void parameter_print(ostream &file, Alg_parameter_ptr p)
{
    file << " -" << p->attr_name() << ":";
    switch (p->attr_type()) {
    case 'a':
        file << "'" << alg_attr_name(p->a) << "'";
        break;
    case 'i':
        file << p->i;
        break;
    case 'l':
        file << (p->l ? "true" : "false");
        break;
    case 'r':
        file << p->r;
        break;
    case 's': {
        string str;
        string_escape(str, p->s, "\"");
        file << str;
        break;
    }
    }
}

Alg_event_ptr Alg_seq::write_track_name(ostream &file, int n, 
                                        Alg_events &events)
// write #track <n> <trackname-or-sequencename>
// if we write the name on the "#track" line, then we do *not* want
// to write again as an update: "-seqnames:"Jordu", so if we do
// find a name and write it, return a pointer to it so the track
// writer knows what update (if any) to skip
{
    Alg_event_ptr e = NULL; // e is the result, default is NULL
    file << "#track " << n;
    const char *attr = symbol_table.insert_string(
                               n == 0 ? "seqnames" : "tracknames");
    // search for name in events with timestamp of 0
    for (int i = 0; i < events.length(); i++) {
        Alg_event_ptr ue = events[i];
        if (ue->time > 0) break;
        if (ue->is_update()) {
            Alg_update_ptr u = (Alg_update_ptr) ue;
            if (u->parameter.attr == attr) {
                file << " " << u->parameter.s;
                e = ue; // return the update event we found
                break;
            }
        }
    }
    file << endl; // end of line containing #track [<name>]
    return e; // return parameter event with name if one was found
}


void Alg_seq::write(ostream &file, bool in_secs, double offset)
{
    int i, j;
    if (in_secs) convert_to_seconds();
    else convert_to_beats();
    file << "#offset " << offset << endl;
    Alg_event_ptr update_to_skip = write_track_name(file, 0, track_list[0]);
    Alg_beats &beats = time_map->beats;
    for (i = 0; i < beats.len - 1; i++) {
        Alg_beat_ptr b = &(beats[i]);
        if (in_secs) {
            file << "T" << TIMFMT << b->time;
        } else {
            file << "TW" << TIMFMT << b->beat / 4;
        }
        double tempo = (beats[i + 1].beat - b->beat) /
                       (beats[i + 1].time - beats[i].time);
        file << " -tempor:" << GFMT << tempo * 60 << "\n";
    }
    if (time_map->last_tempo_flag) { // we have final tempo:
        Alg_beat_ptr b = &(beats[beats.len - 1]);
        if (in_secs) {
            file << "T" << TIMFMT << b->time;
        } else {
            file << "TW" << TIMFMT << b->beat / 4;
        }
        file << " -tempor:" << GFMT << time_map->last_tempo * 60.0 << "\n";
    }

    // write the time signatures
    for (i = 0; i < time_sig.length(); i++) {
        Alg_time_sig &ts = time_sig[i];
        double time = ts.beat;
        if (in_secs) {
            file << "T" << TIMFMT << time << " V- -timesig_numr:" << 
                    GFMT << ts.num << "\n";
            file << "T" << TIMFMT << time << " V- -timesig_denr:" << 
                    GFMT << ts.den << "\n";
        } else {
            double wholes = ts.beat / 4;
            file << "TW" << TIMFMT << wholes << " V- -timesig_numr:" <<
                    GFMT << ts.num << "\n";
            file << "TW" << TIMFMT << wholes << " V- -timesig_denr:" <<
                    GFMT << ts.den << "\n";
        }
    }

    for (j = 0; j < track_list.length(); j++) {
        Alg_events &notes = track_list[j];
        if (j != 0) update_to_skip = write_track_name(file, j, notes);
        // now write the notes at beat positions
        for (i = 0; i < notes.length(); i++) {
            Alg_event_ptr e = notes[i];
            // if we already wrote this event as a track or sequence name,
            // do not write it again
            if (e == update_to_skip) continue;
            double start = e->time;
            if (in_secs) {
                file << "T" << TIMFMT << start;
            } else {
                file << "TW" << TIMFMT << start / 4;
            }
            // write the channel as Vn or V-
            if (e->chan == -1) file << " V-";
            else file << " V" << e->chan;
            // write the note or update data
            if (e->is_note()) {
                Alg_note_ptr n = (Alg_note_ptr) e;
                double dur = n->dur;
                file << " K" << n->get_identifier() << 
                        " P" << GFMT << n->pitch;
                if (in_secs) {
                    file << " U" << TIMFMT << dur;
                } else {
                    file << " Q" << TIMFMT << dur;
                }
                file << " L" << GFMT << n->loud; 
                Alg_parameters_ptr p = n->parameters;
                while (p) {
                    parameter_print(file, &(p->parm));
                    p = p->next;
                }
            } else { // an update
                assert(e->is_update());
                Alg_update_ptr u = (Alg_update_ptr) e;
                if (u->get_identifier() != -1) {
                    file << " K" << u->get_identifier();
                }
                parameter_print(file, &(u->parameter));
            }
            file << "\n";
        }
    }
}

bool Alg_seq::write(const char *filename, double offset)
{
    ofstream file(filename);
    if (file.fail()) return false;
    write(file, units_are_seconds, offset);
    file.close();
    return true;
}
