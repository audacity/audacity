/* allegroconvert.cpp -- convert from allegro to standard midi files */

/* CHANGE LOG
04 apr 03 -- added options to remove tempo track, retaining either 
             the original beats or original timing (RBD)
*/

#include <fstream>
#include "allegro.h"


using namespace std;

void midi_fail(char *msg)
{
    printf("Failure: %s\n", msg);
    exit(1);
}


void *midi_alloc(size_t s) { return malloc(s); }
void midi_free(void *a) { free(a); }


void print_help()
{
    printf("%s%s%s%s%s%s%s%s",
           "Usage: allegroconvert [-m] [-a] [-t tempo] [-f] <filename>\n",
           "    Use -m for midifile->allegro, -a for allegro->midifile.\n",
           "    .mid extension implies -m, .gro implies -a\n",
           "    If tempo (a float) is specified after -t, the tempo track\n",
           "    is deleted and replaced by a fixed tempo.\n",
           "    If -f (flatten) is specified, output a flat tempo=60,\n",
           "    preserving the original timing of all midi events.\n",
           "    Do not use both -t and -f.\n");
    exit(-1);
}


// process -- perform tempo changing and flattening operations
//
void process(Alg_seq_ptr seq, bool tempo_flag, double tempo, 
             bool flatten_flag)
{
    // the tempo changing operation replaces the tempo track with
    // a fixed tempo. This changes the timing of the result.
    if (tempo_flag) {
        seq->convert_to_beats(); // preserve beats
    } else if (flatten_flag) {
        seq->convert_to_seconds(); // preserve timing
    } else return;
    // the following finishes both tempo and flatten processing...
    seq->get_time_map()->beats.len = 1; // remove contents of tempo map
    seq->get_time_map()->last_tempo = tempo / 60.0; // set the new fixed tempo
     // (allegro uses beats/second so divide bpm by 60)
    seq->get_time_map()->last_tempo_flag = true;
}


int main(int argc, char *argv[])
{
    if (argc < 2) {
        print_help();
    }
    char *filename = argv[argc - 1];
    char outfilename[256];
    bool midifile = false;
    bool allegrofile = false;
    bool flatten_flag = false;
    double tempo = 60.0; // default is used for -f (flatten)
    bool tempo_flag = false;
    char *ext = NULL;

    int i = 1;
    while (i < argc - 1) {
        if (argv[i][0] != '-') print_help();
        if (argv[i][1] == 'm') midifile = true;
        else if (argv[i][1] == 'a') allegrofile = true;
        else if (argv[i][1] == 't') {
            i++;
            if (i >= argc - 1) print_help(); // expected tempo
            tempo = atof(argv[i]);
            tempo_flag = true;
        } else if (argv[i][1] == 'f') {
            flatten_flag = true;
        } else print_help();
        i++;
    }
    // Do not use both -t and -f:
    if (tempo_flag & flatten_flag) print_help();

    int len = strlen(filename);
    if (!midifile && !allegrofile) {
        if (len < 4) print_help();    // no extension, need -m or -a
        ext = filename + len - 4;
        if (strcmp(ext, ".mid") == 0) midifile = true;
        else if (strcmp(ext, ".gro") == 0) allegrofile = true;
        else print_help();
    } else if (len > 4) {
        ext = filename + len - 4;
    } 
    Alg_seq_ptr seq;
    strcpy(outfilename, filename);
    if (midifile) {
        seq = new Alg_seq(filename, true);
        process(seq, tempo_flag, tempo, flatten_flag);
        if (ext && strcmp(ext, ".mid") == 0) {
            ext = outfilename + strlen(outfilename) - 4;
        } else {
            ext = outfilename + strlen(outfilename);
        }
        strcpy(ext, ".gro");
        seq->write(outfilename);
    } else if (allegrofile) {
        seq = new Alg_seq(filename, false);
        process(seq, tempo_flag, tempo, flatten_flag);
        if (ext && strcmp(ext, ".gro") == 0) {
            ext = outfilename + strlen(outfilename) - 4;
        } else {
            ext = outfilename + strlen(outfilename);
        }
        strcpy(ext, ".mid");
        seq->smf_write(outfilename);
    }

    int events = 0;
    for (i = 0; i < seq->track_list.length(); i++) {
        events += seq->track_list[i].length();
    }
    printf("%ld tracks, %d events\n", seq->track_list.length(), events);
    printf("wrote %s\n", outfilename);
    
    /* DELETE THE DATA */
    delete seq;

    return 0;
}
