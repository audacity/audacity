
//#include "stdlib.h"
//#include "stdio.h"
//#include "memory.h"
//#include "assert.h"
#include <fstream>
#include "allegro.h"
#include "mfmidi.h"
#include "portmidi.h"
#include "seq2midi.h"
//#include "string.h"
//#include "strparse.h"

#ifdef WIN32
#include "crtdbg.h" // for memory allocation debugging
#endif

void midi_fail(char *msg)
{
    printf("Failure: %s\n", msg);
    exit(1);
}


void *midi_alloc(size_t s) { return malloc(s); }
void midi_free(void *a) { free(a); }


void print_help()
{
    printf("Usage: allegroplay [-m] [-a] <filename> [-n]\n");
    printf("    use -m for midifile, -a for allegro file.\n");
    printf("    .mid extension implies -m, .gro implies -a\n");
    printf("    use -n for non-interactive use (no prompts)\n");
    exit(-1);
}


int main(int argc, char *argv[])
{
    if (argc < 2) {
        print_help();
    }
    char *filename = NULL;
    bool midifile = false;
    bool allegrofile = false;
    bool interactive = true;
    int i = 1; // scan the command line
    while (i < argc) {
        if (argv[i][0] == '-') {
            if (argv[1][1] == 'm') midifile = true;
            else if (argv[i][1] == 'a') allegrofile = true;
            else if (argv[i][1] == 'n') interactive = false;
        } else {
            filename = argv[i];
        }
        i++;
    }
    if (!filename) {
        print_help();
    }

    if (!midifile && !allegrofile) {
        int len = strlen(filename);
        if (len < 4) print_help();    // no extension, need -m or -a
        char *ext = filename + len - 4;
        if (strcmp(ext, ".mid") == 0) midifile = true;
        else if (strcmp(ext, ".gro") == 0) allegrofile = true;
        else print_help();
    }
    Alg_seq seq(filename, midifile);

    int events = 0;
    for (i = 0; i < seq.tracks(); i++) {
        events += seq.track(i)->length();
    }
    if (interactive) {
        printf("%d tracks, %d events\n", seq.tracks(), events);
    }
    /* PLAY THE FILE VIA MIDI: */
    if (interactive) {
        printf("type return to play midi file: ");
        char input[80];
        fgets(input, 80, stdin);
    }
    seq_play(seq);

    return 0;
}
