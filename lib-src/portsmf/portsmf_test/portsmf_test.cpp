#include "memory.h"
#include <iostream>
#include <fstream>
using namespace std;
#include "allegro.h"

void seq_print(Alg_seq_ptr seq)
{
    for (int i = 0; i < seq->tracks(); i++) {
        printf("TRACK %d\n", i);
        for (int j = 0; j < seq->track(i)->length(); j++) {
            (*seq->track(i))[j]->show();
        }
    }
}


Alg_seq_ptr make_simple_score()
{
    Alg_seq_ptr seq = new Alg_seq;
    Alg_update_ptr update = seq->create_update(0, 0, -1);
    update->set_real_value("attackr", 0.007);
    seq->add_event(update, 0);

    Alg_note_ptr note = seq->create_note(0, 0, 63, 63, 105, 0.8);
    // at 100bpm, dur will be 1.33 beats
    seq->add_event(note, 0);

    // at 100bpm, time = W0.33 beats, dur = 0.66 beats

    note = seq->create_note(0.8, 0, 65, 65, 107, 0.4);
    seq->add_event(note, 0);
    //seq_print(seq);
    return seq;
}

void test1()
{
    Alg_seq_ptr seq = make_simple_score();
    seq->write(cout, false);
}


void test2()
{
    Alg_seq_ptr seq = make_simple_score();
    seq->convert_to_seconds();
    seq->get_time_map()->last_tempo = 120.0 / 60.0; // 120bpm
    seq->time_sig.insert(0, 3, 4);
    seq->write(cout, false);
}


Alg_seq_ptr make_score()
{
    Alg_seq_ptr seq = make_simple_score();
    // now we have notes at 0 and 0.8 seconds
    // add new ones at 1.2 and 1.8 seconds
    Alg_note_ptr note = seq->create_note(1.2, 0, 67, 67, 107, 0.6);
    seq->add_event(note, 0);

    note = seq->create_note(1.8, 0, 69, 69, 109, 0.2);
    seq->add_event(note, 0);

    // make the tempo such that notes start at 0, 1, 2, 3 beats
    seq->convert_to_seconds(); // hold time, change beats
    seq->get_time_map()->insert_beat(0.8, 1.0);
    seq->get_time_map()->insert_beat(1.2, 2.0);
    seq->get_time_map()->insert_beat(1.8, 3.0);
    seq->get_time_map()->insert_beat(2.0, 4.0);
    return seq;
}


void test3()
{
    Alg_seq_ptr seq = make_score();
    // show the tempo map:
    seq->get_time_map()->show();

    // write the sequence
    seq->write(cout, false); // in beats
	/* score should include
	    63 @ m 0 = 0s
		65 @ m 0.25 = 0.8s
		67 @ m 0.5 = 1.2s
		69 @ m 0.75 = 1.8s
	 */
    seq->write(cout, true);  // in seconds
}


void test4()
{
    Alg_seq_ptr seq = make_score();
    seq->write(cout, false);
    ofstream file("test4.mid", ios::out | ios::binary);
    if (!file) {
        printf("could not open test4.mid for writing");
        return;
    }
    seq->smf_write(file);
    file.close();
    delete seq;
    ifstream ifile("test4.mid", ios::binary | ios::in);
    if (ifile.fail()) {
        printf("could not open test4.mid for reading");
        return;
    }
    seq = new Alg_seq(ifile, true); // read midi file
    ifile.close();
    seq->write(cout, false);
}


void test5()
{
    Alg_seq_ptr seq = make_score();
    ofstream file("test5.gro");
    if (file.is_open()) {
    printf("could not open test5.gro for writing");
    return;
    }
    seq->write(cout, true);
    seq->write(file, true);
    file.close();
    delete seq;
    ifstream ifile("test5.gro");
    if (ifile.fail()) {
        printf("could not open test5.gro for reading");
        return;
    }
    seq = new Alg_seq(ifile, false); // read midi file
    ifile.close();
    printf("SCORE AFTER READING\n");
    seq->write(cout, true);
}

void test6()
{
    Alg_seq_ptr seq = make_score();
    seq->write(cout, true);
	long index = seq->seek_time(1.0, 0);
	printf("seq->get_units_are_seconds() = %d\n", seq->get_units_are_seconds());
	printf("seq->seek_time(1.0, 0) returns %ld\n", index);
	printf("note is:\n");
	if ((*seq->track(0))[index]->is_note()) {
		((Alg_note_ptr) (*(seq->track(0)))[index])->show();
	} else {
		printf("event is not a note\n");
	}
}



/*
# test6()
# should print
# seq.units_are_seconds t
# seq_time(1,0) returns 5
# note is:
# Alg_note: time 1.2, chan 0, dur 0.6, key 67, pitch 67, loud 107, attributes nil
*/

void test7()
{
    Alg_seq_ptr seq = make_score();
    seq->write(cout, false);
    // insert time signature for one bar each of 4/4, 3/4, 5/4, 4/4 and 
    // call quarter_to_measure() on various times
	seq->convert_to_beats(); // make sure we're talking beats
	seq->time_sig.insert(0, 4, 4);
	seq->time_sig.insert(4, 3, 4);
	seq->time_sig.insert(7, 5, 4);
	seq->time_sig.insert(12, 4, 4);
	double qtable[] = {0, 1, 3.999, 4, 4.001, 5, 7, 7.5, 12, 17, 17.25};
	for (int i = 0; i < 11; i++) {
	    long m;
		double b, n, d;
	    seq->beat_to_measure(qtable[i], &m, &b, &n, &d);
		printf("%g -> %ld + %g (%g/%g)\n", qtable[i], m, b, n, d);
	}
}

/*
# test7()
# should print:
# 0 -> [0, 0, 4, 4]
# 1 -> [0, 1, 4, 4]
# 3.99999 -> [0, 3.99999, 4, 4]
# 4 -> [1, 0, 3, 4]
# 4.00001 -> [1, 1e-005, 3, 4]
# 5 -> [1, 1, 3, 4]
# 7 -> [2, 0, 5, 4]
# 7.5 -> [2, 0.5, 5, 4]
# 12 -> [3, 0, 4, 4]
# 17 -> [4, 1, 4, 4]
# 17.25 -> [4, 1.25, 4, 4]
*/

void test8()
{
    Alg_seq_ptr seq = make_score();
	seq->convert_to_beats(); // just to be sure
    seq->write(cout, false);
	seq->get_time_map()->beats.len = 1;
	seq->get_time_map()->show();
	seq->write(cout, false);
	printf("insert 1 1 returns %d\n", seq->insert_beat(1.0, 1.0));
	seq->get_time_map()->show();
	printf("insert 5 3 returns %d\n", seq->insert_beat(5.0, 3.0));
	seq->get_time_map()->show();
	seq->write(cout, false);
	seq->write(cout, true);
}

/*
# should indicate notes starting at 0, .25, .5, and .75 whole notes,
# and as seconds, starting at 0, 1, 3, and 5 seconds
*/

void test9()
{
    Alg_seq_ptr seq = make_score();
	seq->convert_to_beats(); // just to be sure
    seq->write(cout, false);
	seq->get_time_map()->beats.len = 1;
	seq->get_time_map()->show();
	seq->write(cout, false);
	printf("insert 30 1 returns %d\n", seq->insert_tempo(30.0, 1.0));
	seq->get_time_map()->show();
	printf("insert 15 2 returns %d\n", seq->insert_tempo(15.0, 2.0));
	seq->get_time_map()->show();
	seq->write(cout, false);
	seq->write(cout, true);
}

/*
# show show 
#  TW0.25 -tempor:30
#  TW0.5 -tempor:15
# in the beat-based output, and beats at 0, 0.6, 2.6, 6.6 in the 
# time-based output
*/

void test10()
{
    Alg_seq_ptr seq = make_score();
	seq->convert_to_beats(); // just to be sure
	seq->set_time_sig(0.0, 4, 4);
	seq->set_time_sig(4.0, 3, 4);
	seq->set_time_sig(7.0, 5, 4);
	seq->set_time_sig(12.0, 4, 4);
	double btable[] = { 0.0, 4.0, 7.0, 12.0 };
	for (int i = 0; i < 4; i++) {
	    long m;
		double b, n, d;
	    seq->beat_to_measure(btable[i], &m, &b, &n, &d);
		printf("%g -> %ld + %g (%g/%g)\n", btable[i], m, b, n, d);
	}
}

/*
# result should show increasing measure numbers:
#
# [0, 0, 4, 4]
# [1, 0, 3, 4]
# [2, 0, 5, 4]
# [3, 0, 4, 4]
*/

void test11() // add track
{
    Alg_seq_ptr seq = make_score();
	seq->convert_to_beats();
    seq->add_track(3);
	Alg_note_ptr note = seq->create_note(2.0, 0, 50, 50, 100, 1.0);
	seq->add_event(note, 3);
	seq->write(cout, false);
	seq->write(cout, true);
}
/*
# should put note in track3 at beat TW0.5 (same as T1.2). Make sure
# in the beat representation, note has duration Q1.
*/

void test12() // test merge_tracks()
{
    Alg_seq_ptr seq = make_score();
	seq->convert_to_beats(); // just to be sure
    seq->add_track(3);
	Alg_note_ptr note = seq->create_note(2.0, 0, 50, 50, 100, 1.0);
	seq->add_event(note, 3);
	seq->write(cout, false);
	seq->merge_tracks();
	seq->write(cout, false);
}


/*
# test12()
#
# tracks 1, 2, 3 go away, and the note that was in track 3 appears in track 0
*/

void test13()
{
    Alg_seq_ptr seq = make_score();
	seq->convert_to_beats(); // just to be sure
    seq->write(cout, false);
	seq->set_tempo(120.0, 1, 3);
	seq->write(cout, false);
}


/*
# test13()
# 
# tempo should be transformed to:
#
# TW0 -tempor:75
# TW0.25 -tempor:120
# TW0.75 -tempor:300
# TW1 -tempor:100
*/

void test14()
{
    Alg_seq_ptr seq = make_score();
	seq->convert_to_beats(); // just to be sure
    seq->write(cout, false);
	seq->set_start_time((*(seq->track(0)))[3], 0.5);
	seq->write(cout, false);
}

/*
# test14()
#
# should place pitch 67 note at time TW0.125
*/

void test15() // test copy() routine
{
    Alg_seq_ptr seq = make_score();
	seq->convert_to_beats(); // just to be sure
	Alg_event_ptr event = (*(seq->track(0)))[3];
	event->show();
	event = seq->copy_event(event);
	event->show();
	event = (*(seq->track(0)))[0];
	event->show();
    event = seq->copy_event(event);
	event->show();
}

/*
# test15()
#
# should print two identical notes, (orginal and a copy)
# and two identical updates
*/

void test16() // test cut_from_track
{
    Alg_seq_ptr seq = make_score();
	seq->convert_to_beats(); // just to be sure
    seq->write(cout, false);
    Alg_track_ptr track = seq->cut_from_track(0, 2, 1, true);
	seq->track_list.append(track);
	seq->write(cout, false);
}

/*
# test16()
#
# should print original score and modified score
# modified score has:
#   K63 at TW0
#   K65 at TW0.25
#   K69 at TW0.5
# and on track 1 has
#   K67 at TW0
*/

void test17() // test cut
{
    Alg_seq_ptr seq = make_score();
	seq->convert_to_beats(); // just to be sure
	seq->set_time_sig(0, 2, 4);
	seq->set_time_sig(2, 2, 8);
	seq->set_time_sig(3, 4, 4);
    seq->write(cout, false);
    Alg_seq_ptr new_seq = seq->cut(2, 1, true)->to_alg_seq();
	printf("after cut, original is\n");
	seq->write(cout, false);
	printf("after cut, new is\n");
	new_seq->write(cout, false);
}

/*
# test17()
#
# should see seq with everything, then seq with
# 2/4 at 0, 4/4 at TW0.5
# and K63 at TW0, K65 at TW0.25, and K69 at TW0.5,
# then in last seq,
# 2/8 at 0, and K67 at 0
*/


void test18() // test copy_interval_from_track
{
    Alg_seq_ptr seq = make_score();
	seq->convert_to_beats(); // just to be sure
    Alg_track_ptr track = seq->track_list[0].copy(1, 2, false);
	seq->track_list.append(track);
	seq->write(cout, false);
}
/*
# test18()
# 
# should see track 0 from beat 1 to 3, i.e.
# K65 at T0, K67 at TW0.25
*/

void test19() // test copy
{
	Alg_seq_ptr seq = make_score();
    seq->convert_to_beats();
    seq->set_time_sig(0, 2, 4);
    seq->set_time_sig(2, 2, 8);
    seq->set_time_sig(3, 4, 4);
    seq->set_tempo(120.0, 1, 3);
    seq->write(cout, false);
    printf("\n");
    Alg_seq_ptr new_seq = seq->copy(2, 1, true);
    printf("----------after cut, original---------\n");
    seq->write(cout, false);
    printf("----------after cut, new--------------\n");
    new_seq->write(cout, false);
}

/*
# test19()
#
# should output original, original again (with no changes),
# then a sequence with tempo 120 and k67 at TW0
*/

void test20() // TEST 20 -- test track paste
{
	Alg_seq_ptr seq = make_score();
    seq->convert_to_beats();
    Alg_track &track = *(seq->track(0)->copy(1, 2, false));
    printf("-------seq:---------\n");
    seq->write(cout, false);
    printf("-------track to paste ---------:\n");
    Alg_seq_ptr new_seq = new Alg_seq(track);
    new_seq->write(cout, false);
    seq->track(0)->paste(2, &track);
    printf("----------after paste, seq is:---------\n");
    seq->write(cout, false);
}

/*
# test20()
#
# final printed sequence should be quarter notes as follows:
# K63, K65, K65, K67, K67, K69 (last at TW1.25)
*/

void test21() // TEST 21 -- test paste
{
    Alg_seq_ptr seq = make_score();
    seq->convert_to_beats();
    seq->set_time_sig(0, 2, 4);
    seq->set_time_sig(2, 2, 8);
    seq->set_time_sig(3, 4, 4);
    seq->set_tempo(120.0, 1, 3);
    printf("-------seq original:---------\n");
    seq->write(cout, false);
    Alg_seq_ptr copy_buffer = seq->copy(1, 2, false);
    printf("-------what to paste:---------\n");
    copy_buffer->write(cout, false);
    seq->paste(2, copy_buffer);
    printf("-------after paste: ---------:\n");
    seq->write(cout, false);
}

/*
# test21()
#
# should print like in test20, but with tempo as follows:
#  TW0: 75
#  TW0.25: 120
#  TW0.5:  120
#  TW1:    120
#  TW1.25  300
#  TW1.5:  100
# and the time signatures should be:
#  TW0: 2/4
#  TW0.5: 2/4
#  TW0.75: 2/8
#  TW1: 2/8
#  TW1.25: 4/4
*/

void test22() // TEST 22 -- test merge_to_track
{
    Alg_seq_ptr seq = make_score();
    seq->convert_to_beats();
    Alg_track_ptr track = seq->track(0)->copy(1, 2, false);
    printf("seq\n");
    seq->write(cout, false);
    printf("track:\n");
    Alg_seq_ptr new_seq = new Alg_seq(track);
    new_seq->write(cout, false);
    seq->merge(2, new_seq);
    printf("seq after merge\n");
    seq->write(cout, false);
}

/*
#test22()
#
# should output as follows:
# 0     K63
# 0.25  K65
# 0.5   K67 and K65
# 0.75  K69 and K67
*/


void test23() // test merge
{
    Alg_seq_ptr seq = make_score();
    seq->convert_to_beats();
    Alg_track_ptr track = seq->track(0)->copy(1, 2, false);
    printf("seq:\n");
    seq->write(cout, false);
    printf("new_seq:\n");
    Alg_seq_ptr new_seq = new Alg_seq(track);
    new_seq->write(cout, false);
    seq->merge(2, new_seq);
    printf("seq after merge:\n");
    seq->write(cout, false);
}

/*
# test23()
#
# should output same as test22
*/

void test24() // test silence_track
{
    Alg_seq_ptr seq = make_score();
    seq->convert_to_beats();
    seq->silence_track(0, 1, 2, true);
    seq->write(cout, false);
}

/*
#test24()
#
# should print two notes: K63 at T0 and K69 at TW0.75
*/

void test25() // test silence_events
{
    Alg_seq_ptr seq = make_score();
    seq->convert_to_beats();
    // copy everything from track 0
    Alg_track_ptr tr = seq->copy_track(0, 0, 10, false);
    // and make the new data be track 1
    seq->track_list.append(tr);
    seq->silence(1, 2, true);
    seq->write(cout, false);
}


/*
#test25()
#
# should print track 0 like test24, and track 1 like track 0
*/

void test26() // test clear_track
{
    Alg_seq_ptr seq = make_score();
    seq->convert_to_beats();
    seq->clear_track(0, 1, 2, true);
    seq->write(cout, false);
}


/*
# test26()
#
# should print two notes: K63 at T0 and K69 at TW0.25
*/

void test27() // test clear
{
    Alg_seq_ptr seq = make_score();
    seq->convert_to_beats();
    // copy everything from track 0
    Alg_track_ptr track = seq->copy_track(0, 0, 10, false);
    // and make the new data be track 1
    seq->track_list.append(track);
    seq->clear(1, 2, true);
    seq->write(cout, false);
}

/*
#
# should print track 0 like test26, and track 1 like track 0
*/


void test28() // test insert_silence_in_track
{
    Alg_seq_ptr seq = make_score();
    printf("BEFORE:\n");
    seq->write(cout, false);
    seq->convert_to_beats();
    seq->insert_silence_in_track(0, 1, 2);
    printf("AFTER:\n");
    seq->write(cout, false);
}

/*
# test28()
#
# should print this:
#  TW0 K63
#  TW0.75 K65
#  TW1    K67
#  TW1.25 K69
*/

void test29() // test insert_silence
{
    Alg_seq_ptr seq = make_score();
    seq->convert_to_beats();
    seq->set_time_sig(0, 2, 4);
    seq->set_time_sig(2, 2, 8);
    printf("BEFORE:\n");
    seq->write(cout, false);
    seq->insert_silence(1, 2);
    printf("AFTER:\n");
    seq->write(cout, false);
}


/*
# test29()
#
# should print
#  TW0 -tempor:75
#  TW0.25 -tempor:150
#  TW1 -tempor:100
#  TW1.25 -tempor:300
#  TW1.5 -tempor:100
#  TW0  ... (2/4 time) ...
#  TW1  ... (2/8 time) ...
#  TW0 V0 -attackr:0.007
#  TW0 V0 K63 P63 Q1 L105
#  TW0.75 V0 K65 P65 Q1 L107
#  TW1 V0 K67 P67 Q1 L107
#  TW1.25 V0 K69 P69 Q1 L109
*/

void test30() // test find on a track
{
    Alg_seq_ptr seq = make_score();
    seq->convert_to_beats();
    Alg_event_list_ptr track = seq->track(0)->find(0, 2, false, 1 << 0, 
                           1 << ALG_NOTE);
    seq->track_list.append(new Alg_track(*track, seq->get_time_map(), 
                     seq->get_units_are_seconds()));
    seq->write(cout, false);
}


/*
# test30()
# 
# sets track 1 to TW0 K63, TW0.25 K65
*/


void test31() // test find_in_track
{
    Alg_seq_ptr seq = make_score();
    seq->convert_to_beats();
    Alg_event_list_ptr track = seq->find_in_track(0, 0, 2, false, 1 << 0, 
                          1 << ALG_NOTE);
    seq->track_list.append(new Alg_track(*track, seq->get_time_map(), 
                     seq->get_units_are_seconds()));
    seq->write(cout, false);
}

void test32() // serialize big midi file and unserialize
{
    ifstream file("jsb-wtc-c-f.mid", ios::in | ios::binary);
    if (!file) {
        printf("Error: Could not open jsb-wtc-c-f.mid for reading\n");
        return;
    }
    Alg_seq_ptr seq = new Alg_seq(file, true); // read midi file
    file.close();
    
    ofstream ofile("bigseq1.alg", ios::out | ios::binary);
    printf("after reading, real_dur is %g, beat_dur is %g\n",
       seq->get_real_dur(), seq->get_beat_dur());
    seq->write(ofile, true);
    ofile.close();

    void *buffer;
    long bytes;
    seq->serialize(&buffer, &bytes);
    printf("Serialized %ld bytes\n", bytes);
    Alg_seq_ptr new_seq = (Alg_seq_ptr) seq->unserialize(buffer, bytes);
    ofstream sfile("bigseq2.alg", ios::out | ios::binary);
    new_seq->write(sfile, true);
    sfile.close();
}

void test33() // cut and inspect some notes
{
    ifstream file("be-ps-05.mid", ios::in | ios::binary);
    if (!file.is_open()) {
        printf("Error: Could not open be-ps-05.mid for reading\n");
        return;
    }
    Alg_seq_ptr seq = new Alg_seq(file, true); // read midi file
    file.close();
    seq->convert_to_seconds();
    
    Alg_time_map_ptr tm = seq->get_time_map();
    printf("time map %p before\n", tm);
    // tm->show();
    printf("timestamp before %.15g\n", tm->beats[tm->locate_time(3.74)].time);
    ofstream ofile("before-33.alg", ios::out | ios::binary);
    seq->write(ofile, true);
    ofile.close();

    Alg_seq_ptr cut = seq->cut(0.0, 3.0, false);

    tm = seq->get_time_map();
    printf("timemap %p after\n", tm);
    // tm->show();
    printf("timestamp after %.15g\n", tm->beats[tm->locate_time(0.74)].time);
    ofile.open("after-33.alg",  ios::out | ios::binary);
    seq->write(ofile, true);
    ofile.close();
}


int main()
{
    test33();
    printf("Test 33 done, type return to exit\n");
    getchar();
}
