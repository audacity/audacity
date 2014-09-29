/* Copyright 1989 Carnegie Mellon University */

boolean rec_init(boolean bender);
boolean rec_event(long *data, time_type time);
void rec_final(FILE *fp, boolean absflag);
void write_pitch(FILE *fp, int p);
