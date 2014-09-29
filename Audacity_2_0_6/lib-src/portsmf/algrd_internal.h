/* algread_internal.h -- interface between allegro.cpp and allegrord.cpp */

Alg_error alg_read(std::istream &file, Alg_seq_ptr new_seq, 
                   double *offset_ptr = NULL);

