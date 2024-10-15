/*:filterkit.h */

/*
 * FilterUp() - Applies a filter to a given sample when up-converting.
 * FilterUD() - Applies a filter to a given sample when up- or down-
 *                   converting.
 */

fast_float FilterUp(mem_float Imp[], mem_float ImpD[], int Nwing, 
                    boolean Interp, mem_float *Xp, double Ph, int Inc);

fast_float FilterUD(mem_float Imp[], mem_float ImpD[], int Nwing,
                    boolean Interp, mem_float *Xp, double Ph, int Inc,
                    double dhb);

