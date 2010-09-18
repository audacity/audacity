/*
 *  hillclimb.h
 *  scorealign
 *
 *  Created by Roger Dannenberg on 10/20/07.
 *  Copyright 2007 by Roger B. Dannenberg. All rights reserved.
 *
 * USAGE:
 *    Subclass and define evaluate, a function of n parameters.
 * Set parameters, step_size, min, max, and n. Parameters must
 * be near a local maximum and step_size defines the grid that
 * is searched. Search will not go outside of min and max.
 * Call optimize()
 * retrieve optimized parameter values with get_parameters.
 * 
 */

// while optimizing, this function is called to report progress
typedef void (*Report_fn_ptr)(void *cookie, int iteration, double best);

class Hillclimb {
protected:
    double *parameters; // parameters to optimize
    double *step_size;  // step size for each parameter (these are 
        // provided by the user and remain fixed)
    double *min_param; // minimum parameter values
    double *max_param; // maximum parameter values
    int n; // number of parameters
public:
    Hillclimb() {
        parameters = step_size = min_param = max_param = NULL;
    }
    void setup(int n_);
    ~Hillclimb();
    void set_parameters(double *parameters_, double *step_size_, 
                        double *min_, double *max_, int n_);
    // retrieve parameters after optimization:
    double *get_parameters() { return parameters; }
    virtual double evaluate() = 0;
    double optimize(Report_fn_ptr report, void *cookie);
};


