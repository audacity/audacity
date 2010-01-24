/*
 *  hillclimb.cpp
 *  scorealign
 *
 *  Created by Roger Dannenberg on 10/20/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 * Hillclimb is an abstract class for optimization. It models problems where
 * you have a vector of parameters (stored as an array), a corresponding set
 * of step sizes, and a non-linear function. The function is a virtual
 * member function that subclasses must implement.
 *
 * The optimization algorithm is as follows:
 * An initial set of parameters and step sizes is given.
 *
 * Estimate the partial derivatives with respect to each parameter value
 * by taking a step along that dimension (use step sizes to determine
 * how far to go) and calling the evaluate virtual function.
 * Find the parameter that causes the maximum absolute change. If the
 * change is positive for that parameter, take the step along that
 * dimension. If the change is negative, take a negative step along that
 * dimension. 
 *
 * Repeat the previous paragraph as long as the result of evaluate is
 * increasing. When it stops, you are at the top of a hill, a local
 * maximum.
 */

#include "hillclimb.h"
#include "stdio.h"

#define HC_VERBOSE 0
#define V if (HC_VERBOSE)

void Hillclimb::set_parameters(double *p, double *ss, 
                               double *min_, double *max_, int plen)
{
    parameters = p;
    step_size = ss;
    min_param = min_;
    max_param = max_;
    n = plen;
}

/* this optimize assumes that the surface is smooth enought that if the
 * function decreases when parameter[i] increases, then the function will
 * increase when parameter[i] decreases. The alternative version does more
 * evaluation, but checks in both directions to find the best overall move.

double Hillclimb::optimize()
{
    double best = evaluate();
    while (true) {
        printf("best %g ", best);
        // eval partial derivatives
        int i;
        // variables to search for max partial derivative
        double max = 0; // max of |dy| so far
        int max_i; // index where max was found
        int max_sign = 1; // sign of dy
        double max_y; // value of evaluate() at max_i
        // now search over all parameters for max change
        for (i = 0; i < n; i++) {
            int sign = 1; // sign of derivative in the +step direction
            int step_direction = 1; // how to undo parameter variation
            parameters[i] += step_size[i];
            if (parameters[i] > max_param[i]) {
                // try stepping in the other direction
                parameters[i] -= step_size[i] * 2;
                sign = -1;
                step_direction = -1;
            }
            
            double y = evaluate();
            // restore parameter i
            parameters[i] -= step_size[i] * step_direction;
            
            double dy = y - best;
            if (dy < 0) {
                dy = -dy;
                sign = -sign;
            }
            // is this the best yet and legal move?
            double proposal = parameters[i] + step_size[i] * sign;
            if (dy > max && proposal <= max_param[i] && 
                proposal >= min_param[i]) {
                max = dy;
                max_i = i;
                max_y = y;
                max_sign = sign;
            }
        }
        // best move is parameter max_i in max_sign direction
        parameters[max_i] += step_size[max_i] * max_sign;
        printf("moved %d to %g", max_i, parameters[max_i]);
        // what's the value now? put it in max_y
        if (max_sign == -1) max_y = evaluate();
        printf(" to get %g (vs. best %g)\n", max_y, best);
        // otherwise, max_y already has the new value
        if (max_y <= best) { // no improvement, we're done
            parameters[max_i] -= step_size[max_i] * max_sign;
            printf("\nCompleted hillclimbing, best %g\n", best);
            return best;
        }
        // improvement because max_y higher than best:
        best = max_y;
    }
}
*/

double Hillclimb::optimize()
{
    double best = evaluate();
    while (true) {
        V printf("best %g ", best);
        // eval partial derivatives
        int i;
        // variables to search for max partial derivative
        double max_y = best; // max of evaluate() so far
        int max_i; // index where best max was found
        double max_parameter; // the good parameter value for max_i
        // now search over all parameters for best improvement
        for (i = 0; i < n; i++) {
            V printf("optimize at %d param %g ", i, parameters[i]);
            double save_param = parameters[i];
            parameters[i] = save_param + step_size[i];
            if (parameters[i] <= max_param[i]) {
                double y = evaluate();
                V printf("up->%g ", y);
                if (y > max_y) {
                    V printf("NEW MAX! ");
                    max_y = y;
                    max_i = i;
                    max_parameter = parameters[i];
                }
            }
            parameters[i] = save_param - step_size[i];
            if (parameters[i] >= min_param[i]) {
                double y = evaluate();
                V printf("dn->%g ", y);
                if (y > max_y) {
                    V printf("NEW MAX! ");
                    max_y = y;
                    max_i = i;
                    max_parameter = parameters[i];
                }
            }
            parameters[i] = save_param;
            V printf("\n");
        }
        if (max_y <= best) { // no improvement, we're done
            V printf("\nCompleted hillclimbing, best %g\n", best);
            return best;
        }
        // improvement because max_y higher than best:
        parameters[max_i] = max_parameter;
        best = max_y;
    }
}


