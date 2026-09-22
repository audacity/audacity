#ifndef QM_DSP_PCA_H
#define QM_DSP_PCA_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 *  pca.h
 *
 *  Created by Mark Levy on 08/02/2006.
 *  Copyright 2006 Centre for Digital Music, Queen Mary, University of London.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
 *
 */

void pca_project(double** data, int n, int m, int ncomponents);

#ifdef __cplusplus
}
#endif


#endif

