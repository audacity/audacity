/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * redland.h - Redland RDF Application Framework main header
 *
 * Copyright (C) 2000-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2000-2004, University of Bristol, UK http://www.bristol.ac.uk/
 * 
 * This package is Free Software and part of Redland http://librdf.org/
 * 
 * It is licensed under the following three licenses as alternatives:
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 * 
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 * 
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * complete terms and further detail along with the license texts for
 * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
 * 
 * 
 */


#ifndef REDLAND_H
#define REDLAND_H

#ifdef __cplusplus
extern "C" {
#endif

#ifndef LIBRDF_OBJC_FRAMEWORK
/* Redland consists of */
/* raptor */
#include <raptor.h>
/* rasqal */
#include <rasqal.h>
/* librdf */
#include <librdf.h>
#else
#include <Redland/raptor.h>
#include <Redland/rasqal.h>
#include <Redland/librdf.h>
#endif

#ifdef __cplusplus
}
#endif

#endif
