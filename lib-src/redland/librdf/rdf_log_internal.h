/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_log_internal.h - Internal RDF logging definitions
 *
 * Copyright (C) 2004-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2004-2005, University of Bristol, UK http://www.bristol.ac.uk/
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


#ifndef LIBRDF_LOG_INTERNAL_H
#define LIBRDF_LOG_INTERNAL_H

#ifdef __cplusplus
extern "C" {
#endif

void librdf_log_simple(librdf_world* world, int code, librdf_log_level level, librdf_log_facility facility, void *locator, const char *message);
void librdf_log(librdf_world* world, int code, librdf_log_level level, librdf_log_facility facility, void *locator, const char *message, ...) REDLAND_PRINTF_FORMAT(6, 7);

void librdf_fatal(librdf_world* world, int facility, const char *file, int line, const char *function, const char *message);

void librdf_test_error(librdf_world* world, const char *message);
void librdf_test_warning(librdf_world* world, const char *message);

#ifdef __cplusplus
}
#endif

#endif
