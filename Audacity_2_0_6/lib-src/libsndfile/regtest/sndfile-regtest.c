/*
**	Copyright (C) 2005-2011 Erik de Castro Lopo
**
**	This program is free software; you can redistribute it and/or modify
**	it under the terms of the GNU General Public License as published by
**	the Free Software Foundation; either version 2 of the License, or
**	(at your option) any later version.
**
**	This program is distributed in the hope that it will be useful,
**	but WITHOUT ANY WARRANTY; without even the implied warranty of
**	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**	GNU General Public License for more details.
**
**	You should have received a copy of the GNU General Public License
**	along with this program; if not, write to the Free Software
**	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sndfile.h>

#if HAVE_SQLITE3

#include "regtest.h"

enum
{	OPT_ADD_FILE	= 0x0100,
	OPT_CREATE_DB	= 0x0200,
	OPT_DEL_ENTRY	= 0x0400,
	OPT_LIST_ALL	= 0x0800,
	OPT_TEST_ALL	= 0x1000,
	OPT_VERBOSE		= 0x2000
} ;

static void print_libsndfile_version (void) ;

int
main (int argc, char * argv [])
{	const char *db_name = "./.sndfile-regtest.db" ;
	REG_DB *reg_db ;
	int k, retval ;

	if (argc < 2)
	{	printf ("\nUsage message goes here.\n\n") ;
		exit (0) ;
		} ;

	if (argc == 2 && strcmp (argv [1], "--create-db") == 0)
		return db_create (db_name) ;

	reg_db = db_open (db_name) ;

	if (argc == 2)
	{	if (strcmp (argv [1], "--list-all") == 0)
			return db_list_all (reg_db) ;

		if (strcmp (argv [1], "--check-all") == 0)
		{	print_libsndfile_version () ;
			retval = db_check_all (reg_db) ;
			puts ("\nDone.\n") ;
			return retval ;
			} ;
		} ;

	if (argc == 3 && strcmp (argv [1], "--del-entry") == 0)
	{	db_del_entry (reg_db, argv [2]) ;
		db_close (reg_db) ;
		return 0 ;
		} ;

	if (strcmp (argv [1], "--check-file") == 0)
	{	print_libsndfile_version () ;

		for (k = 2 ; k < argc ; k++)
			db_check_file (reg_db, argv [k]) ;
		db_close (reg_db) ;
		return 0 ;
		} ;

	if (strcmp (argv [1], "--add-file") == 0)
	{	print_libsndfile_version () ;

		for (k = 2 ; k < argc ; k++)
			db_add_file (reg_db, argv [k]) ;
		db_close (reg_db) ;
		return 0 ;
		} ;

	printf ("\nError : unhandled command line args :") ;
	for (k = 1 ; k < argc ; k++)
		printf (" %s", argv [k]) ;
	puts ("\n") ;

	return 1 ;
} /* main */

static void
print_libsndfile_version (void)
{	char version [64] ;

	sf_command (NULL, SFC_GET_LIB_VERSION, version, sizeof (version)) ;
	printf ("\nsndfile-regtest : using %s\n\n", version) ;
} /* print_lib_version */

#else

int
main (void)
{
	puts ("\nThis program was not compiled with libsqlite3 and hence doesn't work.\n") ;

	return 0 ;
} /* main */

#endif

