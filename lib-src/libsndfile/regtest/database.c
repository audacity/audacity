/*
**	Copyright (C) 2005 Erik de Castro Lopo
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
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>

#include <sndfile.h>

#include "regtest.h"

#if HAVE_SQLITE3

#include <sqlite3.h>

typedef struct
{	sqlite3 *sql ;

	int count ;
	int ekey_max ;

	/* Filename and pathname for file. */
	char filename [256] ;
	char pathname [512] ;

	/* Storage for createding SQL commands. Must be larger than logbuf below. */
	char cmdbuf [1 << 15] ;

	/* Storage for log buffer retrieved from SNDFILE* .*/
	char logbuf [1 << 14] ;

} REGTEST_DB ;

/* In checksum.c */
int calc_checksum (SNDFILE * file, const SF_INFO * info) ;

static void get_filename_pathname (REGTEST_DB * db, const char *filepath) ;
static void single_quote_replace (char * buf) ;

static int get_ekey_from_filename (REGTEST_DB * db, const char *filepath) ;
static int get_filename_pathname_by_ekey (REGTEST_DB * db, int ekey) ;
static int check_file_by_ekey (REGTEST_DB * db, int ekey) ;

static int count_callback (REGTEST_DB * db, int argc, char **argv, char **colname) ;
static int ekey_max_callback (REGTEST_DB * db, int argc, char **argv, char **colname) ;
static int callback (void *unused, int argc, char **argv, char **colname) ;

REG_DB *
db_open (const char * db_name)
{	REGTEST_DB * db ;
	int err ;

	if ((db = malloc (sizeof (REGTEST_DB))) == NULL)
	{	perror ("malloc") ;
		exit (1) ;
		} ;

	if ((err = sqlite3_open (db_name, &(db->sql))) != 0)
	{	printf ("Can't open database: %s\n", sqlite3_errmsg (db->sql)) ;
        sqlite3_close (db->sql) ;
		free (db) ;
		exit (1) ;
		} ;

	return (REG_DB *) db ;
} /* db_open */

int
db_create (const char * db_name)
{	REGTEST_DB * db ;
	const char *cmd ;
	char * errmsg = NULL ;
	int err ;

	db = (REGTEST_DB *) db_open (db_name) ;

	cmd = "create table sndfile (ekey INTEGER PRIMARY KEY,"
			"fname VARCHAR(1),"
			"fpath VARCHAR(1),"
			"srate INTEGER,"
			"frames VARCHAR(1),"
			"channels INTEGER,"
			"format VARCHAR(1),"
			"checksum VARCHAR(1),"
			"logbuf VARCHAR(1)"
			");" ;

	err = sqlite3_exec (db->sql, cmd, callback, 0, &errmsg) ;
	if (err != SQLITE_OK)
		printf ("Line %d : SQL error: %s\n", __LINE__, errmsg) ;

	sqlite3_close (db->sql) ;
	free (db) ;

	return 0 ;
} /* db_create */

int
db_close (REG_DB * db_handle)
{	REGTEST_DB * db ;

	db = (REGTEST_DB *) db_handle ;

	sqlite3_close (db->sql) ;
	free (db) ;

	return 0 ;
} /* db_close */

/*==============================================================================
*/

int
db_file_exists (REG_DB * db_handle, const char * filename)
{	REGTEST_DB * db ;
	const char * cptr ;
	char * errmsg ;
	int err ;

	db = (REGTEST_DB *) db_handle ;

	if ((cptr = strrchr (filename, '/')) != NULL)
		filename = cptr + 1 ;

	snprintf (db->cmdbuf, sizeof (db->cmdbuf), "select fname from sndfile where fname='%s'", filename) ;

	db->count = 0 ;
	err = sqlite3_exec (db->sql, db->cmdbuf, (sqlite3_callback) count_callback, db, &errmsg) ;
	if (db->count == 1)
		return 1 ;

	return 0 ;
} /* db_file_exists */

int
db_add_file (REG_DB * db_handle, const char * filepath)
{	REGTEST_DB * db ;
	SNDFILE * sndfile ;
	SF_INFO info ;
	char * errmsg ;
	int err, checksum ;

	db = (REGTEST_DB *) db_handle ;

	get_filename_pathname (db, filepath) ;

	if (db_file_exists (db_handle, filepath))
	{	printf ("    %s : already in database\n", db->filename) ;
		return 0 ;
		} ;

	memset (&info, 0, sizeof (info)) ;
	sndfile = sf_open (db->pathname, SFM_READ, &info) ;
	sf_command (sndfile, SFC_GET_LOG_INFO, db->logbuf, sizeof (db->logbuf)) ;
	checksum = (sndfile == NULL) ? 0 : calc_checksum (sndfile, &info) ;
	sf_close (sndfile) ;

	if (sndfile == NULL)
	{	printf ("    %s : could not open : %s\n", db->filename, sf_strerror (NULL)) ;
		puts (db->logbuf) ;
		return 1 ;
		} ;

	single_quote_replace (db->logbuf) ;

	snprintf (db->cmdbuf, sizeof (db->cmdbuf), "insert into sndfile "
		"(fname, fpath, srate, frames, channels, format, checksum, logbuf) values"
		"('%s','%s',%d,'%ld', %d, '0x%08x', '0x%08x', '%s');",
		db->filename, db->pathname, info.samplerate, (long) info.frames, info.channels, info.format, checksum, db->logbuf) ;

	if (strlen (db->cmdbuf) >= sizeof (db->cmdbuf) - 1)
	{	printf ("strlen (db->cmdbuf) too long.\n") ;
		exit (1) ;
		} ;

	err = sqlite3_exec (db->sql, db->cmdbuf, callback, 0, &errmsg) ;
	if (err != SQLITE_OK)
	{	printf ("Line %d : SQL error: %s\n", __LINE__, errmsg) ;
		puts (db->cmdbuf) ;
		} ;

	return 0 ;
} /* db_add_file */

int
db_check_file (REG_DB * db_handle, const char * filepath)
{	REGTEST_DB * db ;
	int ekey ;

	if (db_file_exists (db_handle, filepath) == 0)
	{	printf ("\nFile not in database.\n\n") ;
		exit (0) ;
		} ;

	db = (REGTEST_DB *) db_handle ;

	ekey = get_ekey_from_filename (db, filepath) ;

	return check_file_by_ekey (db, ekey) ;
} /* db_check_file */

/*==============================================================================
*/

int
db_check_all (REG_DB * db_handle)
{	REGTEST_DB * db ;
	char * errmsg ;
	int err, ekey ;

	db = (REGTEST_DB *) db_handle ;

	db->ekey_max = 0 ;

	snprintf (db->cmdbuf, sizeof (db->cmdbuf), "select ekey from sndfile") ;

	err = sqlite3_exec (db->sql, db->cmdbuf, (sqlite3_callback) ekey_max_callback, db, &errmsg) ;
	if (err != SQLITE_OK)
	{	printf ("Line %d : SQL error: %s\n", __LINE__, errmsg) ;
		puts (db->cmdbuf) ;
		} ;

	for (ekey = 1 ; ekey <= db->ekey_max ; ekey++)
		if (get_filename_pathname_by_ekey (db, ekey) != 0)
			check_file_by_ekey (db, ekey) ;

	return 0 ;
} /* db_check_all */


int
db_list_all (REG_DB * db_handle)
{
	printf ("%s : %p\n", __func__, db_handle) ;
	return 0 ;
} /* db_list_all */

int
db_del_entry (REG_DB * db_handle, const char * entry)
{
	printf ("%s : %p %s\n", __func__, db_handle, entry) ;
	return 0 ;
} /* db_del_entry */

/*==============================================================================
*/

static int
get_ekey_from_filename (REGTEST_DB * db, const char *filepath)
{	char * errmsg, **result ;
	int err, ekey = 0, rows, cols ;

	get_filename_pathname (db, filepath) ;

	snprintf (db->cmdbuf, sizeof (db->cmdbuf), "select ekey from sndfile where fname='%s'", db->filename) ;

	err = sqlite3_get_table (db->sql, db->cmdbuf, &result, &rows, &cols, &errmsg) ;
	if (err != SQLITE_OK)
	{	printf ("Line %d : SQL error: %s\n", __LINE__, errmsg) ;
		puts (db->cmdbuf) ;
		} ;

	if (cols != 1 || rows != 1)
	{	printf ("Bad juju!! rows = %d cols = %d\n", rows, cols) ;
		exit (1) ;
		} ;

	ekey = strtol (result [1], NULL, 10) ;

	sqlite3_free_table (result) ;

	return ekey ;
} /* get_ekey_from_filename */

static int
get_filename_pathname_by_ekey (REGTEST_DB * db, int ekey)
{	char *errmsg, **result ;
	int err, rows, cols ;

	snprintf (db->cmdbuf, sizeof (db->cmdbuf), "select fname,fpath from sndfile where ekey='%d'", ekey) ;

	err = sqlite3_get_table (db->sql, db->cmdbuf, &result, &rows, &cols, &errmsg) ;
	if (err != SQLITE_OK)
	{	printf ("Line %d : SQL error: %s\n", __LINE__, errmsg) ;
		puts (db->cmdbuf) ;
		return 0 ;
		} ;

	if (cols != 2 || rows != 1)
	{	printf ("\nError (%s %d) : rows = %d cols = %d\n", __func__, __LINE__, rows, cols) ;
		exit (1) ;
		} ;

	strncpy (db->filename, result [2], sizeof (db->filename)) ;
	strncpy (db->pathname, result [3], sizeof (db->pathname)) ;

	sqlite3_free_table (result) ;

	return 1 ;
} /* get_filename_pathname_by_ekey */

static int
check_file_by_ekey (REGTEST_DB * db, int ekey)
{	SNDFILE * sndfile ;
	SF_INFO info ;
	char * errmsg, **result ;
	int err, k, rows, cols, checksum ;

	printf ("    %s : ", db->filename) ;
	fflush (stdout) ;

	memset (&info, 0, sizeof (info)) ;
	sndfile = sf_open (db->pathname, SFM_READ, &info) ;
	sf_command (sndfile, SFC_GET_LOG_INFO, db->logbuf, sizeof (db->logbuf)) ;
	checksum = (sndfile == NULL) ? 0 : calc_checksum (sndfile, &info) ;
	sf_close (sndfile) ;

	if (sndfile == NULL)
	{	printf ("\n\nError : Could not open '%s' : %s\n", db->pathname, sf_strerror (NULL)) ;
		puts (db->logbuf) ;
		exit (1) ;
		} ;

	single_quote_replace (db->logbuf) ;

	snprintf (db->cmdbuf, sizeof (db->cmdbuf), "select fname,srate,frames,channels,format,"
			"checksum,logbuf from sndfile where ekey='%d'", ekey) ;

	err = sqlite3_get_table (db->sql, db->cmdbuf, &result, &rows, &cols, &errmsg) ;
	if (err != SQLITE_OK)
	{	printf ("Line %d : SQL error: %s\n", __LINE__, errmsg) ;
		puts (db->cmdbuf) ;
		} ;

	for (k = 0 ; k < cols ; k++)
	{	if (strcmp (result [k], "fname") == 0)
		{	if (strcmp (result [k + cols], db->filename) == 0)
				continue ;
			printf ("\n\nError : fname doesn't match : %s != %s\n", result [k + cols], db->filename) ;
			} ;

		if (strcmp (result [k], "srate") == 0)
		{	if (strtol (result [k + cols], NULL, 10) == info.samplerate)
				continue ;
			printf ("\n\nError : srate doesn't match : %s == %d\n", result [k + cols], info.samplerate) ;
			} ;

		if (strcmp (result [k], "frames") == 0)
		{	if (strtoll (result [k + cols], NULL, 10) == info.frames)
				continue ;
			printf ("\n\nError : frames doesn't match : %s == %ld\n", result [k + cols], (long) info.frames) ;
			} ;

		if (strcmp (result [k], "channels") == 0)
		{	if (strtol (result [k + cols], NULL, 10) == info.channels)
				continue ;
			printf ("\n\nError : channels doesn't match : %s == %d\n", result [k + cols], info.channels) ;
			} ;

		if (strcmp (result [k], "format") == 0)
		{	if (strtol (result [k + cols], NULL, 16) == info.format)
				continue ;
			printf ("\n\nError : format doesn't match : %s == 0x%08x\n", result [k + cols], info.format) ;
			} ;

		if (strcmp (result [k], "checksum") == 0)
		{	int db_val = (int) strtoll (result [k + cols], NULL, 16) ;

			if (db_val == checksum)
				continue ;
			printf ("\n\nError : checksum doesn't match : 0x%08x == 0x%08x\n", db_val, checksum) ;
			} ;

		if (strcmp (result [k], "logbuf") == 0)
			continue ;

		printf ("\nHere is the old logubuffer :\n\n%s\n\nand the new :\n\n%s\n\n", result [2 * cols - 1], db->logbuf) ;
		exit (1) ;
		} ;

	sqlite3_free_table (result) ;

	puts ("ok") ;

	return 0 ;
} /* check_file_by_ekey */

/*==============================================================================
*/

static void
get_filename_pathname (REGTEST_DB * db, const char *filepath)
{	const char * cptr ;

	if (filepath [0] != '/')
	{	memset (db->pathname, 0, sizeof (db->pathname)) ;
		if (getcwd (db->pathname, sizeof (db->pathname)) == NULL)
		{	perror ("\ngetcwd failed") ;
			exit (1) ;
			} ;

		db->pathname [strlen (db->pathname)] = '/' ;
		strncat (db->pathname, filepath, sizeof (db->pathname)) ;
		}
	else
		strncpy (db->pathname, filepath, sizeof (db->pathname)) ;

	if ((cptr = strrchr (db->pathname, '/')) == NULL)
	{	printf ("\nError : bad pathname %s\n", filepath) ;
		exit (1) ;
		} ;

	strncpy (db->filename, cptr + 1, sizeof (db->filename)) ;
} /* get filename_pathname */

static void
single_quote_replace (char * buf)
{	while ((buf = strchr (buf, '\'')) != 0)
		buf [0] = '"' ;
} /* single_quote_replace */

static int
count_callback (REGTEST_DB * db, int argc, char **argv, char **colname)
{	db->count ++ ;

	argc = 0 ;
	argv = NULL ;
	colname = NULL ;
	return 0 ;
} /* count_callback */

static int
ekey_max_callback (REGTEST_DB * db, int argc, char **argv, char **unused)
{	int ekey ;

	argc = 0 ;
	unused = NULL ;

	ekey = strtol (argv [0], NULL, 10) ;
	if (ekey > db->ekey_max)
		db->ekey_max = ekey ;

	return 0 ;
} /* ekey_max_callback */

static int
callback (void *unused, int argc, char **argv, char **colname)
{	int k ;

	unused = NULL ;

	for (k = 0 ; k < argc ; k++)
		printf ("%s = %s\n", colname [k], argv [k] ? argv [k] : "NULL") ;

	printf ("\n") ;

	return 0 ;
} /* callback */

#else

int dummy (void) ;

int
dummy (void)
{	/*
	**	Empty dummy fnction so tha compiler doesn't winge about an
	**	empty file.
	*/
	return 0 ;
} /* dummy */

#endif
