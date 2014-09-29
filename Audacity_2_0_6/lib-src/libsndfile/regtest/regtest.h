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

typedef struct REG_DB_tag REG_DB ;

/* In database.c */
REG_DB * db_open (const char * db_name) ;

int db_create (const char * dbname) ;

int db_close (REG_DB * db_handle) ;

int db_file_exists (REG_DB * db_handle, const char * filename) ;
int db_add_file (REG_DB * db_handle, const char * filename) ;
int db_check_file (REG_DB * db_handle, const char * filename) ;

int db_list_all (REG_DB * db_handle) ;
int db_check_all (REG_DB * db_handle) ;
int db_del_entry (REG_DB * db_handle, const char * entry) ;

/* In checksum.c */
int calc_checksum (SNDFILE * file, const SF_INFO * info) ;

