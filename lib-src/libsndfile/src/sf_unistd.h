/*
** Copyright (C) 2002-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

/* Some defines that microsoft 'forgot' to implement. */

#ifndef S_IRWXU
#define	S_IRWXU 	0000700	/* rwx, owner */
#endif

#ifndef		S_IRUSR
#define		S_IRUSR	0000400	/* read permission, owner */
#endif

#ifndef		S_IWUSR
#define		S_IWUSR	0000200	/* write permission, owner */
#endif

#ifndef		S_IXUSR
#define		S_IXUSR	0000100	/* execute/search permission, owner */
#endif

#define	S_IRWXG		0000070	/* rwx, group */
#define		S_IRGRP	0000040	/* read permission, group */
#define		S_IWGRP	0000020	/* write permission, grougroup */
#define		S_IXGRP	0000010	/* execute/search permission, group */

#define	S_IRWXO		0000007	/* rwx, other */
#define		S_IROTH	0000004	/* read permission, other */
#define		S_IWOTH	0000002	/* write permission, other */
#define		S_IXOTH	0000001	/* execute/search permission, other */

#ifndef S_ISFIFO
#define S_ISFIFO(mode)	(((mode) & _S_IFMT) == _S_IFIFO)
#endif

#ifndef S_ISREG
#define	S_ISREG(mode)	(((mode) & _S_IFREG) == _S_IFREG)
#endif

/*
**	Don't know if these are still needed.
**
**	#define	_IFMT		_S_IFMT
**	#define _IFREG		_S_IFREG
*/

