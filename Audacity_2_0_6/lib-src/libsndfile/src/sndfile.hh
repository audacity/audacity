/*
** Copyright (C) 2005-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** All rights reserved.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are
** met:
**
**     * Redistributions of source code must retain the above copyright
**       notice, this list of conditions and the following disclaimer.
**     * Redistributions in binary form must reproduce the above copyright
**       notice, this list of conditions and the following disclaimer in
**       the documentation and/or other materials provided with the
**       distribution.
**     * Neither the author nor the names of any contributors may be used
**       to endorse or promote products derived from this software without
**       specific prior written permission.
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
** "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
** TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
** PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
** CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
** EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
** PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
** OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
** WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
** OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
** ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
** The above modified BSD style license (GPL and LGPL compatible) applies to
** this file. It does not apply to libsndfile itself which is released under
** the GNU LGPL or the libsndfile test suite which is released under the GNU
** GPL.
** This means that this header file can be used under this modified BSD style
** license, but the LGPL still holds for the libsndfile library itself.
*/

/*
** sndfile.hh -- A lightweight C++ wrapper for the libsndfile API.
**
** All the methods are inlines and all functionality is contained in this
** file. There is no separate implementation file.
**
** API documentation is in the doc/ directory of the source code tarball
** and at http://www.mega-nerd.com/libsndfile/api.html.
*/

#ifndef SNDFILE_HH
#define SNDFILE_HH

#include <sndfile.h>

#include <string>
#include <new> // for std::nothrow

class SndfileHandle
{	private :
		struct SNDFILE_ref
		{	SNDFILE_ref (void) ;
			~SNDFILE_ref (void) ;

			SNDFILE *sf ;
			SF_INFO sfinfo ;
			int ref ;
			} ;

		SNDFILE_ref *p ;

	public :
			/* Default constructor */
			SndfileHandle (void) : p (NULL) {} ;
			SndfileHandle (const char *path, int mode = SFM_READ,
							int format = 0, int channels = 0, int samplerate = 0) ;
			SndfileHandle (std::string const & path, int mode = SFM_READ,
							int format = 0, int channels = 0, int samplerate = 0) ;
			SndfileHandle (int fd, bool close_desc, int mode = SFM_READ,
							int format = 0, int channels = 0, int samplerate = 0) ;

#ifdef ENABLE_SNDFILE_WINDOWS_PROTOTYPES
			SndfileHandle (LPCWSTR wpath, int mode = SFM_READ,
							int format = 0, int channels = 0, int samplerate = 0) ;
#endif

			~SndfileHandle (void) ;

			SndfileHandle (const SndfileHandle &orig) ;
			SndfileHandle & operator = (const SndfileHandle &rhs) ;

		/* Mainly for debugging/testing. */
		int refCount (void) const { return (p == NULL) ? 0 : p->ref ; }

		operator bool () const { return (p != NULL) ; }

		bool operator == (const SndfileHandle &rhs) const { return (p == rhs.p) ; }

		sf_count_t	frames (void) const		{ return p ? p->sfinfo.frames : 0 ; }
		int			format (void) const		{ return p ? p->sfinfo.format : 0 ; }
		int			channels (void) const	{ return p ? p->sfinfo.channels : 0 ; }
		int			samplerate (void) const { return p ? p->sfinfo.samplerate : 0 ; }

		int error (void) const ;
		const char * strError (void) const ;

		int command (int cmd, void *data, int datasize) ;

		sf_count_t	seek (sf_count_t frames, int whence) ;

		void writeSync (void) ;

		int setString (int str_type, const char* str) ;

		const char* getString (int str_type) const ;

		static int formatCheck (int format, int channels, int samplerate) ;

		sf_count_t read (short *ptr, sf_count_t items) ;
		sf_count_t read (int *ptr, sf_count_t items) ;
		sf_count_t read (float *ptr, sf_count_t items) ;
		sf_count_t read (double *ptr, sf_count_t items) ;

		sf_count_t write (const short *ptr, sf_count_t items) ;
		sf_count_t write (const int *ptr, sf_count_t items) ;
		sf_count_t write (const float *ptr, sf_count_t items) ;
		sf_count_t write (const double *ptr, sf_count_t items) ;

		sf_count_t readf (short *ptr, sf_count_t frames) ;
		sf_count_t readf (int *ptr, sf_count_t frames) ;
		sf_count_t readf (float *ptr, sf_count_t frames) ;
		sf_count_t readf (double *ptr, sf_count_t frames) ;

		sf_count_t writef (const short *ptr, sf_count_t frames) ;
		sf_count_t writef (const int *ptr, sf_count_t frames) ;
		sf_count_t writef (const float *ptr, sf_count_t frames) ;
		sf_count_t writef (const double *ptr, sf_count_t frames) ;

		sf_count_t	readRaw		(void *ptr, sf_count_t bytes) ;
		sf_count_t	writeRaw	(const void *ptr, sf_count_t bytes) ;

		/**< Raw access to the handle. SndfileHandle keeps ownership. */
		SNDFILE * rawHandle (void) ;

		/**< Take ownership of handle, iff reference count is 1. */
		SNDFILE * takeOwnership (void) ;
} ;

/*==============================================================================
**	Nothing but implementation below.
*/

inline
SndfileHandle::SNDFILE_ref::SNDFILE_ref (void)
: ref (1)
{}

inline
SndfileHandle::SNDFILE_ref::~SNDFILE_ref (void)
{	if (sf != NULL) sf_close (sf) ; }

inline
SndfileHandle::SndfileHandle (const char *path, int mode, int fmt, int chans, int srate)
: p (NULL)
{
	p = new (std::nothrow) SNDFILE_ref () ;

	if (p != NULL)
	{	p->ref = 1 ;

		p->sfinfo.frames = 0 ;
		p->sfinfo.channels = chans ;
		p->sfinfo.format = fmt ;
		p->sfinfo.samplerate = srate ;
		p->sfinfo.sections = 0 ;
		p->sfinfo.seekable = 0 ;

		p->sf = sf_open (path, mode, &p->sfinfo) ;
		} ;

	return ;
} /* SndfileHandle const char * constructor */

inline
SndfileHandle::SndfileHandle (std::string const & path, int mode, int fmt, int chans, int srate)
: p (NULL)
{
	p = new (std::nothrow) SNDFILE_ref () ;

	if (p != NULL)
	{	p->ref = 1 ;

		p->sfinfo.frames = 0 ;
		p->sfinfo.channels = chans ;
		p->sfinfo.format = fmt ;
		p->sfinfo.samplerate = srate ;
		p->sfinfo.sections = 0 ;
		p->sfinfo.seekable = 0 ;

		p->sf = sf_open (path.c_str (), mode, &p->sfinfo) ;
		} ;

	return ;
} /* SndfileHandle std::string constructor */

inline
SndfileHandle::SndfileHandle (int fd, bool close_desc, int mode, int fmt, int chans, int srate)
: p (NULL)
{
	if (fd < 0)
		return ;

	p = new (std::nothrow) SNDFILE_ref () ;

	if (p != NULL)
	{	p->ref = 1 ;

		p->sfinfo.frames = 0 ;
		p->sfinfo.channels = chans ;
		p->sfinfo.format = fmt ;
		p->sfinfo.samplerate = srate ;
		p->sfinfo.sections = 0 ;
		p->sfinfo.seekable = 0 ;

		p->sf = sf_open_fd (fd, mode, &p->sfinfo, close_desc) ;
		} ;

	return ;
} /* SndfileHandle fd constructor */

inline
SndfileHandle::~SndfileHandle (void)
{	if (p != NULL && --p->ref == 0)
		delete p ;
} /* SndfileHandle destructor */


inline
SndfileHandle::SndfileHandle (const SndfileHandle &orig)
: p (orig.p)
{	if (p != NULL)
		++p->ref ;
} /* SndfileHandle copy constructor */

inline SndfileHandle &
SndfileHandle::operator = (const SndfileHandle &rhs)
{
	if (&rhs == this)
		return *this ;
	if (p != NULL && --p->ref == 0)
		delete p ;

	p = rhs.p ;
	if (p != NULL)
		++p->ref ;

	return *this ;
} /* SndfileHandle assignment operator */

inline int
SndfileHandle::error (void) const
{	return sf_error (p->sf) ; }

inline const char *
SndfileHandle::strError (void) const
{	return sf_strerror (p->sf) ; }

inline int
SndfileHandle::command (int cmd, void *data, int datasize)
{	return sf_command (p->sf, cmd, data, datasize) ; }

inline sf_count_t
SndfileHandle::seek (sf_count_t frame_count, int whence)
{	return sf_seek (p->sf, frame_count, whence) ; }

inline void
SndfileHandle::writeSync (void)
{	sf_write_sync (p->sf) ; }

inline int
SndfileHandle::setString (int str_type, const char* str)
{	return sf_set_string (p->sf, str_type, str) ; }

inline const char*
SndfileHandle::getString (int str_type) const
{	return sf_get_string (p->sf, str_type) ; }

inline int
SndfileHandle::formatCheck (int fmt, int chans, int srate)
{
	SF_INFO sfinfo ;

	sfinfo.frames = 0 ;
	sfinfo.channels = chans ;
	sfinfo.format = fmt ;
	sfinfo.samplerate = srate ;
	sfinfo.sections = 0 ;
	sfinfo.seekable = 0 ;

	return sf_format_check (&sfinfo) ;
}

/*---------------------------------------------------------------------*/

inline sf_count_t
SndfileHandle::read (short *ptr, sf_count_t items)
{	return sf_read_short (p->sf, ptr, items) ; }

inline sf_count_t
SndfileHandle::read (int *ptr, sf_count_t items)
{	return sf_read_int (p->sf, ptr, items) ; }

inline sf_count_t
SndfileHandle::read (float *ptr, sf_count_t items)
{	return sf_read_float (p->sf, ptr, items) ; }

inline sf_count_t
SndfileHandle::read (double *ptr, sf_count_t items)
{	return sf_read_double (p->sf, ptr, items) ; }

inline sf_count_t
SndfileHandle::write (const short *ptr, sf_count_t items)
{	return sf_write_short (p->sf, ptr, items) ; }

inline sf_count_t
SndfileHandle::write (const int *ptr, sf_count_t items)
{	return sf_write_int (p->sf, ptr, items) ; }

inline sf_count_t
SndfileHandle::write (const float *ptr, sf_count_t items)
{	return sf_write_float (p->sf, ptr, items) ; }

inline sf_count_t
SndfileHandle::write (const double *ptr, sf_count_t items)
{	return sf_write_double (p->sf, ptr, items) ; }

inline sf_count_t
SndfileHandle::readf (short *ptr, sf_count_t frame_count)
{	return sf_readf_short (p->sf, ptr, frame_count) ; }

inline sf_count_t
SndfileHandle::readf (int *ptr, sf_count_t frame_count)
{	return sf_readf_int (p->sf, ptr, frame_count) ; }

inline sf_count_t
SndfileHandle::readf (float *ptr, sf_count_t frame_count)
{	return sf_readf_float (p->sf, ptr, frame_count) ; }

inline sf_count_t
SndfileHandle::readf (double *ptr, sf_count_t frame_count)
{	return sf_readf_double (p->sf, ptr, frame_count) ; }

inline sf_count_t
SndfileHandle::writef (const short *ptr, sf_count_t frame_count)
{	return sf_writef_short (p->sf, ptr, frame_count) ; }

inline sf_count_t
SndfileHandle::writef (const int *ptr, sf_count_t frame_count)
{	return sf_writef_int (p->sf, ptr, frame_count) ; }

inline sf_count_t
SndfileHandle::writef (const float *ptr, sf_count_t frame_count)
{	return sf_writef_float (p->sf, ptr, frame_count) ; }

inline sf_count_t
SndfileHandle::writef (const double *ptr, sf_count_t frame_count)
{	return sf_writef_double (p->sf, ptr, frame_count) ; }

inline sf_count_t
SndfileHandle::readRaw (void *ptr, sf_count_t bytes)
{	return sf_read_raw (p->sf, ptr, bytes) ; }

inline sf_count_t
SndfileHandle::writeRaw (const void *ptr, sf_count_t bytes)
{	return sf_write_raw (p->sf, ptr, bytes) ; }

inline SNDFILE *
SndfileHandle::rawHandle (void)
{	return (p ? p->sf : NULL) ; }

inline SNDFILE *
SndfileHandle::takeOwnership (void)
{
	if (p == NULL || (p->ref != 1))
		return NULL ;

	SNDFILE * sf = p->sf ;
	p->sf = NULL ;
	delete p ;
	p = NULL ;
	return sf ;
}

#ifdef ENABLE_SNDFILE_WINDOWS_PROTOTYPES

inline
SndfileHandle::SndfileHandle (LPCWSTR wpath, int mode, int fmt, int chans, int srate)
: p (NULL)
{
	p = new (std::nothrow) SNDFILE_ref () ;

	if (p != NULL)
	{	p->ref = 1 ;

		p->sfinfo.frames = 0 ;
		p->sfinfo.channels = chans ;
		p->sfinfo.format = fmt ;
		p->sfinfo.samplerate = srate ;
		p->sfinfo.sections = 0 ;
		p->sfinfo.seekable = 0 ;

		p->sf = sf_wchar_open (wpath, mode, &p->sfinfo) ;
		} ;

	return ;
} /* SndfileHandle const wchar_t * constructor */

#endif

#endif	/* SNDFILE_HH */

