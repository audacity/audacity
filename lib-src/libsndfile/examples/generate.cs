/* (c) 2004 James Robson, http://www.arbingersys.com
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
**
** ****************************
**
** How to use:
**    - libsndfile.dll must have already been compiled and be in this
**      application's search path
**
**    - You must edit this file to point to the file you want to convert. Set
**    	the following line of code (found in the Main() function further below)
**    	to the name of a .WAV file that exists on your system.
**		186:	string sfn = "input.wav";
**
**    - From a command prompt type
**    		csc generate.cs
**
**    - Run the resulting executable 'generate.exe'
**
**
** Note: You will obviously need the csc compiler and the .NET runtime. I think
** 	 these are freely available for download from Microsoft's website
** 	 (part of the .NET SDK?).
*/


using System;
using System.Runtime.InteropServices;
using sf_count_t = System.Int64;	//alias; see SF_INFO struct

#if PLATFORM_64
using size_t = System.UInt64;
#else
using size_t = System.UInt32;
#endif


class lsndf_example {


//sound file formats
	public enum lsndf_frmts {
		SF_FORMAT_WAV			= 0x010000,		/* Microsoft WAV format (little endian). */
		SF_FORMAT_AIFF			= 0x020000,		/* Apple/SGI AIFF format (big endian). */
		SF_FORMAT_AU			= 0x030000,		/* Sun/NeXT AU format (big endian). */
		SF_FORMAT_RAW			= 0x040000,		/* RAW PCM data. */
		SF_FORMAT_PAF			= 0x050000,		/* Ensoniq PARIS file format. */
		SF_FORMAT_SVX			= 0x060000,		/* Amiga IFF / SVX8 / SV16 format. */
		SF_FORMAT_NIST			= 0x070000,		/* Sphere NIST format. */
		SF_FORMAT_VOC			= 0x080000,		/* VOC files. */
		SF_FORMAT_IRCAM			= 0x0A0000,		/* Berkeley/IRCAM/CARL */
		SF_FORMAT_W64			= 0x0B0000,		/* Sonic Foundry's 64 bit RIFF/WAV */
		SF_FORMAT_MAT4			= 0x0C0000,		/* Matlab (tm) V4.2 / GNU Octave 2.0 */
		SF_FORMAT_MAT5			= 0x0D0000,		/* Matlab (tm) V5.0 / GNU Octave 2.1 */
		SF_FORMAT_PVF			= 0x0E0000,		/* Portable Voice Format */
		SF_FORMAT_XI			= 0x0F0000,		/* Fasttracker 2 Extended Instrument */
		SF_FORMAT_HTK			= 0x100000,		/* HMM Tool Kit format */
		SF_FORMAT_SDS			= 0x110000,		/* Midi Sample Dump Standard */

		/* Subtypes from here on. */

		SF_FORMAT_PCM_S8		= 0x0001,		/* Signed 8 bit data */
		SF_FORMAT_PCM_16		= 0x0002,		/* Signed 16 bit data */
		SF_FORMAT_PCM_24		= 0x0003,		/* Signed 24 bit data */
		SF_FORMAT_PCM_32		= 0x0004,		/* Signed 32 bit data */

		SF_FORMAT_PCM_U8		= 0x0005,		/* Unsigned 8 bit data (WAV and RAW only) */

		SF_FORMAT_FLOAT			= 0x0006,		/* 32 bit float data */
		SF_FORMAT_DOUBLE		= 0x0007,		/* 64 bit float data */

		SF_FORMAT_ULAW			= 0x0010,		/* U-Law encoded. */
		SF_FORMAT_ALAW			= 0x0011,		/* A-Law encoded. */
		SF_FORMAT_IMA_ADPCM		= 0x0012,		/* IMA ADPCM. */
		SF_FORMAT_MS_ADPCM		= 0x0013,		/* Microsoft ADPCM. */

		SF_FORMAT_GSM610		= 0x0020,		/* GSM 6.10 encoding. */
		SF_FORMAT_VOX_ADPCM		= 0x0021,		/* OKI / Dialogix ADPCM */

		SF_FORMAT_G721_32		= 0x0030,		/* 32kbs G721 ADPCM encoding. */
		SF_FORMAT_G723_24		= 0x0031,		/* 24kbs G723 ADPCM encoding. */
		SF_FORMAT_G723_40		= 0x0032,		/* 40kbs G723 ADPCM encoding. */

		SF_FORMAT_DWVW_12		= 0x0040, 		/* 12 bit Delta Width Variable Word encoding. */
		SF_FORMAT_DWVW_16		= 0x0041, 		/* 16 bit Delta Width Variable Word encoding. */
		SF_FORMAT_DWVW_24		= 0x0042, 		/* 24 bit Delta Width Variable Word encoding. */
		SF_FORMAT_DWVW_N		= 0x0043, 		/* N bit Delta Width Variable Word encoding. */

		SF_FORMAT_DPCM_8		= 0x0050,		/* 8 bit differential PCM (XI only) */
		SF_FORMAT_DPCM_16		= 0x0051,		/* 16 bit differential PCM (XI only) */


		/* Endian-ness options. */

		SF_ENDIAN_FILE			= 0x00000000,	/* Default file endian-ness. */
		SF_ENDIAN_LITTLE		= 0x10000000,	/* Force little endian-ness. */
		SF_ENDIAN_BIG			= 0x20000000,	/* Force big endian-ness. */
		SF_ENDIAN_CPU			= 0x30000000,	/* Force CPU endian-ness. */

		SF_FORMAT_SUBMASK		= 0x0000FFFF,
		SF_FORMAT_TYPEMASK		= 0x0FFF0000,
		SF_FORMAT_ENDMASK		= 0x30000000
	}


//modes and other
	public enum lsndf_tf
	{	/* True and false */
		SF_FALSE	= 0,
		SF_TRUE		= 1,

		/* Modes for opening files. */
		SFM_READ	= 0x10,
		SFM_WRITE	= 0x20,
		SFM_RDWR	= 0x30
	}


//important SF_INFO structure
	[StructLayout(LayoutKind.Sequential)]
	public struct SF_INFO
	{
		public sf_count_t	frames ;		// Used to be called samples.  Changed to avoid confusion.
		public int			samplerate ;
		public int			channels ;
		public int			format ;
		public int			sections ;
		public int			seekable ;
	};


//function declarations
//Note: Not all functions have been prototyped here. Only the ones necessary to
//	make this application work. The below code should give some clues as to
//	how to add the rest since they have a lot of parameter and return type
//	similarities.
	[DllImport("libsndfile.dll")]
	public static extern IntPtr sf_open ([MarshalAs(UnmanagedType.LPStr)] string path, int mode, ref SF_INFO sfinfo);

	[DllImport("libsndfile.dll")]
	static extern int sf_error (IntPtr sndfile);

	[DllImport("libsndfile.dll")]
	static extern IntPtr sf_strerror (IntPtr sndfile);

	[DllImport("libsndfile.dll")]
	static extern int sf_format_check (ref SF_INFO info);

	[DllImport("libsndfile.dll")]
	static extern sf_count_t sf_read_float	(IntPtr sndfile, float[] ptr, sf_count_t items);

	[DllImport("libsndfile.dll")]
	static extern sf_count_t sf_write_float	(IntPtr sndfile, float[] ptr, sf_count_t items);

	[DllImport("libsndfile.dll")]
    static extern int sf_close (IntPtr sndfile);


	public const sf_count_t BUFFER_LEN = 4096;


//program entry
	static void Main(  ) {


//declarations
		SF_INFO sfinfo = new SF_INFO();
		float[] buffer = new float[BUFFER_LEN];
		sf_count_t rcnt;

//set the input file
		string sfn = "input.wav";	//set to a file on YOUR system
		//string sfn = "noexist.wav"; 	//test with non-existent file

//set the output file
		string ofn = "output.wav";

//read in sound file to convert
		IntPtr infile = sf_open (sfn, (int)lsndf_tf.SFM_READ, ref sfinfo);

//exit if error was thrown
		if ( (int)infile == 0 ) {
			Console.WriteLine("Error opening " + sfn);
			Console.WriteLine("Error #" + sf_error(infile));
			return;
		}

//set the file type for the output file
//uncomment one and only one of the statements below to change the output
//file encoding.
		//sfinfo.format = (int)(lsndf_frmts.SF_FORMAT_WAV | lsndf_frmts.SF_FORMAT_PCM_U8);
		//sfinfo.format = (int)(lsndf_frmts.SF_FORMAT_WAV | lsndf_frmts.SF_FORMAT_PCM_16);
		//sfinfo.format = (int)(lsndf_frmts.SF_FORMAT_WAV | lsndf_frmts.SF_FORMAT_MS_ADPCM);
		sfinfo.format = (int)(lsndf_frmts.SF_FORMAT_WAV | lsndf_frmts.SF_FORMAT_IMA_ADPCM);
		//sfinfo.format = (int)(lsndf_frmts.SF_FORMAT_WAV | lsndf_frmts.SF_FORMAT_GSM610);
		/* Soundforge W64. */
		//sfinfo.format = (int)(lsndf_frmts.SF_FORMAT_W64 | lsndf_frmts.SF_FORMAT_PCM_U8);
		//sfinfo.format = (int)(lsndf_frmts.SF_FORMAT_W64 | lsndf_frmts.SF_FORMAT_PCM_16);
		//sfinfo.format = (int)(lsndf_frmts.SF_FORMAT_W64 | lsndf_frmts.SF_FORMAT_MS_ADPCM);
		//sfinfo.format = (int)(lsndf_frmts.SF_FORMAT_W64 | lsndf_frmts.SF_FORMAT_IMA_ADPCM);
		//sfinfo.format = (int)(lsndf_frmts.SF_FORMAT_W64 | lsndf_frmts.SF_FORMAT_GSM610);


//check that SF_INFO is valid
		if ( sf_format_check(ref sfinfo) == 0 ) {
			Console.WriteLine("sf_format_check failed. Invalid encoding");
			return;
		}

//open output file
		IntPtr outfile = sf_open (ofn, (int)lsndf_tf.SFM_WRITE, ref sfinfo);

//exit if error was thrown
		if ( (int)outfile == 0 ) {
			Console.WriteLine("Error opening " + ofn);
			Console.WriteLine("Error #" + sf_error(outfile));
			return;
		}

//infile -> outfile
		Console.Write(sfn + " -> " + ofn);
		while ( (rcnt = sf_read_float (infile, buffer, BUFFER_LEN)) > 0) {
			Console.Write(".");
			sf_write_float (outfile, buffer, BUFFER_LEN);
		}
		Console.WriteLine("done.");

//close up shop
		sf_close(infile);
		sf_close(outfile);


	} //main()


} //class lsndf_example {}

