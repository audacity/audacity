#define NOTEOFF 0x80
#define NOTEON 0x90
#define PRESSURE 0xa0
#define CONTROLLER 0xb0
#define PITCHBEND 0xe0
#define PROGRAM 0xc0
#define CHANPRESSURE 0xd0

/* These are the strings used in keynote to identify Standard MIDI File */
/* meta text messages. */

#define METATEXT                "Text Event"
#define METACOPYRIGHT           "Copyright Notice"
#define METASEQUENCE            "Sequence/Track Name"
#define METAINSTRUMENT          "Instrument Name"
#define METALYRIC               "Lyric"
#define METAMARKER              "Marker"
#define METACUE                 "Cue Point"
#define METAUNRECOGNIZED        "Unrecognized"


class Midifile_reader {
public:
    void midifile();
    int Mf_nomerge; /* 1 => continue'ed system exclusives are */
				        /* not collapsed. */
    long Mf_currtime; /* current time in delta-time units */
    int Mf_skipinit;   /* 1 if initial garbage should be skipped */
    Midifile_reader();
	// call finalize() when done or you may leak memory.
	void finalize();  /* clean up before deletion */
	// Note: rather than finalize, we should have ~Midifile_reader(),
	// but at least VC++ complains that there is no Mf_free(), even
	// though Mf_free is declared as virtual and this is an abstract
	// class. I don't understand this, so finalize() is a workaround. -RBD

protected:
    int midifile_error;

    virtual void *Mf_malloc(size_t size) = 0; /* malloc() */
    virtual void Mf_free(void *obj, size_t size) = 0; /* free() */
    /* Methods to be called while processing the MIDI file. */
    virtual void Mf_starttrack() = 0;
    virtual void Mf_endtrack() = 0;
    virtual int Mf_getc() = 0;
    virtual void Mf_chanprefix(int) = 0;
    virtual void Mf_portprefix(int) = 0;
    virtual void Mf_eot() = 0;
    virtual void Mf_error(const char *) = 0;
    virtual void Mf_header(int,int,int) = 0;
    virtual void Mf_on(int,int,int) = 0;
    virtual void Mf_off(int,int,int) = 0;
    virtual void Mf_pressure(int,int,int) = 0;
    virtual void Mf_controller(int,int,int) = 0;
    virtual void Mf_pitchbend(int,int,int) = 0;
    virtual void Mf_program(int,int) = 0;
    virtual void Mf_chanpressure(int,int) = 0;
    virtual void Mf_sysex(int,unsigned char*) = 0;
    virtual void Mf_arbitrary(int,unsigned char*) = 0;
    virtual void Mf_metamisc(int,int,unsigned char*) = 0;
    virtual void Mf_seqnum(int) = 0;
    virtual void Mf_smpte(int,int,int,int,int) = 0;
    virtual void Mf_timesig(int,int,int,int) = 0;
    virtual void Mf_tempo(int) = 0;
    virtual void Mf_keysig(int,int) = 0;
    virtual void Mf_sqspecific(int,unsigned char*) = 0;
    virtual void Mf_text(int,int,unsigned char*) = 0;

private:
    long Mf_toberead;

    long readvarinum();
    long read32bit();
    int read16bit();
    void msgenlarge();
    unsigned char *msg();
    int readheader();
    void readtrack();
    void sysex();
    void msginit();
    int egetc();
    int msgleng();

    int readmt(const char*,int);
    long to32bit(int,int,int,int);
    int to16bit(int,int);
    void mferror(const char *);
    void badbyte(int);
    void metaevent(int);
    void msgadd(int);
    void chanmessage(int,int,int);

    unsigned char *Msgbuff;
    long Msgsize;
    long Msgindex;
};


