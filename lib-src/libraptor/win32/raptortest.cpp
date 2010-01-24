// raptortest.cpp : Defines the entry point for the console application.
//

#include <stdio.h>
#include <windows.h>
#include "..\raptor.h"

#define TIMEIT
#define DUMPIT

int nStmts = 0;

void dump_statement(void* user_data, const raptor_statement *statement)
{
	nStmts++;
#ifdef DUMPIT
	printf("%li, [%s, ", nStmts, statement->subject);

	if(statement->predicate_type == RAPTOR_PREDICATE_TYPE_ORDINAL)
		printf("[rdf:_%d]", *((int*)statement->predicate));
	else if(statement->predicate_type == RAPTOR_PREDICATE_TYPE_XML_NAME)
		printf("%s", (const char*)statement->predicate);
	else
		printf("[%s]", (const char*)statement->predicate);

	if(statement->object_type == RAPTOR_OBJECT_TYPE_LITERAL || 
		statement->object_type == RAPTOR_OBJECT_TYPE_XML_LITERAL)
		printf(", \"%s\"]",  (const char*)statement->object);
	else if(statement->object_type == RAPTOR_OBJECT_TYPE_XML_NAME)
		printf(", %s]", (const char*)statement->object);
	else
		printf(", %s]", statement->object);

	printf("\n");
#endif
}

void usage()
{
	printf("Usage: raptortest <rdffileurl>\nE.g. file:c:\\test.rdf\n");
}

int main(int argc, char* argv[])
{
	if (argc != 2)
	{
		usage();
		return -1;
	}


	raptor_init();

	raptor_parser* p = raptor_new("");

	raptor_set_statement_handler(p, NULL, dump_statement);

#ifdef TIMEIT
	LARGE_INTEGER  Start; 
	QueryPerformanceCounter(&Start);
#endif
	raptor_parse_file(p, argv[1], NULL);

#ifdef TIMEIT
	LARGE_INTEGER End;
	QueryPerformanceCounter(&End);
	LARGE_INTEGER Frec;
	QueryPerformanceFrequency(&Frec);
	double Total = (End.LowPart-Start.LowPart);
	double StmtsPer = Total/nStmts;

	printf("%li statments processed in %g t. %g t/stmts, %li t/s, %g stmts/s", nStmts, Total, StmtsPer, Frec.LowPart, (double)Frec.LowPart/StmtsPer);
#endif

	raptor_free(p);

	raptor_finish();

	return 0;
}

  
