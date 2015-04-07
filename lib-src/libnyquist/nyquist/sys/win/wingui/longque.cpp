#include "stddef.h"
#include "cppext.h"
#include "longque.h"
#include "stdlib.h"

void longque::init(int size)
{
    head = 0;
    tail = 0;
    count = 0;
    max = size;
    buff = (long *) malloc(sizeof(long) * size);
}


void longque::finish()
{
    free(buff);
}


//1 producer-consumer safe
long longque::remove()
{
    long l;
    if (count <= 0) return 0;
    count--;
    l = buff[head++];
    if (head == max) head = 0;
    return l;
}

        

