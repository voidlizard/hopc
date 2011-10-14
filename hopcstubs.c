
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#include "hopcstubs.h"

void hopc_ffi__display(hopc_runtime *r, hcell data) {
    hword_t *p = data.p;
    p++;
/*    printf("[%d] {%d} [%08X] ", time(0), CURRENT(r)->tsleep, CURRENT(r));*/
    while(*p) putchar(*p++);
}

void hopc_ffi__newline(hopc_runtime *r) {
    putchar('\n');
}

