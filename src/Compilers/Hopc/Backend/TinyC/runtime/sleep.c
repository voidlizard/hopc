
#define HOPCREGNUM 16

#include "hopcruntime.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    for(;;) {
        printf("time: %d\n", hopc_getcputime());
        hopc_system_sleep(500000);
    }
}

