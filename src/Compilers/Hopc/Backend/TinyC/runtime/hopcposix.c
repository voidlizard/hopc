
#include <time.h>
#include <sys/time.h>
#include <unistd.h>

#include <hopcruntime.h>

htime_t hopc_getcputime() {
    struct timespec t;    
/*    clock_gettime(CLOCK_MONOTONIC, &t);*/
/*    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t);*/
    clock_gettime(CLOCK_REALTIME, &t);
    return (htime_t)((t.tv_sec*(1000000000/HOPCTIMEDIV)) + (t.tv_nsec/HOPCTIMEDIV));
}

void hopc_system_sleep(htime_t usec) {
    struct timespec t, rm;
/*    fprintf(stderr, "SLEEP %d %d\n", usec, usec * HOPCTIMEDIV);*/
    t.tv_sec =  usec / 1000000000;
    t.tv_nsec = usec % 1000000000;
    clock_nanosleep(CLOCK_REALTIME, 0, &t, &rm);
}

