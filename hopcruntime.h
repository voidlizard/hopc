#ifndef __hopcruntime_h
#define __hopcruntime_h

typedef unsigned  hword_t;

typedef struct {
} hopc_runtime;

#define HOPC_CALLFFI(n, args...) n(args)

#endif
