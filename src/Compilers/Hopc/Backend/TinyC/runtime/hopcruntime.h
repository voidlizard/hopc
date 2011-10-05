#ifndef __hopcruntime_h
#define __hopcruntime_h

#ifndef HOPCREGNUM
#define HOPCREGNUM 16
#endif

#include <stdint.h>

typedef unsigned long hword_t ;

typedef union {
	hword_t *p;
	hword_t  w;
} hcell;

//typedef unsigned  hword_t;
typedef hword_t htag;

#define HOPCMAXRECORDFIELDS 64 // TODO: remove hardcode

#define BITVECTSIZE(n) ((n)/sizeof(unsigned char) + ((n%(sizeof(unsigned char)))?1:0))

#define HOPCTASKTAG 0

#define CELLS(m) (m/sizeof(hcell))

#define P(x) ((x).p)

#define W(x) ((x).w)

#define S(x) ((hword_t*)(x))

typedef struct __hopctagdata {
    hword_t size;
    unsigned char pmask[HOPCMAXRECORDFIELDS/8];
} hopc_tagdata;

typedef struct __hopgc {
    hcell *heapstart_p;
    hcell *heapend_p;
    hcell *top;
    hcell *gcbottom;
} hopc_gc;

typedef struct __ar {
    hword_t  size; // TODO: remove this somehow
    struct __ar *next;
    hcell slots[1];
} hopc_ar;

#define HOPC_AR_HEAD (CELLS(sizeof(hopc_ar)) - CELLS(sizeof(hcell)))
#define HOPC_AR_SLOTS(ar) (((ar)->size)-HOPC_AR_HEAD)

typedef struct __closure {
    hcell cp;
    hcell ar;
} hopc_closure;

#define CLOSURE_AR(r, x) ((memchunk*)((hopc_closure*)hopc_gc_chunk_start(runtime, (hcell*)((x).p)))->ar.p)
#define CLOSURE_CP(r, x) (((hopc_closure*)hopc_gc_chunk_start(runtime, (hcell*)((x).p)))->cp)

#define HOPCTASKMASKSIZE BITVECTSIZE(HOPCREGNUM)

typedef struct __task {
    struct __task *next;
    hopc_ar *arhead;
    hcell regs[HOPCREGNUM];
    hword_t id;
    unsigned char mask[HOPCTASKMASKSIZE];
} hopc_task;

typedef struct {
    hopc_gc gc;
    hopc_task *taskheadp;
    hword_t taskid;
    const hopc_tagdata *tagdata;
} hopc_runtime;

// FIXME: move to hopcruntime.c
#define TAGBITS ((sizeof(hword_t))*8-2)
typedef struct __memchunk {
    union {
        struct {
            unsigned gc_alive:1;
            unsigned gc_follow:1;
            hword_t tag:TAGBITS;
        } t; 
        hcell cell;
    };
} memchunk;

#define HOPC_CALLFFI(n, args...) hopc_ffi__##n(args)

void hopc_init_runtime(hopc_runtime *, hcell *mem, hword_t size);
void hopc_gc_init(hopc_gc *gc, hcell *mem, hword_t size);
hcell *hopc_gc_alloc_chunk(hopc_runtime *, htag);
void hopc_gc_collect(hopc_runtime* r);

hcell *hopc_gc_chunk_start(hopc_runtime *r, hcell *p); 

hword_t hopc_gc_maxmem(hopc_runtime *r); 
hword_t hopc_gc_freemem(hopc_runtime *r);
hword_t hopc_gc_chunksize(hopc_runtime *r, memchunk *p);

void hopc_gc_mark_root_alive(hopc_runtime *runtime, memchunk *chunk);

hopc_task *hopc_find_task(hopc_runtime *runtime, hword_t id);
hopc_task *hopc_insert_task(hopc_runtime *runtime);
void hopc_delete_task(hopc_runtime *runtime, hword_t id);

hword_t hopc_tagsize(hopc_runtime *r, htag tag); 

memchunk *hopc_make_activation_record(hopc_runtime *runtime, htag tag); 
hopc_ar *hopc_push_activation_record2(hopc_runtime *runtime, memchunk *chunk);
hopc_ar *hopc_push_activation_record(hopc_runtime *runtime, htag tag);
void hopc_pop_activation_record(hopc_runtime *r);

void hopc_spill_ar(hopc_runtime *r, hcell *chunk, hword_t slot, hcell data);
hcell *hopc_make_closure(hopc_runtime *runtime, hword_t label, hcell *archunk, htag tag);
void hopc_spill(hopc_runtime *r, hword_t slot, hcell data);
hcell hopc_unspill(hopc_runtime *r, hword_t slot);

void hopc_out_of_mem_hook(hopc_runtime*);

#define BITGET(t, n) (((t)[(n)/8])&(1<<(n)%8))
#define HOPC_IS_PTR_TAG(t, n) (BITGET(((t)->pmask), (n)))

#endif
