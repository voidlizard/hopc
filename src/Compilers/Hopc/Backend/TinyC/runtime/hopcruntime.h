#ifndef __hopcruntime_h
#define __hopcruntime_h

#ifndef HOPCREGNUM
#define HOPCREGNUM 16
#endif

#include <stdint.h>

typedef unsigned  hword_t;
typedef unsigned  htag;

#define HOPCMAXRECORDFIELDS 64 // TODO: remove hardcode

#define BITVECTSIZE(n) ((n)/sizeof(unsigned char) + ((n%(sizeof(unsigned char)))?1:0))

#define HOPCTASKTAG 0

#define WORDS(m) (m/sizeof(hword_t))

#define P(x) ((hword_t*)(x))

#define W(x) ((hword_t)(x))


typedef struct __hopctagdata {
    hword_t size;
    unsigned char pmask[HOPCMAXRECORDFIELDS/8];
} hopc_tagdata;

typedef struct __hopgc {
	hword_t *heapstart_p;
	hword_t *heapend_p;
  hword_t *top;
  hword_t *gcbottom;
} hopc_gc;

typedef struct __ar {
    hword_t  size; // TODO: remove this somehow
    struct __ar *next;
    hword_t slots[1];
} hopc_ar;

typedef struct __closure {
  hword_t  cp;
  hword_t *ar;
} hopc_closure;

#define HOPC_CLOSURE_UNPACK_AR(r, x)  (((hopc_closure*)(hopc_gc_chunk_start((r), (x))))->ar)  //W(hopc_gc_chunk_start((r), (((hopc_closure*)(hopc_gc_chunk_start((r), (x))))->ar)))
#define HOPC_CLOSURE_UNPACK_CHECKPOINT(r, x) (((hopc_closure*)(hopc_gc_chunk_start((r), (x))))->cp)

#define HOPCTASKMASKSIZE BITVECTSIZE(HOPCREGNUM)

typedef struct __task {
    struct __task *next;
    hopc_ar *arhead;
    hword_t regs[HOPCREGNUM];
    hword_t id;
    unsigned char mask[HOPCTASKMASKSIZE];
} hopc_task;

typedef struct {
    hopc_gc gc;
    hopc_task *taskheadp;
    hword_t taskid;
    const hopc_tagdata *tagdata;
} hopc_runtime;

#define HOPC_CALLFFI(n, args...) hopc_ffi__##n(args)

void hopc_init_runtime(hopc_runtime *, hword_t *mem, hword_t size);

hword_t hopc_tagsize(hopc_runtime*, htag);

void hopc_gc_init(hopc_gc *gc, hword_t *mem, hword_t size);
hword_t *hopc_gc_alloc_chunk(hopc_runtime *, htag);
void hopc_gc_mark_root_alive(hopc_runtime *runtime, hword_t *r);
hword_t hopc_gc_chunksize(hopc_runtime*, hword_t*);
hword_t* hopc_gc_prev_chunk(hopc_runtime *r, hword_t *p);
hword_t hopc_gc_maxmem(hopc_runtime*);
hword_t hopc_gc_freemem(hopc_runtime*);
hword_t *hopc_gc_find_live(hopc_runtime*r, hword_t*, hword_t*);
void hopc_gc_mark_roots(hopc_runtime*);
void hopc_gc_mark(hopc_runtime*); 
void hopc_gc_compact(hopc_runtime*);
hword_t *hopc_gc_chunk_start(hopc_runtime*, hword_t *p);
hword_t *hopc_gc_calc_shift(hopc_runtime* r, hword_t**, hword_t **);
hword_t hopc_gc_compact_step(hopc_runtime*);
void hopc_gc_update_pointers(hopc_runtime*, hword_t*, hword_t);
void hopc_gc_collect(hopc_runtime*);

hopc_task *hopc_insert_task(hopc_runtime*);
void hopc_delete_task(hopc_runtime*, hword_t);

hword_t *hopc_make_activation_record(hopc_runtime*, htag); // returns a chunk!

hopc_ar *hopc_push_activation_record(hopc_runtime*, htag);
//hopc_ar *hopc_push_activation_record2(hopc_runtime*, hword_t*);
void hopc_pop_activation_record(hopc_runtime*);

void hopc_spill(hopc_runtime*, hword_t, hword_t);
void hopc_spill_ar(hopc_runtime*, hword_t*, hword_t, hword_t);
hword_t hopc_unspill(hopc_runtime*, hword_t);

hword_t *hopc_make_closure(hopc_runtime*, hword_t, hword_t*, htag);

void hopc_out_of_mem_hook(hopc_runtime*);

// FIXME: move to hopcruntime.c
#define TAGBITS ((sizeof(hword_t))*8-2)
typedef struct __memchunk {
    union {
        struct {
            unsigned gc_alive:1;
            unsigned gc_follow:1;
            unsigned tag:TAGBITS;
        } t; 
        hword_t _skip[1];
    };
//    hword_t payload[1];
} memchunk;

#define BITGET(t, n) (((t)[(n)/8])&(1<<(n)%8))
#define HOPC_IS_PTR_TAG(t, n) (BITGET(((t)->pmask), (n)))

#endif
