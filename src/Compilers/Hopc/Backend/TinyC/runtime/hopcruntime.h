#ifndef __hopcruntime_h
#define __hopcruntime_h

#ifndef HOPCREGNUM
#define HOPCREGNUM 16
#endif

#include <stdint.h>

// FIXME: debug
#include <stdio.h>

typedef unsigned long hword_t;
typedef uint16_t hregmask;
typedef uint64_t htime_t; // FIXME: system-dependent

#define HTIME_MAX ((htime_t)(-1))

typedef union {
	hword_t *p;
	hword_t  w;
} hcell;

//typedef unsigned  hword_t;
typedef hword_t htag;

// FIXME: move to hopcruntime.c
#define TAGBITS(n) ((sizeof(hword_t))*8-(n))

#define HOPCTIMEDIV 1000

#define HOPCTASKQT 100000

#define HOPCMAXRECORDFIELDS 64 // TODO: remove hardcode

#define BITVECTSIZE(n) ((n)/sizeof(unsigned char) + ((n%(sizeof(unsigned char)))?1:0))

#define HOPCTASKTAG 0

#define CELLS(m) (m/sizeof(hcell))

#define P(x) ((x).p)

#define W(x) ((x).w)

#define S(x) ((hword_t*)(x))

#define HOPC_TICKS_OF_MILLIS(x) ((x)*HOPCTIMEDIV)

#define HOPC_SCHEDULER_IDLE_TICKS 10000

typedef struct __hopctagdata {
    hword_t size;
    hregmask pmask[HOPCMAXRECORDFIELDS/sizeof(hregmask)];
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
#define HOPCTASKREGMASK(r, m) (CURRENT((r))->mask = (m))

#define HOPC_TASK_ALIVE     0
#define HOPC_TASK_TO_DELETE 1
#define HOPC_TASK_DELETED   2

typedef union __task_id {
    struct {
        unsigned int state:2;
        hword_t id:TAGBITS(2);
    } t;
    hword_t _skip;
} hopc_task_id;

typedef struct __task {
    struct __task *next;
    hopc_ar *arhead;
    hcell regs[HOPCREGNUM];
    hopc_task_id id;
    hregmask mask;
    htime_t tsleep;
    htime_t tsleep_since;
} hopc_task;

typedef struct {
    hopc_task *first;
    hopc_task **last;
} hopc_task_q;

typedef struct {
    hopc_gc gc;
    hopc_task_q workers;
    hopc_task_q sleepers;
    hword_t taskid;
    htime_t taskqt;
    htime_t tasktime;
    htime_t idle;
    const hopc_tagdata *tagdata;
} hopc_runtime;

#define HOPC_GC_TRESHOLD(r) (hopc_gc_maxmem((r))/4)

#define CURRENT(r) ((r)->workers.first)
#define HOPC_HAS_TASKS(r) ((r)->workers.first || (r->sleepers.first))

typedef struct __memchunk {
    union {
        struct {
            unsigned gc_alive:1;
            unsigned gc_follow:1;
            hword_t tag:TAGBITS(2);
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
memchunk* hopc_gc_prev_chunk(hopc_runtime *r, memchunk *p);

void hopc_gc_mark_root_alive(hopc_runtime *runtime, memchunk *chunk);

hopc_task *hopc_find_task(hopc_runtime *runtime, hword_t id);
hopc_task *hopc_insert_task(hopc_runtime *runtime);
void hopc_delete_task(hopc_runtime *runtime, hword_t id);
void hopc_switch_task(hopc_runtime *runtime, htime_t delta);
void hopc_spawn_task(hopc_runtime*, hcell);
void hopc_detach_current(hopc_runtime *runtime);

void hopc_ffi__sleep(hopc_runtime*, hcell);

#define hopc_ffi__yield(r)  ((r)->tasktime=0)
#define hopc_ffi__spawn(r,c) {RS=(c);goto spawn;}

hword_t hopc_tagsize(hopc_runtime *r, htag tag); 

memchunk *hopc_make_activation_record(hopc_runtime *runtime, htag tag); 
hopc_ar *hopc_push_activation_record2(hopc_runtime *runtime, memchunk *chunk);
hopc_ar *hopc_push_activation_record(hopc_runtime *runtime, htag tag);
void hopc_pop_activation_record(hopc_runtime *r);

void hopc_spill_ar(hopc_runtime *r, hcell *chunk, hword_t slot, hcell data);
hcell *hopc_make_closure(hopc_runtime *runtime, hword_t label, hcell *archunk, htag tag);
void hopc_fix_closure(hopc_runtime *runtime, hword_t label, hcell *chunk);
void hopc_spill(hopc_runtime *r, hword_t slot, hcell data);
hcell hopc_unspill(hopc_runtime *r, hword_t slot);

void hopc_out_of_mem_hook(hopc_runtime*);

htime_t hopc_getcputime();

void hopc_system_sleep(htime_t);

#define BITGET(t, n) ((t)&(1<<(n)))

void hopc_debug_runtime(hopc_runtime*);

void dump_heap3(hopc_runtime *runtime); 

#endif
