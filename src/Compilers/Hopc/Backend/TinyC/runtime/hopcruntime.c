#include <hopcruntime.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define BLOCKSIZE(n) ((n) + CELLS(sizeof(memchunk)))

#define PTRSHIFT(p, l, n) (((hword_t*)(p))>(((hword_t*)(l))) ? (((hword_t*)(p))-(n)) : ((hword_t*)(p)))

static inline hcell *hopc_ar_chunk_start(hopc_runtime *r, hopc_ar*);

memchunk *hopc_get_task_chunk(hopc_runtime *r, hopc_task *t);
void hopc_gc_update_pointers(hopc_runtime *r, hcell *left, hword_t shift);
void hopc_gc_update_chunk_pointers(hopc_runtime *r, hcell* lb, hword_t shift, hword_t size, hcell *raw, const hregmask*);
void hopc_gc_mark_chunk_pointers(hopc_runtime *r, hword_t size, hcell *raw, const hregmask*);
hopc_ar *hopc_gc_update_ar_pointers(hopc_runtime *r, hopc_task *tp, hcell *lb, hword_t shift);
void hopc_gc_update_task_pointers(hopc_runtime *r, hcell *lb, hword_t shift);
memchunk* hopc_gc_prev_chunk(hopc_runtime *r, memchunk *p);
void hopc_gc_mark_dead(hopc_runtime* r);
void hopc_gc_mark(hopc_runtime* r);
hcell *hopc_gc_find_live(hopc_runtime *r, hcell *from, hcell *rbound);
static hword_t hopc_gc_distance(hopc_runtime *r, hcell *left, hcell *right);
hcell* hopc_gc_calc_shift(hopc_runtime* r, hcell **leftp, hcell **rightp);
void hopc_gc_compact(hopc_runtime* r);
void hopc_gc_update_pointers(hopc_runtime *r, hcell *left, hword_t shift);
void hopc_gc_mark_activation_record(hopc_runtime *r, hopc_ar *p);
void hopc_gc_mark_roots(hopc_runtime* r);
void hopc_gc_mark_root_alive(hopc_runtime *runtime, memchunk *chunk); 


void hopc_out_of_mem_hook(hopc_runtime *r) {
    fprintf(stderr, "*** OUT OF MEMORY");
    exit(-1); // FIXME
}


void hopc_gc_init(hopc_gc *gc, hcell *mem, hword_t size) {
    gc->heapstart_p = mem;
    gc->heapend_p = mem + size;
    gc->top = gc->heapstart_p;
    gc->gcbottom =  gc->heapstart_p;
}

void hopc_init_runtime(hopc_runtime *runtime, hcell *mem, hword_t size) {
    hopc_gc_init(&(runtime->gc), mem, size);
    runtime->taskheadp = 0;
    runtime->taskid = 0;
}

hword_t hopc_tagsize(hopc_runtime *r, htag tag) {
    switch(tag) {
        case HOPCTASKTAG:
            return CELLS(sizeof(hopc_task));
    }
    return r->tagdata[tag].size;
}

hcell *hopc_gc_chunk_start(hopc_runtime *r, hcell *p) {
    return p - hopc_tagsize(r, ((memchunk*)p)->t.tag);
}

static inline hcell *hopc_ar_chunk_start(hopc_runtime *r, hopc_ar *p) {
    return ((hcell*)p) + p->size;
}

hcell *hopc_gc_alloc_chunk(hopc_runtime *runtime, htag tag) {
    hcell *top = runtime->gc.top;
    hcell *end = runtime->gc.heapend_p;
    hword_t ts = hopc_tagsize(runtime, tag);
    hword_t size = BLOCKSIZE(ts);
    memchunk *chunk = 0;
    if( end - top >= size ) {
        runtime->gc.top += size;
        chunk = (memchunk*)(top + ts);
        chunk->t.gc_alive = 0;
        chunk->t.gc_follow = 1;
        chunk->t.tag = tag;
        fprintf(stderr, "allocated: %08X %d %d %04X free: %d\n", chunk, size, hopc_tagsize(runtime, tag), tag, (runtime->gc.heapend_p - runtime->gc.top));
        return (hcell*)chunk;
    }
    hopc_out_of_mem_hook(runtime);
    return (hcell*)0;
}

memchunk *hopc_get_task_chunk(hopc_runtime *r, hopc_task *t) {
    return ((memchunk*)t + hopc_tagsize(r, HOPCTASKTAG));
}

hopc_task *hopc_find_task(hopc_runtime *runtime, hword_t id) {
    hopc_task *p = runtime->taskheadp;
    for(; p && p->id != id; p = p->next );
    if( p && p->id == id ) return p;
    return 0;
}

hopc_task *hopc_insert_task(hopc_runtime *runtime) {
    hcell *chunk = hopc_gc_alloc_chunk(runtime, HOPCTASKTAG);
    hopc_task *t = 0;
    ((memchunk*)chunk)->t.gc_follow = 0;
    t = (hopc_task*)hopc_gc_chunk_start(runtime, (hcell*)chunk);
    t->id = runtime->taskid++;
    t->next = runtime->taskheadp;
    runtime->taskheadp = t;
    fprintf(stderr, "ALLOC TASK CHUNK: 0x%08X 0x%08X\n", t, chunk);
    hopc_gc_mark_root_alive(runtime, ((memchunk*)chunk));
    t->arhead = 0;
    memset(t->regs, 0, sizeof(hcell)*HOPCREGNUM);
    t->mask = 0;
    return t;
}

void hopc_delete_task(hopc_runtime *runtime, hword_t id) {
    hopc_task *p = runtime->taskheadp;
    hopc_task *t = hopc_find_task(runtime, id);
    
    if( t == runtime->taskheadp ) {
        runtime->taskheadp = t->next;
        return;
    }

    for(; t && p && p->next != t ; p = p->next );
    if( p && p->next == t ) {
        p->next = t->next;
    }
}

memchunk *hopc_make_activation_record(hopc_runtime *runtime, htag tag) {
    hcell *chunk = hopc_gc_alloc_chunk(runtime, tag);
    hopc_ar *arp = 0;
    arp = (hopc_ar*)hopc_gc_chunk_start(runtime, chunk);
    arp->size = hopc_tagsize(runtime, tag);
    memset(arp->slots, 0, sizeof(hword_t)*HOPC_AR_SLOTS(arp));
    ((memchunk*)chunk)->t.gc_follow = 1;
    return (memchunk*)chunk;
}


hopc_ar *hopc_push_activation_record2(hopc_runtime *runtime, memchunk *chunk) {
    hopc_ar *arp = (hopc_ar*)hopc_gc_chunk_start(runtime, (hcell*)chunk);
    if( runtime->taskheadp ) {
        arp->next = runtime->taskheadp->arhead;
        runtime->taskheadp->arhead = arp;
    }
    return arp;
}

hopc_ar *hopc_push_activation_record(hopc_runtime *runtime, htag tag) {
    memchunk *chunk = hopc_make_activation_record(runtime, tag);
    return hopc_push_activation_record2(runtime, chunk);
}

void hopc_pop_activation_record(hopc_runtime *r) {
    hopc_ar *arp = r->taskheadp ? r->taskheadp->arhead : 0;
    if( arp ) {
        r->taskheadp->arhead = arp->next;
    }
}

void hopc_spill_ar(hopc_runtime *r, hcell *chunk, hword_t slot, hcell data) {
  hopc_ar *arp = (hopc_ar*)hopc_gc_chunk_start(r, chunk);
  arp->slots[slot] = data;
}

hcell *hopc_make_closure(hopc_runtime *runtime, hword_t label, hcell *archunk, htag tag) {
    hcell *chunk = hopc_gc_alloc_chunk(runtime, tag);
    hopc_closure *closp = (hopc_closure*)hopc_gc_chunk_start(runtime, chunk);
/*    printf("make closure: %d %08X\n", label, archunk );*/
    closp->cp.w = label;
    closp->ar.p = (hword_t*)archunk;
/*    printf("make closure cp: %d %08X t:%04X t1:%04X %08X / ar %08X\n", closp->cp, closp, tag, ((memchunk*)chunk)->t.tag, chunk, closp->ar);*/
    return chunk;
}

void hopc_spill(hopc_runtime *r, hword_t slot, hcell data) {
  r->taskheadp->arhead->slots[slot] = data;
}

hcell hopc_unspill(hopc_runtime *r, hword_t slot) {
  return r->taskheadp->arhead->slots[slot];
}

#define BMASKWORD(i) ((i)/(sizeof(hregmask)*2))
#define BOFF(i) ((i)%(sizeof(hregmask)*2))

void hopc_gc_update_chunk_pointers(hopc_runtime *r, hcell* lb, hword_t shift, hword_t size, hcell *raw, const hregmask *mask) {
    hword_t i = 0;
    for(i = 0; i < size; i++) {
        if( BITGET(mask[BMASKWORD(i)], BOFF(i)) ) {
            raw[size].p = (hword_t*)PTRSHIFT(raw[size].p, lb, shift);
        }
    }
}

void hopc_gc_collect(hopc_runtime* r) {
    fprintf(stderr, "hopc_gc_collect\n");
    hopc_gc_mark_roots(r);
/*    hopc_gc_mark(r);*/
/*    hopc_gc_compact(r);*/
/*    hopc_gc_mark_dead(r);*/
}

void hopc_gc_mark_chunk_pointers(hopc_runtime *r, hword_t size, hcell *raw, const hregmask *mask) {
    hword_t i = 0;
    for(i = 0; i < size; i++) {
        if( BITGET(mask[BMASKWORD(i)], BOFF(i)) ) {
            fprintf(stderr, "marking slot %d as pointer\n", i);
            hopc_gc_mark_root_alive(r, (memchunk*)raw[size].p);
        }
    }
}

hopc_ar *hopc_gc_update_ar_pointers(hopc_runtime *r, hopc_task *tp, hcell *lb, hword_t shift) {
    hopc_ar *p = tp->arhead, *p2 = 0;
    while(p) {
        hcell *chunk = hopc_ar_chunk_start(r, p);
        if(p2) p2->next = (hopc_ar*)PTRSHIFT(((hcell*)p2->next), lb, shift);
        p2 = p;
        hopc_gc_update_chunk_pointers(r, lb, shift, HOPC_AR_SLOTS(p), p->slots,
                                      r->tagdata[((memchunk*)chunk)->t.tag].pmask);
        p = p->next;
    }
    return (hopc_ar*)PTRSHIFT(tp->arhead, lb, shift);
}


void hopc_gc_update_task_pointers(hopc_runtime *r, hcell *lb, hword_t shift) {
    hopc_task *p2 = 0;
    hopc_task *p = r->taskheadp;
    fprintf(stderr, "update task pointers: [0x%08X] 0x%08X %d\n", r->taskheadp, lb, shift);
    while(p) {
        if(p2) p2->next = (hopc_task*)PTRSHIFT(p2->next, lb, shift);
        p2 = p;
        p->arhead = hopc_gc_update_ar_pointers(r, p, lb, shift);
        hopc_gc_update_chunk_pointers(r, lb, shift, HOPCREGNUM, p->regs, &(p->mask));
        p = p->next;
    }
    r->taskheadp = (hopc_task*)PTRSHIFT(r->taskheadp, lb, shift);
/*    exit(-1);*/
}

hword_t hopc_gc_maxmem(hopc_runtime *r) {
    return CELLS((hword_t)(r->gc.heapend_p - r->gc.heapstart_p));
}

hword_t hopc_gc_freemem(hopc_runtime *r) {
    return CELLS((hword_t)(r->gc.heapend_p - r->gc.top));
}

hword_t hopc_gc_chunksize(hopc_runtime *r, memchunk *p) {
    return BLOCKSIZE(hopc_tagsize(r, p->t.tag));
}

memchunk* hopc_gc_prev_chunk(hopc_runtime *r, memchunk *p) {
    return  p - BLOCKSIZE(hopc_tagsize(r, p->t.tag));
}


void hopc_gc_mark_dead(hopc_runtime* r) {
    hcell *chunk = r->gc.top-1;
    for(; chunk >= r->gc.gcbottom; chunk = (hcell*)hopc_gc_prev_chunk(r, (memchunk*)chunk)) {
        ((memchunk*)chunk)->t.gc_alive = 0;
    }
}

void hopc_gc_mark(hopc_runtime* r) {
    hcell *chunk = r->gc.top-1, *lastp = 0;
    memchunk *memc = (memchunk*)chunk;
    for(; chunk >= r->gc.gcbottom; chunk = (hcell*)hopc_gc_prev_chunk(r, memc)) {
        memc = (memchunk*)chunk;
        lastp = chunk;
        if( memc->t.gc_alive ) {
            hcell *p = hopc_gc_chunk_start(r, chunk);
            hword_t ts = hopc_tagsize(r, memc->t.tag);
            if( memc->t.gc_follow ) { // TASKS AND AR's MUST BE ALREADY MARKED AT THE MOMENT
                hopc_gc_mark_chunk_pointers(r, ts, p, r->tagdata[memc->t.tag].pmask);
            }
        }
    }
}

hcell *hopc_gc_find_live(hopc_runtime *r, hcell *from, hcell *rbound) {
    hcell *chunk = from;
    memchunk *memc = (memchunk*)chunk;
    for(; chunk >= r->gc.gcbottom; chunk = (hcell*)hopc_gc_prev_chunk(r, memc)) {
        memc = (memchunk*)chunk;
        if( rbound && chunk < rbound && memc->t.gc_alive ) { 
            return chunk;
        } else if( !rbound && memc->t.gc_alive ) {
            return chunk;
        }
    }
    return (hcell*)0;
}


static hword_t hopc_gc_distance(hopc_runtime *r, hcell *left, hcell *right) {
    hcell *rp = hopc_gc_chunk_start(r, right);
    return rp > left ? rp - left - 1 : 0 ;
}

hcell* hopc_gc_calc_shift(hopc_runtime* r, hcell **leftp, hcell **rightp) {
    hcell *top = r->gc.top-1;
    hcell *left = 0;
    hcell *right = hopc_gc_find_live(r, top, 0);
  
    *leftp = 0;
    *rightp = 0;

    if( !right ) {
        r->gc.top = r->gc.gcbottom;
        return 0;
    }

    if( right < top ) {
        r->gc.top = right - 1;
    }

    left = hopc_gc_find_live(r, right, right);

    while( left &&  left > r->gc.gcbottom && !hopc_gc_distance(r, left, right) ) {
        right = left;
        left = hopc_gc_find_live(r, right, right);
    }

    if( !left ) {
        *leftp = r->gc.gcbottom;
    } else {
        *leftp = left + 1;
    }

    *rightp = hopc_gc_chunk_start(r, right);

/*    printf("l: 0x%08X r: 0x%08X\n", left, right);*/
    return left;
}

void hopc_gc_compact(hopc_runtime* r) {
    hcell *lp = 0;
    hcell *rp = 0;
    hcell *lbound = 0;
    hword_t shift = 0;
    do {
        lbound = hopc_gc_calc_shift(r, &lp, &rp);
        shift = rp > lp ? rp - lp : 0;
        if( shift ) {
            printf("l: 0x%08X r: 0x%08X s: %d lbound: 0x%08X\n", lp, rp, shift, lbound);
            hopc_gc_update_pointers(r, lbound, shift);
            memmove(lp, rp, (r->gc.top - rp)*sizeof(hcell));
            r->gc.top -= shift;
        }
/*        break;*/
    } while(shift);
}

void hopc_gc_update_pointers(hopc_runtime *r, hcell *left, hword_t shift) {
    hcell *top = r->gc.top-1;
    hcell *chunk = left;
    memchunk *memc = (memchunk*)chunk;
    hopc_gc_update_task_pointers(r, left, shift);
    for(chunk = top; chunk > left; chunk = (hcell*)hopc_gc_prev_chunk(r, memc)) {
        memc = (memchunk*)chunk;
        if( memc->t.gc_alive ) {
            hcell *p = hopc_gc_chunk_start(r, chunk);
            hword_t ts = hopc_tagsize(r, memc->t.tag);
            if( memc->t.gc_follow ) {
                hopc_gc_update_chunk_pointers(r, left, shift, ts, p, r->tagdata[memc->t.tag].pmask);
            }
        }
    }
}

void hopc_gc_mark_activation_record(hopc_runtime *r, hopc_ar *p) {
    while(p) {
        fprintf(stderr, "markup activation record 0x%08X\n", p);
        hcell *chunk = ((hcell*)p) + p->size;
        hopc_gc_mark_root_alive(r, (memchunk*)chunk);
        hopc_gc_mark_chunk_pointers(r, HOPC_AR_SLOTS(p), p->slots, r->tagdata[((memchunk*)chunk)->t.tag].pmask);
        p = p->next;
    }
}

void hopc_gc_mark_roots(hopc_runtime* r) {
    hopc_task *p = r->taskheadp;
    hcell *chunk = 0;

    fprintf(stderr, "hopc_gc_mark_roots\n");
    fprintf(stderr, "TASK HEAD: 0x%08X\n", p);

    for(; p; p = p->next ) {
        chunk = (hcell*)hopc_get_task_chunk(r, p);
        fprintf(stderr, "FOUND TASK CHUNK: 0x%08X 0x%08X\n", p, chunk);
        fprintf(stderr, "AR: 0x%08X\n", p->arhead);
        hopc_gc_mark_activation_record(r, p->arhead);
        hopc_gc_mark_root_alive(r, (memchunk*)chunk);
        hopc_gc_mark_chunk_pointers(r, HOPCREGNUM, p->regs, &(p->mask));
    }
}


void hopc_gc_mark_root_alive(hopc_runtime *runtime, memchunk *chunk) {
    if( ((hcell*)chunk) >= runtime->gc.heapstart_p 
     && ((hcell*)chunk) < runtime->gc.top ) {
        chunk->t.gc_alive = 1;
    }
}
