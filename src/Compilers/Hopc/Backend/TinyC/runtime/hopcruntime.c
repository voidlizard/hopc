#include <hopcruntime.h>

#include <string.h>
#include <stdlib.h>

#define BLOCKSIZE(n) ((n) + WORDS(sizeof(memchunk)))

#define PTRSHIFT(p, l, n) ((p)>(l) ? (((hword_t*)(p))-(n)) : (p))

void hopc_gc_update_task_pointers(hopc_runtime*, hword_t*, hword_t);
void hopc_gc_update_chunk_pointers(hopc_runtime*, hword_t* lb, hword_t shift, hword_t, hword_t*, const unsigned char *);
hopc_ar *hopc_gc_update_ar_pointers(hopc_runtime*, hopc_task*, hword_t*, hword_t);
void hopc_gc_mark_chunk_pointers(hopc_runtime*, hword_t, hword_t*, const unsigned char*);
void hopc_gc_mark_activation_record(hopc_runtime*, hopc_ar*);

void hopc_gc_init(hopc_gc *gc, hword_t *mem, hword_t size) {
    gc->heapstart_p = mem;
    gc->heapend_p = mem + size;
    gc->top = gc->heapstart_p;
    gc->gcbottom =  gc->heapstart_p;
}

void hopc_init_runtime(hopc_runtime *runtime, hword_t *mem, hword_t size) {
    hopc_gc_init(&(runtime->gc), mem, size);
    runtime->taskheadp = 0;
    runtime->taskid = 0;
}

hopc_task *hopc_insert_task(hopc_runtime *runtime) {
    hword_t *chunk = hopc_gc_alloc_chunk(runtime, HOPCTASKTAG);
    hopc_task *t = 0;
    if( !chunk ) {
        hopc_out_of_mem_hook(runtime);
        return 0; // FIXME: out of mem
    }
    ((memchunk*)chunk)->t.gc_follow = 0;
    t = (hopc_task*)hopc_gc_chunk_start(runtime, chunk);
    t->id = runtime->taskid++;
    t->next = runtime->taskheadp;
    runtime->taskheadp = t;
    printf("ALLOC TASK CHUNK: 0x%08X 0x%08X\n", t, chunk);
    hopc_gc_mark_root_alive(runtime, chunk);
    t->arhead = 0;
    memset(t->regs, 0, sizeof(hword_t)*HOPCREGNUM);
    memset(t->mask, 0, HOPCTASKMASKSIZE);
    return t;
}

hopc_task *hopc_find_task(hopc_runtime *runtime, hword_t id) {
    hopc_task *p = runtime->taskheadp;
    for(; p && p->id != id; p = p->next );
    if( p && p->id == id ) return p;
    return 1;
}

hword_t *hopc_get_task_chunk(hopc_runtime *r, hopc_task *t) {
    return ((hword_t*)t + hopc_tagsize(r, HOPCTASKTAG));
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

hopc_ar *hopc_push_activation_record(hopc_runtime *runtime, htag tag) {
    hword_t *chunk = hopc_gc_alloc_chunk(runtime, tag);
    hopc_ar *arp = 0;
    if( !chunk ) {
        hopc_out_of_mem_hook(runtime);
        return 0;
    }
    arp = (hopc_ar*)hopc_gc_chunk_start(runtime, chunk);
    arp->size = hopc_tagsize(runtime, tag);
    memset(arp->slots, 0, sizeof(hword_t)*(arp->size-2));
    ((memchunk*)chunk)->t.gc_follow = 0;
    if( runtime->taskheadp ) {
        arp->next = runtime->taskheadp->arhead;
        runtime->taskheadp->arhead = arp;
    }
    return arp;
}

void hopc_pop_activation_record(hopc_runtime *r) {
    hopc_ar *arp = r->taskheadp ? r->taskheadp->arhead : 0;
    if( arp ) {
        r->taskheadp->arhead = arp->next;
    }
}

void hopc_spill(hopc_runtime *r, hword_t slot, hword_t data) {
  r->taskheadp->arhead->slots[slot] = data;
}

hword_t hopc_unspill(hopc_runtime *r, hword_t slot) {
  return r->taskheadp->arhead->slots[slot];
}

void hopc_gc_update_chunk_pointers(hopc_runtime *r, hword_t* lb, hword_t shift, hword_t size, hword_t *raw, const unsigned char *mask) {
    while(--size) {
        if( BITGET(mask, size) ) {
            raw[size] = (hword_t)(PTRSHIFT(((hword_t*)raw[size]), lb, shift));
        }
    }
}

void hopc_gc_mark_chunk_pointers(hopc_runtime *r, hword_t size, hword_t *raw, const unsigned char *mask) {
    while(--size) {
        if( BITGET(mask, size) ) {
            hopc_gc_mark_root_alive(r, (hword_t*)raw[size]);
        }
    }
}

hopc_ar *hopc_gc_update_ar_pointers(hopc_runtime *r, hopc_task *tp, hword_t *lb, hword_t shift) {
    hopc_ar *p = tp->arhead, *p2 = 0;
    while(p) {
        hword_t *chunk = ((hword_t*)p) + p->size;
        if(p2) p2->next = (hopc_ar*)PTRSHIFT(((hword_t*)p2->next), lb, shift);
        p2 = p;
        hopc_gc_update_chunk_pointers(r, lb, shift, p->size-2, p->slots, r->tagdata[((memchunk*)chunk)->t.tag].pmask); // FIXME: hardcode
        p = p->next;
    }
    return (hopc_ar*)PTRSHIFT(((hword_t*)tp->arhead), lb, shift);
}

void hopc_gc_update_task_pointers(hopc_runtime *r, hword_t *lb, hword_t shift) {
    hopc_task *p2 = 0;
    hopc_task *p = r->taskheadp;
    printf("update task pointers: [0x%08X] 0x%08X %d\n", r->taskheadp, lb, shift);
    while(p) {
        if(p2) p2->next = (hopc_task*)PTRSHIFT(((hword_t*)p2->next), lb, shift);
        p2 = p;
        p->arhead = hopc_gc_update_ar_pointers(r, p, lb, shift);
        hopc_gc_update_chunk_pointers(r, lb, shift, HOPCREGNUM, p->regs, p->mask);
        p = p->next;
    }
    r->taskheadp = (hopc_task*)PTRSHIFT(((hword_t*)r->taskheadp), lb, shift);
/*    exit(-1);*/
}

hword_t *hopc_gc_alloc_chunk(hopc_runtime *runtime, htag tag) {
    hword_t *top = runtime->gc.top;
    hword_t *end = runtime->gc.heapend_p;
    hword_t ts = hopc_tagsize(runtime, tag);
    hword_t size = BLOCKSIZE(ts);
    memchunk *chunk = 0;
    if( end - top >= size ) {
        runtime->gc.top += size;
        chunk = (memchunk*)(top + ts);
        chunk->t.gc_alive = 0;
        chunk->t.gc_follow = 1;
        chunk->t.tag = tag;
/*        printf("allocated: %08X %d %d %04X\n", (hword_t*)chunk, size, hopc_tagsize(runtime, tag), tag);*/
        return (hword_t*)chunk;
    }
    return (hword_t*)0;
}

void hopc_gc_mark_root_alive(hopc_runtime *runtime, hword_t *r) {
    memchunk *chunk = (memchunk*)r;
    if( r >= runtime->gc.heapstart_p && r < runtime->gc.top ) {
        chunk->t.gc_alive = 1;
    }
}

hword_t hopc_gc_maxmem(hopc_runtime *r) {
    return r->gc.heapend_p - r->gc.heapstart_p;
}

hword_t hopc_gc_freemem(hopc_runtime *r) {
    return r->gc.heapend_p - r->gc.top;
}

hword_t hopc_gc_chunksize(hopc_runtime *r, hword_t *p) {
    htag tag = ((memchunk*)p)->t.tag;
    hword_t sz = BLOCKSIZE(hopc_tagsize(r, tag));
/*    printf("chunksize: 0x%08X %d %d %04X\n", p, sz, hopc_tagsize(r, tag), tag);*/
    return sz; 
}

hword_t* hopc_gc_prev_chunk(hopc_runtime *r, hword_t *p) {
    htag tag = ((memchunk*)p)->t.tag;
    return  p - BLOCKSIZE(hopc_tagsize(r, tag));
}

hword_t hopc_tagsize(hopc_runtime *r, htag tag) {
    switch(tag) {
        case HOPCTASKTAG:
            return sizeof(hopc_task);
    }
    return r->tagdata[tag].size;
}

void hopc_gc_mark_dead(hopc_runtime* r) {
    hword_t *chunk = 0;
    for(chunk = r->gc.top-1; chunk >= r->gc.gcbottom; chunk = hopc_gc_prev_chunk(r, chunk)) {
        ((memchunk*)chunk)->t.gc_alive = 0;
    }
}

void hopc_gc_mark(hopc_runtime* r) {
    hword_t *chunk = 0, *lastp = 0;
    memchunk *memc;
    for(chunk = r->gc.top-1; chunk >= r->gc.gcbottom; chunk = hopc_gc_prev_chunk(r, chunk)) {
        memc = (memchunk*)chunk;
        lastp = chunk;
        if( memc->t.gc_alive ) {
            hword_t *p = hopc_gc_chunk_start(r, chunk);
            hword_t ts = hopc_tagsize(r, memc->t.tag);
            if( memc->t.gc_follow ) { // TASKS AND AR's MUST BE ALREADY MARKED AT THE MOMENT
                hopc_gc_mark_chunk_pointers(r, ts, chunk, r->tagdata[memc->t.tag].pmask);
            }
        }
    }
}

hword_t *hopc_gc_find_live(hopc_runtime *r, hword_t *from, hword_t *rbound) {
    hword_t *chunk = from;
    memchunk *memc;
    for(chunk = from; chunk >= r->gc.gcbottom; chunk = hopc_gc_prev_chunk(r, chunk)) {
        memc = (memchunk*)chunk;
        if( rbound && chunk < rbound && memc->t.gc_alive ) { 
            return chunk;
        } else if( !rbound && memc->t.gc_alive ) {
            return chunk;
        }
    }
    return (hword_t*)0;
}

hword_t *hopc_gc_chunk_start(hopc_runtime *r, hword_t *p) {
    return p - hopc_tagsize(r, ((memchunk*)p)->t.tag);
}

static hword_t hopc_gc_distance(hopc_runtime *r, hword_t *left, hword_t *right) {
    hword_t *rp = hopc_gc_chunk_start(r, right);
    return rp > left ? rp - left - 1 : 0 ;
}

hword_t* hopc_gc_calc_shift(hopc_runtime* r, hword_t **leftp, hword_t **rightp) {
    hword_t *top = r->gc.top-1;
    hword_t *left = 0;
    hword_t *right = hopc_gc_find_live(r, top, 0);
  
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
    hword_t *lp = 0;
    hword_t *rp = 0;
    hword_t *lbound = 0;
    hword_t shift = 0;
    do {
        lbound = hopc_gc_calc_shift(r, &lp, &rp);
        shift = rp > lp ? rp - lp : 0;
        if( shift ) {
            printf("l: 0x%08X r: 0x%08X s: %d lbound: 0x%08X\n", lp, rp, shift, lbound);
            hopc_gc_update_pointers(r, lbound, shift);
            memmove(lp, rp, (r->gc.top - rp)*sizeof(hword_t));
            r->gc.top -= shift;
        }
/*        break;*/
    } while(shift);
}

void hopc_gc_update_pointers(hopc_runtime *r, hword_t *left, hword_t shift) {
    hword_t *top = r->gc.top-1;
    hword_t *chunk = left;
    memchunk *memc;
    hopc_gc_update_task_pointers(r, left, shift);
    for(chunk = top; chunk > left; chunk = hopc_gc_prev_chunk(r, chunk)) {
        memc = (memchunk*)chunk;
        if( memc->t.gc_alive ) {
            hword_t *p = hopc_gc_chunk_start(r, chunk);
            hword_t ts = hopc_tagsize(r, memc->t.tag);
            if( memc->t.gc_follow ) {
                hopc_gc_update_chunk_pointers(r, left, shift, ts, p, r->tagdata[memc->t.tag].pmask);
            }
        }
    }
}

void hopc_gc_mark_activation_record(hopc_runtime *r, hopc_ar *p) {
    while(p) {
        hword_t *chunk = ((hword_t*)p) + p->size;
        hopc_gc_mark_root_alive(r, chunk);
        hopc_gc_mark_chunk_pointers(r, p->size-2, p->slots, r->tagdata[((memchunk*)chunk)->t.tag].pmask); // FIXME: hardcoded value 2
        p = p->next;
    }
}

void hopc_gc_mark_roots(hopc_runtime* r) {
    hopc_task *p = r->taskheadp;
    hword_t *chunk = 0;
    for(; p; p = p->next ) {
        chunk = hopc_get_task_chunk(r, p);
        printf("FOUND TASK CHUNK: 0x%08X 0x%08X\n", p, chunk);
        hopc_gc_mark_activation_record(r, p->arhead);
        hopc_gc_mark_root_alive(r, chunk);
        hopc_gc_mark_chunk_pointers(r, HOPCREGNUM, p->regs, p->mask);
    }
}

void hopc_gc_collect(hopc_runtime* r) {
    hopc_gc_mark_roots(r);
    hopc_gc_mark(r);
    hopc_gc_compact(r);
    hopc_gc_mark_dead(r);
}


void hopc_out_of_mem_hook(hopc_runtime *r) {
    exit(-1); // FIXME
}


