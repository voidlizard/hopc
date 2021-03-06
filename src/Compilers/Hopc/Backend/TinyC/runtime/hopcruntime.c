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


static inline void task_ins_head(hopc_task **head, hopc_task *task) {
    hopc_task *t = 0;
    t = *head;
    *head = task;
    task->next = t;
}

static inline hopc_task *task_del_head(hopc_task **head) {
    hopc_task *detached = 0;
    if( *head ) {
        detached = *head;
        *head = (*head)->next;
    }
    return detached;
}

static inline void task_q_init(hopc_task_q *q) {
    q->first = 0;
    q->last = &q->first;
}

static inline void task_q_push(hopc_task_q *q, hopc_task *t) {
    if( t ) {
        t->next = 0;
        *q->last = t;
        q->last = &t->next; 
    }
}

static inline void task_q_push_front(hopc_task_q *q, hopc_task *t) {
    if( t && !(t->next = q->first) )
        q->last = &t->next;
    q->first = t;
}

static hopc_task *task_q_pop(hopc_task_q *q) {
    hopc_task *tmp = q->first;
    if( tmp && !(q->first = q->first->next) ) {
        q->last = &q->first;
    }
    return tmp;
}

static void task_sweep(hopc_task *t) {
    if(t) {
        t->arhead = 0;
        t->mask = 0;
        t->id.t.state = HOPC_TASK_TO_DELETE;
    }
}

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
    task_q_init(&runtime->workers);
    task_q_init(&runtime->sleepers);
    runtime->taskid = 0;
    runtime->taskqt = HOPCTASKQT;
    runtime->tasktime = runtime->taskqt;
    runtime->idle = 0;
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
/*        fprintf(stderr, "allocated: %08X %d %d %04X free: %d\n", chunk, size, hopc_tagsize(runtime, tag), tag, (runtime->gc.heapend_p - runtime->gc.top));*/
        return (hcell*)chunk;
    }
    hopc_out_of_mem_hook(runtime);
    return (hcell*)0;
}

memchunk *hopc_get_task_chunk(hopc_runtime *r, hopc_task *t) {
    return ((memchunk*)t + hopc_tagsize(r, HOPCTASKTAG));
}

hopc_task *hopc_find_task(hopc_runtime *runtime, hword_t id) {
    hopc_task *p = runtime->workers.first;
    for(; p && p->id.t.id != id; p = p->next );
    if( p && p->id.t.id == id ) return p;
    p = runtime->sleepers.first;
    for(; p && p->id.t.id != id; p = p->next );
    if( p && p->id.t.id == id ) return p;
    return 0;
}

hopc_task *hopc_insert_task(hopc_runtime *runtime) {
    hcell *chunk = hopc_gc_alloc_chunk(runtime, HOPCTASKTAG);
    hopc_task *t = 0;
    ((memchunk*)chunk)->t.gc_follow = 0;
    t = (hopc_task*)hopc_gc_chunk_start(runtime, (hcell*)chunk);

    hopc_gc_mark_root_alive(runtime, ((memchunk*)chunk));

    t->id.t.id = ++runtime->taskid;
    t->id.t.state = HOPC_TASK_ALIVE;
    t->tsleep = 0;
    t->tsleep_since = 0;
    t->arhead = 0;
    memset(t->regs, 0, sizeof(hcell)*HOPCREGNUM);
    t->mask = 0;
 
/*    task_ins_head(&runtime->tasks, t);*/
    task_q_push_front(&runtime->workers, t);

    return t;
}

void hopc_detach_current(hopc_runtime *runtime) {
    if( CURRENT(runtime) ) {
        task_sweep(CURRENT(runtime));
/*        fprintf(stderr, "DETACH TASK %08X\n", CURRENT(runtime));*/
        runtime->tasktime = 0;
    }
}

void hopc_delete_task(hopc_runtime *runtime, hword_t id) {
    hopc_task *t = hopc_find_task(runtime, id);
    task_sweep(t);
}

void hopc_ffi__sleep(hopc_runtime *runtime, hcell cell) {
    if( CURRENT(runtime) ) {
        CURRENT(runtime)->tsleep_since = hopc_getcputime(runtime);
        CURRENT(runtime)->tsleep = HOPC_TICKS_OF_MILLIS(W(cell));
        runtime->tasktime = 0;
    }
}

// hcell --- closure
void hopc_spawn_task(hopc_runtime *runtime, hcell cell) {
  hopc_task *tp = hopc_insert_task(runtime);
  hopc_closure *cp = (hopc_closure*)hopc_gc_chunk_start(runtime, (hcell*)P(cell));
  tp->regs[0] = cp->cp;
  tp->arhead = (hopc_ar*)hopc_gc_chunk_start(runtime, (hcell*)P(cp->ar));
  runtime->tasktime = runtime->taskqt;
/*  fprintf(stderr, "SPAWN %08X R0: %d\n", tp, W(tp->regs[0]));*/
}


void hopc_switch_task(hopc_runtime *rt, htime_t delta) {
    rt->tasktime = rt->taskqt;
    rt->idle = 0;

    if( rt->workers.first && rt->workers.first->id.t.state == HOPC_TASK_TO_DELETE ) {
        rt->workers.first->id.t.state = HOPC_TASK_DELETED;
        task_q_pop(&rt->workers);
    }

    if( rt->sleepers.first && rt->sleepers.first->id.t.state == HOPC_TASK_TO_DELETE ) {
        rt->sleepers.first->id.t.state = HOPC_TASK_DELETED;
        task_q_pop(&rt->sleepers);
    }

    if( !rt->workers.first && rt->sleepers.first ) {
/*        sleep(1);*/
/*        fprintf(stderr, "IDLE!\n");*/
    }

    if( rt->sleepers.first ) {
/*        fprintf(stderr, "SLEEP\n");*/
        htime_t sleep = rt->sleepers.first->tsleep;
        htime_t since = rt->sleepers.first->tsleep_since;
        htime_t now = hopc_getcputime(rt);
        htime_t delta = since > now ? ((now - since) & HTIME_MAX) : now - since;
/*        fprintf(stderr, "sleep %d since %d now %d delta: %d\n", sleep, since, now, delta);*/
        if( delta >= sleep ) {
/*            fprintf(stderr, "EXPIRED!\n");*/
/*            exit(-1);*/
            rt->sleepers.first->tsleep = 0;
            rt->sleepers.first->tsleep_since = 0;
            task_q_push_front(&rt->workers, task_q_pop(&rt->sleepers));
            return;
        }
        task_q_push(&rt->sleepers, task_q_pop(&rt->sleepers));
    }

    do {
        if( rt->workers.first ) {
            if( rt->workers.first->tsleep ) {
                task_q_push(&rt->sleepers, task_q_pop(&rt->workers));
                rt->tasktime = 0;
/*                fprintf(stderr, "GO TO SLEEP: %d\n", rt->workers.first->id.t.id);*/
            } else {
                task_q_push(&rt->workers, task_q_pop(&rt->workers));
            }
        }
    } while( rt->workers.first && rt->workers.first->tsleep );

    if( !rt->workers.first && rt->sleepers.first ) {
        htime_t idle = rt->sleepers.first->tsleep;
        hopc_system_sleep( rt->sleepers.first->tsleep );
        rt->tasktime = 0;
        hopc_system_sleep(idle < HOPC_SCHEDULER_IDLE_TICKS?idle:HOPC_SCHEDULER_IDLE_TICKS);
    }

/*    fprintf(stderr, "task switched %d w: %08X s: %08X idle: %d\n", rt->workers.first->id.t.id, rt->workers.first, rt->sleepers.first, rt->idle)*/

}

memchunk *hopc_make_activation_record(hopc_runtime *runtime, htag tag) {
    hcell *chunk = hopc_gc_alloc_chunk(runtime, tag);
    hopc_ar *arp = 0;
    arp = (hopc_ar*)hopc_gc_chunk_start(runtime, chunk);
    arp->size = hopc_tagsize(runtime, tag);
    memset(arp->slots, 0, sizeof(hword_t)*HOPC_AR_SLOTS(arp));
    ((memchunk*)chunk)->t.gc_follow = 0;
    return (memchunk*)chunk;
}

hopc_ar *hopc_push_activation_record2(hopc_runtime *runtime, memchunk *chunk) {
    hopc_ar *arp = (hopc_ar*)hopc_gc_chunk_start(runtime, (hcell*)chunk);
    if( CURRENT(runtime) ) {
        arp->next = CURRENT(runtime)->arhead;
        CURRENT(runtime)->arhead = arp;
    }
    return arp;
}

hopc_ar *hopc_push_activation_record(hopc_runtime *runtime, htag tag) {
    memchunk *chunk = hopc_make_activation_record(runtime, tag);
    return hopc_push_activation_record2(runtime, chunk);
}

void hopc_pop_activation_record(hopc_runtime *r) {
    hopc_ar *arp = CURRENT(r) ? CURRENT(r)->arhead : 0;
    if( arp ) {
        CURRENT(r)->arhead = arp->next;
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

void hopc_fix_closure(hopc_runtime *runtime, hword_t label, hcell *chunk) {
    hopc_closure *closp = (hopc_closure*)hopc_gc_chunk_start(runtime, chunk);
    hopc_ar *arp = (hopc_ar*)hopc_gc_chunk_start(runtime, (hcell*)closp->ar.p);
/*    fprintf(stderr, "FIXING CLOSURE: %d\n", label);*/
    arp->slots[0].w = label;
}

void hopc_spill(hopc_runtime *r, hword_t slot, hcell data) {
    CURRENT(r)->arhead->slots[slot] = data;
}

hcell hopc_unspill(hopc_runtime *r, hword_t slot) {
    return CURRENT(r)->arhead->slots[slot];
}

#define BMASKWORD(i) ((i)/(sizeof(hregmask)*2))
#define BOFF(i) ((i)%(sizeof(hregmask)*2))

void hopc_gc_update_chunk_pointers(hopc_runtime *r, hcell* lb, hword_t shift, hword_t size, hcell *raw, const hregmask *mask) {
    hword_t i = 0;
    for(i = 0; i < size; i++) {
        if( BITGET(mask[BMASKWORD(i)], BOFF(i)) ) {
            raw[i].p = (hword_t*)PTRSHIFT(raw[i].p, lb, shift);
        }
    }
}

void hopc_gc_collect(hopc_runtime* r) {
/*    dump_tasks(r);*/
/*    fprintf(stderr, "\nCURRENT %08X\n", CURRENT(r));*/
    hopc_gc_mark_roots(r);
/*    fprintf(stderr, "*** SURVIVED MARK ROOTS ****\n");*/
    hopc_gc_mark(r);
/*    fprintf(stderr, "*** SURVIVED MARK ****\n");*/
    hopc_gc_compact(r);
/*    fprintf(stderr, "*** SURVIVED GC ****\n");*/
    hopc_gc_mark_dead(r);
/*    dump_tasks(r);*/
/*    fprintf(stderr, "\nCURRENT %08X\n", CURRENT(r));*/
    r->tasktime = 0;
}

void hopc_gc_mark_chunk_pointers(hopc_runtime *r, hword_t size, hcell *raw, const hregmask *mask) {
    hword_t i = 0;
    for(i = 0; i < size; i++) {
        if( BITGET(mask[BMASKWORD(i)], BOFF(i)) ) {
            hopc_gc_mark_root_alive(r, (memchunk*)(raw[i].p));
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
/*    fprintf(stderr, "hopc_gc_update_task_pointers\n");*/
    hopc_task *p2 = 0;
    hopc_task *p = r->workers.first;
    while(p) {
        if(p2) p2->next = (hopc_task*)PTRSHIFT(p2->next, lb, shift);
        p2 = p;
        p->arhead = hopc_gc_update_ar_pointers(r, p, lb, shift);
        hopc_gc_update_chunk_pointers(r, lb, shift, HOPCREGNUM, p->regs, &(p->mask));
        p = p->next;
    }
    p = r->sleepers.first;
    while(p) {
        if(p2) p2->next = (hopc_task*)PTRSHIFT(p2->next, lb, shift);
        p2 = p;
        p->arhead = hopc_gc_update_ar_pointers(r, p, lb, shift);
        hopc_gc_update_chunk_pointers(r, lb, shift, HOPCREGNUM, p->regs, &(p->mask));
        p = p->next;
    }

    r->workers.first  = (hopc_task*)PTRSHIFT(r->workers.first, lb, shift);
    *r->workers.last  = (hopc_task*)PTRSHIFT(*(r->workers.last), lb, shift);
    r->sleepers.first = (hopc_task*)PTRSHIFT(r->sleepers.first, lb, shift);
    *r->sleepers.last = (hopc_task*)PTRSHIFT(*(r->sleepers.last), lb, shift);
/*    fprintf(stderr, "hopc_gc_update_task_pointers done\n");*/
/*    task_q_init(&r->workers);*/
/*    task_q_init(&r->sleepers);*/
/*    r->workers.first = 0;*/
/*    r->s*/
}

hword_t hopc_gc_maxmem(hopc_runtime *r) {
    return (hword_t)(r->gc.heapend_p - r->gc.heapstart_p);
}

hword_t hopc_gc_freemem(hopc_runtime *r) {
    return (hword_t)(r->gc.heapend_p - r->gc.top);
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
    hcell *left = r->gc.gcbottom;
    hcell *right = hopc_gc_find_live(r, top, 0);
  
    *leftp = 0;
    *rightp = 0;

    if( !right ) { // No compacting required
/*        fprintf(stderr, "No compacting required\n");*/
        r->gc.top = r->gc.gcbottom;
        return r->gc.heapend_p;
    }

    if( right < top ) {
        r->gc.top = right + 1;
    }

    left = hopc_gc_find_live(r, right, right);

    while( left &&  left > r->gc.gcbottom && !hopc_gc_distance(r, left, right) ) {
        right = left;
        left = hopc_gc_find_live(r, right, right);
    }

/*    fprintf(stderr, "LEFT: %08X\n", left);*/

    if( !left ) {
        *leftp = r->gc.gcbottom;
        left = r->gc.gcbottom;
    } else {
        *leftp = left + 1;
    }

    *rightp = hopc_gc_chunk_start(r, right);

/*    fprintf(stderr, "hopc_gc_calc_shift: l: 0x%08X r: 0x%08X\n", left, right);*/
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
/*            printf("l: 0x%08X r: 0x%08X s: %d lbound: 0x%08X\n", lp, rp, shift, lbound);*/
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
/*        fprintf(stderr, "markup activation record 0x%08X\n", p);*/
        hcell *chunk = ((hcell*)p) + p->size;
        hopc_gc_mark_root_alive(r, (memchunk*)chunk);
        hopc_gc_mark_chunk_pointers(r, HOPC_AR_SLOTS(p), p->slots, r->tagdata[((memchunk*)chunk)->t.tag].pmask);
        p = p->next;
    }
}

void hopc_gc_mark_roots(hopc_runtime* r) {
/*    fprintf(stderr, "markup roots\n");*/
    hopc_task *p = r->workers.first;
    hopc_task *tmp = *r->workers.last;
    *r->workers.last = r->sleepers.first;
    hcell *chunk = 0;
    for(; p; p = p->next ) {
/*        fprintf(stderr, "markup task %08X\n", p);*/
        chunk = (hcell*)hopc_get_task_chunk(r, p);
        hopc_gc_mark_activation_record(r, p->arhead);
        hopc_gc_mark_root_alive(r, (memchunk*)chunk);
        hopc_gc_mark_chunk_pointers(r, HOPCREGNUM, p->regs, &(p->mask));
    }
    *r->workers.last = tmp; 
}

void hopc_gc_mark_root_alive(hopc_runtime *runtime, memchunk *chunk) {
    if( ((hcell*)chunk) >= runtime->gc.heapstart_p 
     && ((hcell*)chunk) < runtime->gc.top ) {
        chunk->t.gc_alive = 1;
/*        fprintf(stderr, "hopc_gc_mark_root_alive %08X\n", (unsigned long)chunk);*/
    }
}


void hopc_debug_runtime(hopc_runtime* runtime) {
#ifdef HOPC_DEBUG_LEVEL6
    fprintf(stderr, "\r free memory: %8d  total memory: %8d                 ", 
                    hopc_gc_freemem(runtime),
                    hopc_gc_maxmem(runtime));
#endif
}

void dump_tasks(hopc_runtime *rt) {
    hopc_task *p = rt->workers.first;
    hopc_ar *arp = 0;
    for(; p; p = p->next ){ 
        hcell *chunk = (hcell*)hopc_get_task_chunk(rt, p);
        fprintf(stderr, "w.task %08X %08X [%d]\n", chunk, p, p->id.t.id);
        for( arp = p->arhead; arp; arp = arp->next ) {
            fprintf(stderr, "  ar %08X %08X\n", (hcell*)arp + arp->size, arp);
        }
    }
    p = rt->sleepers.first;
    for(; p; p = p->next ) {
        hcell *chunk = (hcell*)hopc_get_task_chunk(rt, p);
        fprintf(stderr, "w.task %08X %08X [%d]\n", chunk, p, p->id.t.id);
        for( arp = p->arhead; arp; arp = arp->next ) {
            fprintf(stderr, "  ar %08X %08X\n", (hcell*)arp + arp->size, arp);
        }
    }
}


void dump_heap3(hopc_runtime *runtime) {
    hcell *chunk = 0;
    memchunk *memc = 0;
    for(chunk = runtime->gc.top - 1; chunk >= runtime->gc.heapstart_p;
                                     chunk = (hcell*)hopc_gc_prev_chunk(runtime, memc)) {
        memc = (memchunk*)chunk;
        fprintf(stderr, "; 0x%08X %s %d %d %04X\n", chunk,
                                           (memc->t.gc_alive?"alive":"dead"),
                                           hopc_tagsize(runtime, memc->t.tag),
                                           hopc_gc_chunksize(runtime, memc),
                                           memc->t.tag);
    }
}


