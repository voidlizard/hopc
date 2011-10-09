
#define HOPCREGNUM 16

#include "hopcruntime.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

hcell heap[1024];

#define ROOTS 63 

void dump_heap3(hopc_runtime *runtime);
void dump_heap_raw(hopc_runtime *runtime);

#define TUPLE1 HOPCTASKTAG + 1
#define TUPLE2 TUPLE1 + 1
#define TUPLE3 TUPLE2 + 1
#define TUPLE4 TUPLE3 + 1

// supposed to be generated automatically
const hopc_tagdata tagdata[] = {
     {CELLS(sizeof(hopc_task)), {0}}
    ,{1, {0}} // 1-tuple
    ,{2, {0}} // 2-tuple
    ,{3, {0}} // 3-tuple
    ,{4, {0}} // 4-tuple
};


int main() {
    static hopc_runtime runtime = { .tagdata = tagdata };
    hword_t total = 0;
    hopc_task *taskp = 0;
    hopc_task *taskp2 = 0;
    hword_t tid = 0;
    hcell *p = 0;

    printf("hcell size: %d %d\n", sizeof(hcell), CELLS(sizeof(hcell)));
    printf("hopc_task: %d %d\n", sizeof(hopc_task), hopc_tagsize(&runtime, HOPCTASKTAG));
    printf("HOPC_AR_HEAD: %d\n", HOPC_AR_HEAD);
    printf("hopc_closure: %d %d\n", sizeof(hopc_closure), CELLS(sizeof(hopc_closure)));
    printf("long long: %d\n", sizeof(long long int));
    printf("htime_t: %d\n", sizeof(htime_t));

/*    dump_heap_raw(&runtime);*/

    hopc_init_runtime(&runtime, heap, (sizeof(heap)/sizeof(hcell)));
    srand(time(0));

    printf("cputime: %d\n", hopc_getcputime());

    exit(0);

    total = hopc_gc_maxmem(&runtime) - hopc_gc_freemem(&runtime);
    printf("; top: 0x%08X end: 0x%08X, total: %d, free: %d\n", runtime.gc.top, runtime.gc.heapend_p, total, hopc_gc_freemem(&runtime));

    hopc_gc_alloc_chunk(&runtime, TUPLE4);

    hopc_insert_task(&runtime);

    hopc_gc_alloc_chunk(&runtime, TUPLE4);

    hopc_gc_alloc_chunk(&runtime, TUPLE2);

    hopc_insert_task(&runtime);

    tid = runtime.taskheadp->id;

    runtime.taskheadp->regs[1].p = (hword_t*)hopc_gc_alloc_chunk(&runtime, TUPLE3);
    p = hopc_gc_chunk_start(&runtime, (hcell*)runtime.taskheadp->regs[1].p);
    p[0].w = 0xDEAD;
    p[1].w = 0xBEEF;
    p[2].w = 0xCAFE;
    runtime.taskheadp->mask = 2;
    fprintf(stderr, "initialized a tuple to survive the gc %08X\n", runtime.taskheadp->regs[1].w);

    hopc_insert_task(&runtime);

/*    dump_heap3(&runtime);*/

    dump_heap3(&runtime);

    total = hopc_gc_maxmem(&runtime) - hopc_gc_freemem(&runtime);

    taskp2 = hopc_find_task(&runtime, tid);
    p = hopc_gc_chunk_start(&runtime, taskp2->regs[1].p);
    fprintf(stderr, "%08X\n", p[0].w);
    fprintf(stderr, "%08X\n", p[1].w);
    fprintf(stderr, "%08X\n", p[2].w);

    printf("; top: 0x%08X end: 0x%08X, total: %d, free: %d\n", runtime.gc.top, runtime.gc.heapend_p, total, hopc_gc_freemem(&runtime));

/*    dump_heap_raw(&runtime);*/

    hopc_gc_collect(&runtime);
    dump_heap3(&runtime);

/*    dump_heap_raw(&runtime);*/

    taskp2 = hopc_find_task(&runtime, tid);
    p = hopc_gc_chunk_start(&runtime, taskp2->regs[1].p);
    fprintf(stderr, "%08X\n", p[0].w);
    fprintf(stderr, "%08X\n", p[1].w);
    fprintf(stderr, "%08X\n", p[2].w);

    printf("; top: 0x%08X end: 0x%08X, total: %d, free: %d\n", runtime.gc.top, runtime.gc.heapend_p, total, hopc_gc_freemem(&runtime));

    hopc_gc_collect(&runtime);

    dump_heap3(&runtime);

    taskp2 = hopc_find_task(&runtime, tid);
    p = hopc_gc_chunk_start(&runtime, taskp2->regs[1].p);
    fprintf(stderr, "%08X\n", p[0].w);
    fprintf(stderr, "%08X\n", p[1].w);
    fprintf(stderr, "%08X\n", p[2].w);

    printf("; top: 0x%08X end: 0x%08X, total: %d, free: %d\n", runtime.gc.top, runtime.gc.heapend_p, total, hopc_gc_freemem(&runtime));

    return 0;

}

void dump_heap_raw(hopc_runtime *runtime) {
    hcell *cp = runtime->gc.heapstart_p;
    hcell *endp = runtime->gc.heapend_p;
    unsigned int i = 0;
    fprintf(stdout, "\n");
    for(; cp < endp; cp++, i++ ) {
        if( !(i % 16) ) { fprintf(stdout, "\n"); }
        fprintf(stdout, "%08X ", (unsigned int)cp->w);
    }
    fprintf(stdout, "\n");
}

