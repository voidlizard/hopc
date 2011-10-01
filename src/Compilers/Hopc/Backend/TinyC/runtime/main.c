
#define HOPCREGNUM 16

#include "hopcruntime.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

hword_t heap[2*1024];

#define ROOTS 63 

void dump_heap(hopc_runtime *r, hword_t num, hword_t **roots);
void dump_heap2(hopc_runtime *runtime, hword_t num, hword_t **roots);
void dump_heap3(hopc_runtime *runtime, hword_t num, hword_t **roots);

#define TUPLE1 HOPCTASKTAG
#define TUPLE2 TUPLE1 + 1
#define TUPLE3 TUPLE2 + 1
#define TUPLE4 TUPLE3 + 1

// supposed to be generated automatically
const hopc_tagdata tagdata[] = {
     {WORDS(sizeof(hopc_task)), {0}}
    ,{1, {0}} // 1-tuple
    ,{2, {0}} // 2-tuple
    ,{3, {0}} // 3-tuple
    ,{4, {0}} // 4-tuple
};

int main() {
    static hopc_runtime runtime = { .tagdata = tagdata };
    hword_t mem[10];

    hword_t *roots[ROOTS+1] = {0};
    int types[] = {TUPLE1, TUPLE2, TUPLE3, TUPLE4};

    int r = ROOTS, i = 0, j = 0, tag = 0;

    memchunk *memc = 0;
    hword_t *chunk = 0, *chunk2 = 0;
    hword_t len = 0;
    hword_t total = 0;
    hword_t tmp = 0;
    hword_t tmp1 = 0;
    hopc_task *taskp = 0;
    hopc_task *taskp2 = 0;
    int m = ROOTS/2;

    srand(time(0));

    hopc_init_runtime(&runtime, heap, (sizeof(heap)/sizeof(hword_t)));

    printf("sizeof(memchunk): %d\n", sizeof(memchunk)/sizeof(hword_t));
    printf("sizeof(hopc_task) (bytes): %d\n", sizeof(hopc_task));

    total = hopc_gc_maxmem(&runtime) - hopc_gc_freemem(&runtime);
    printf("top: 0x%08X end: 0x%08X, total: %d, free: %d\n", runtime.gc.top, runtime.gc.heapend_p, total, hopc_gc_freemem(&runtime));

    hopc_insert_task(&runtime);
    hopc_insert_task(&runtime);

    hopc_gc_alloc_chunk(&runtime, TUPLE1);

    hopc_insert_task(&runtime);
    hopc_insert_task(&runtime);
    taskp2 = hopc_insert_task(&runtime);
    hopc_insert_task(&runtime);

    hopc_gc_alloc_chunk(&runtime, TUPLE2);

    hopc_insert_task(&runtime);

    hopc_gc_alloc_chunk(&runtime, TUPLE4);

    hopc_insert_task(&runtime);

    total = hopc_gc_maxmem(&runtime) - hopc_gc_freemem(&runtime);
    printf("top: 0x%08X end: 0x%08X, total: %d, free: %d\n", runtime.gc.top, runtime.gc.heapend_p, total, hopc_gc_freemem(&runtime));

    for(taskp = runtime.taskheadp; taskp; taskp = taskp->next) {
        printf("TASK: 0x%08X %08X\n", taskp, taskp->id);
    }

    hopc_delete_task(&runtime, 5);

    dump_heap3(&runtime, ROOTS, roots);
    hopc_gc_collect(&runtime);

    printf("-- after collecting\n");
    dump_heap3(&runtime, ROOTS, roots);
    printf("top: 0x%08X end: 0x%08X, total: %d, free: %d\n", runtime.gc.top, runtime.gc.heapend_p, total, hopc_gc_freemem(&runtime));

    for(taskp = runtime.taskheadp; taskp; taskp = taskp->next) {
        printf("TASK: 0x%08X %08X\n", taskp, taskp->id);
    }

    hopc_delete_task(&runtime, 4);

    dump_heap3(&runtime, ROOTS, roots);
    hopc_gc_collect(&runtime);

    printf("-- after collecting\n");
    dump_heap3(&runtime, ROOTS, roots);
    printf("top: 0x%08X end: 0x%08X, total: %d, free: %d\n", runtime.gc.top, runtime.gc.heapend_p, total, hopc_gc_freemem(&runtime));

    for(taskp = runtime.taskheadp; taskp; taskp = taskp->next) {
        printf("TASK: 0x%08X %08X\n", taskp, taskp->id);
    }

/*    dump_heap3(&runtime, ROOTS, roots);*/

/*    printf("-- after collecting 2 Х-( \n");*/
/*    hopc_gc_collect(&runtime);*/
/*    dump_heap3(&runtime, ROOTS, roots);*/

/*    printf("top: 0x%08X\n", runtime.gc.top);*/

/*    do {*/
/*        tag = types[(rand()%(sizeof(types)/sizeof(types[0])))];*/
/*        chunk = hopc_gc_alloc_chunk(&runtime, tag);*/
/*        tmp1 = hopc_gc_maxmem(&runtime)/3;*/
/*        tmp = 3*hopc_gc_freemem(&runtime);*/
/*        if(  (hopc_gc_freemem(&runtime) > 1000 && m > 0 && !(rand()%3)) ||  tmp1 > tmp  && !(rand()%3) && r >= 0 ) {*/
/*            roots[r--] = chunk;*/
/*            m--;*/
/*        }*/
/*        if(!chunk) break;*/
/*        if(hopc_gc_freemem(&runtime) < 256) break;*/
/*    } while(chunk);*/

/*    total = hopc_gc_maxmem(&runtime) - hopc_gc_freemem(&runtime);*/
/*    printf("top: 0x%08X end: 0x%08X, total: %d, free: %d\n", runtime.gc.top, runtime.gc.heapend_p, total, hopc_gc_freemem(&runtime));*/

/*    for(chunk = runtime.gc.top-1; chunk >= runtime.gc.heapstart_p; chunk = hopc_gc_prev_chunk(&runtime, chunk)) {*/
/*        memc = (memchunk*)chunk;*/
/*        printf("traverse: 0x%08X X X %04X\n", chunk, memc->t.tag);*/
/*    }*/

/*    for(i=0;i<=ROOTS;i++) { // mockup*/
/*        printf("root: %08X\n", roots[i]);*/
/*        hopc_gc_mark_root_alive(&runtime, roots[i]);*/
/*    }*/

/*    chunk = hopc_gc_alloc_chunk(&runtime, TUPLE1);*/
/*    hopc_gc_mark_root_alive(&runtime, chunk);*/
/*    chunk = hopc_gc_alloc_chunk(&runtime, TUPLE2);*/
/*    hopc_gc_mark_root_alive(&runtime, chunk);*/

/*    dump_heap3(&runtime, ROOTS, roots);*/
/*    hopc_gc_collect(&runtime);*/
/*    printf("-- after collect\n");*/
/*    printf("top: 0x%08X end: 0x%08X, total: %d, free: %d\n", runtime.gc.top, runtime.gc.heapend_p, total, hopc_gc_freemem(&runtime));*/
/*    dump_heap3(&runtime, ROOTS, roots);*/


/*    dump_heap3(&runtime, ROOTS, roots); */
/*    dump_heap2(&runtime, ROOTS, roots);*/
/*    dump_heap(&runtime, ROOTS, roots);*/
    printf("--\n");

    return 0;
}

void dump_heap3(hopc_runtime *runtime, hword_t num, hword_t **roots) {
    hword_t *chunk = 0;
    memchunk *memc;
    for(chunk = runtime->gc.top-1; chunk >= runtime->gc.heapstart_p; chunk = hopc_gc_prev_chunk(runtime, chunk)) {
        memc = (memchunk*)chunk;
        printf("; 0x%08X %s %d %d %04X\n", chunk, 
                                           (memc->t.gc_alive?"alive":"dead"),
                                           hopc_tagsize(runtime, memc->t.tag),
                                           hopc_gc_chunksize(runtime, chunk), 
                                           memc->t.tag);
    }
}


/*void dump_heap(hopc_runtime *runtime, hword_t num, hword_t **roots) {*/
/*    int i,r,found=0;*/
/*    char c = 'U';*/
/*    const int cols = 128;*/
/*    for(i=0;i<sizeof(heap)/sizeof(heap[0]);i++) {*/
/*        if( !(i%cols) ) putchar('\n');*/
/*        found = 0;*/
/*        for(r=0;r<num;r++) { */
/*            if( roots[r] == &heap[i] ) {*/
/*                found = roots[r] == &heap[i];*/
/*                break;*/
/*            }*/
/*        }*/
/*        c = found ? '@' : '?';*/
/*        c = runtime->gc.top == &heap[i] ? '^' : c;*/
/*        putchar(c);*/
/*    }*/
/*    printf("\n%d\n", i);*/
/*    putchar('\n');*/
/*}*/

/*void dump_heap2(hopc_runtime *runtime, hword_t num, hword_t **roots) {*/
/*    hword_t *b = runtime->gc.heapstart_p;*/
/*    hword_t *p = runtime->gc.heapstart_p;*/
/*    hword_t *end = runtime->gc.top;*/
/*    hword_t sz = 0;*/
/*    const int cols = 128;*/
/*    char h = 'x';*/
/*    int i = 0, j = 0;*/
/*    do {*/
/*        if( !(j%cols) ) putchar('\n');*/
/*        h = ((memchunk*)p)->t.gc_alive ? 'L' : 'x';*/
/*        sz = hopc_gc_chunksize(runtime, p);*/
/*        putchar(h);*/
/*        j++;*/
/*        for(i=0;i<sz-1;i++, j++) { if( !(j%cols) ) putchar('\n'); putchar(h); }*/
/*        p += sz; */
/*    } while( p < end );*/
/*    if( p < runtime->gc.heapend_p && p == end ) {*/
/*        if( !(j%cols) ) putchar('\n'); */
/*        putchar('^');*/
/*        p++;*/
/*        j++;*/
/*    }*/
/*    while( p < runtime->gc.heapend_p ) {*/
/*        if( !(j%cols) ) putchar('\n');*/
/*        putchar('.');*/
/*        j++;*/
/*        p++; */
/*    }*/
/*    putchar('\n');*/
/*    printf("%d\n", j);*/
/*}*/

