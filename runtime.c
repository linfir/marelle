// runtime.c

#include <errno.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint64_t value;

_Static_assert(sizeof(void *) == 8, "the runtime assumes 64-bit pointers");

// Prints fmt, a printf format using up to two string arguments, and exits; in prelude.low
_Noreturn void scheme_fatal(const char *fmt, const char *a, const char *b);

// ----------------------------------------------------------------------------

// The GC state, in prelude.low
extern value *scheme_mem;
extern size_t scheme_len, scheme_cap, scheme_limit;
extern value *spare_mem;

// Statistics, printed at exit when SCHEME_GC_STATS is set
extern uint64_t gc_count, gc_copied, gc_max_live, gc_used_before;

// Stress mode (SCHEME_GC_STRESS): collect every gc_stress allocations
extern uint64_t gc_stress, gc_countdown;

// ----------------------------------------------------------------------------

value scheme_enter(void);

// Prints x to out, in prelude.low
void scheme_display(FILE *out, value x);

// The positive integer in the environment variable name, or default_value if unset
static uint64_t env_positive(const char *name, uint64_t default_value, uint64_t max) {
    const char *env = getenv(name);
    if (env == NULL) {
        return default_value;
    }
    char *end;
    errno = 0;
    unsigned long long n = strtoull(env, &end, 10);
    if (errno != 0 || end == env || *end != '\0' || n == 0 || n > max) {
        scheme_fatal("Invalid %s: %s\n", name, env);
    }
    return n;
}

// Each stretch between collections allocates from the words copied up to the used words
static void print_gc_stats(void) {
    uint64_t allocated = gc_used_before + scheme_len - gc_copied;
    fflush(stdout);
    fprintf(stderr,
            "GC: %" PRIu64 " collections, %" PRIu64 " words allocated, %" PRIu64 " words copied, "
            "%" PRIu64 " max live words\n",
            gc_count, allocated, gc_copied, gc_max_live);
}

int main(void) {
    // 2^26 words by default: 1 GiB for both spaces
    scheme_cap = env_positive("SCHEME_HEAP", (uint64_t)1 << 26, SIZE_MAX / sizeof(value));
    gc_stress = env_positive("SCHEME_GC_STRESS", 0, UINT64_MAX);
    gc_countdown = gc_stress;
    scheme_limit = gc_stress != 0 ? 0 : scheme_cap;
    const char *stats = getenv("SCHEME_GC_STATS");
    if (stats != NULL && *stats != '\0') {
        atexit(print_gc_stats);
    }
    scheme_mem = calloc(scheme_cap, sizeof(value));
    spare_mem = calloc(scheme_cap, sizeof(value));
    if (scheme_mem == NULL || spare_mem == NULL) {
        scheme_fatal("Memory allocation failed\n", NULL, NULL);
    }
    value v = scheme_enter();
    scheme_display(stdout, v);
    free(scheme_mem);
    free(spare_mem);
    return 0;
}
