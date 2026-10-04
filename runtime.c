// runtime.c

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Their addresses are object headers, aligned so that they have a pointer tag
_Alignas(8) char procedure_type;
_Alignas(8) char frame_type;
_Alignas(8) char forward_type;

// tagged value, tag is 3 lower bits
// 0 pointer
// 1 integer
// 2 immediate
// 3 pointer to cons cell
typedef uint64_t value;

_Static_assert(sizeof(void *) == 8, "the runtime assumes 64-bit pointers");

#define IMMEDIATE(x) ((value)(x) << 3 | 2)

enum {
    BOOL_FALSE = IMMEDIATE(0),
    BOOL_TRUE = IMMEDIATE(1),
    NIL = IMMEDIATE(2),
    UNDEF = IMMEDIATE(3),
};

#undef IMMEDIATE

static bool is_pointer(value x) {
    return (x & 7) == 0;
}

static bool is_integer(value x) {
    return (x & 7) == 1;
}

static bool is_immediate(value x) {
    return (x & 7) == 2;
}

static bool is_pair(value x) {
    return (x & 7) == 3;
}

static int64_t integer_value(value x) {
    return (int64_t)x >> 3;
}

static value int_to_value(int64_t x) {
    return ((value)x << 3) | 1;
}

static value bool_to_value(bool b) {
    return b ? BOOL_TRUE : BOOL_FALSE;
}

static void print_obj(FILE *out, value x);

// Prints the message and the offending value, then exits
_Noreturn void scheme_error(const char *msg, value x) {
    fflush(stdout);
    fprintf(stderr, "Error: %s: ", msg);
    print_obj(stderr, x);
    fprintf(stderr, "\n");
    exit(1);
}

static void check_integers(const char *msg, value x, value y) {
    if (!is_integer(x)) {
        scheme_error(msg, x);
    }
    if (!is_integer(y)) {
        scheme_error(msg, y);
    }
}

void check_procedure(value x, uint64_t nargs) {
    value *ptr = (value *)x;
    if (!is_pointer(x) || ptr == NULL || ptr[0] != (value)(uintptr_t)&procedure_type) {
        scheme_error("expected procedure", x);
    }
    uint64_t arity = ptr[1];
    if (arity != nargs) {
        fflush(stdout);
        fprintf(stderr,
                "Error: procedure of arity %" PRIu64 " called with %" PRIu64 " arguments\n",
                arity, nargs);
        exit(1);
    }
}

value int_add(value x, value y) {
    check_integers("+: expected integer", x, y);
    return x + y - 1;
}

value int_sub(value x, value y) {
    check_integers("-: expected integer", x, y);
    return x - y + 1;
}

value int_mul(value x, value y) {
    check_integers("*: expected integer", x, y);
    value product = (value)integer_value(x) * (value)integer_value(y);
    return int_to_value((int64_t)product);
}

value int_eq(value x, value y) {
    check_integers("=: expected integer", x, y);
    return bool_to_value(x == y);
}

value int_lt(value x, value y) {
    check_integers("<: expected integer", x, y);
    return bool_to_value((int64_t)x < (int64_t)y);
}

value int_leq(value x, value y) {
    check_integers("<=: expected integer", x, y);
    return bool_to_value((int64_t)x <= (int64_t)y);
}

value int_gt(value x, value y) {
    check_integers(">: expected integer", x, y);
    return bool_to_value((int64_t)x > (int64_t)y);
}

value int_geq(value x, value y) {
    check_integers(">=: expected integer", x, y);
    return bool_to_value((int64_t)x >= (int64_t)y);
}

// ----------------------------------------------------------------------------

value pair_p(value x) {
    return bool_to_value(is_pair(x));
}

value car(value pair) {
    if (!is_pair(pair)) {
        scheme_error("car: expected pair", pair);
    }
    value *ptr = (value *)(uintptr_t)(pair & ~(value)7);
    return ptr[0];
}

value cdr(value pair) {
    if (!is_pair(pair)) {
        scheme_error("cdr: expected pair", pair);
    }
    value *ptr = (value *)(uintptr_t)(pair & ~(value)7);
    return ptr[1];
}

// ----------------------------------------------------------------------------

// Copying collector (Cheney) with two semispaces.
// scheme_mem is the current one, scheme_len its used words out of scheme_cap.
// The inline path of alloc_frame bumps scheme_len up to scheme_limit, 0 in stress mode.
// Each space is zero past its used part, so frame slots not yet written hold 0.
value *scheme_mem;
size_t scheme_len, scheme_cap, scheme_limit;
static value *spare_mem;

// Statistics, printed at exit when SCHEME_GC_STATS is set
static uint64_t gc_count, gc_copied, gc_max_live, gc_used_before;

// Stress mode (SCHEME_GC_STRESS): collect every gc_stress allocations
static uint64_t gc_stress, gc_countdown;

static size_t object_words(value *p) {
    if (p[0] == (value)(uintptr_t)&frame_type) {
        return p[1];
    }
    if (p[0] == (value)(uintptr_t)&procedure_type) {
        return 4 + p[3];
    }
    return 2; // pair
}

// Copies the object x points to, if in the current space, and returns its new address;
// a moved object holds forward_type and its new address
static value forward(value x, value **free) {
    value tag = x & 7;
    if (tag != 0 && tag != 3) {
        return x;
    }
    uintptr_t addr = (uintptr_t)(x - tag);
    if (addr < (uintptr_t)scheme_mem || addr >= (uintptr_t)(scheme_mem + scheme_len)) {
        return x; // null or static procedure
    }
    value *p = (value *)addr;
    if (p[0] == (value)(uintptr_t)&forward_type) {
        return p[1] | tag;
    }
    size_t n = tag == 3 ? 2 : object_words(p);
    value *q = *free;
    memcpy(q, p, n * sizeof(value));
    *free = q + n;
    p[0] = (value)(uintptr_t)&forward_type;
    p[1] = (value)(uintptr_t)q;
    return (value)(uintptr_t)q | tag;
}

// Forwards the values of an object in the new space, returns its length
static size_t scan(value *p, value **free) {
    size_t n = object_words(p);
    size_t i = 0;
    if (p[0] == (value)(uintptr_t)&frame_type) {
        i = 3;
    } else if (p[0] == (value)(uintptr_t)&procedure_type) {
        i = 4;
    }
    for (; i < n; i++) {
        p[i] = forward(p[i], free);
    }
    return n;
}

// Moves the live objects and updates the roots, which must hold every heap pointer in use;
// the rest is reached through them
static void scheme_collect(value *roots, uint64_t nroots) {
    value *to = spare_mem;
    value *free = to;
    for (uint64_t i = 0; i < nroots; i++) {
        roots[i] = forward(roots[i], &free);
    }
    for (value *p = to; p < free;) {
        p += scan(p, &free);
    }
    size_t copied = (size_t)(free - to);
    gc_count++;
    gc_copied += copied;
    gc_used_before += scheme_len;
    if (copied > gc_max_live) {
        gc_max_live = copied;
    }
    memset(scheme_mem, 0, scheme_len * sizeof(value));
    spare_mem = scheme_mem;
    scheme_mem = to;
    scheme_len = copied;
    assert(scheme_len <= scheme_cap);
}

// Called from runtime.s, the only place where a collection may happen
value *scheme_alloc(uint64_t nwords, value *roots, uint64_t nroots) {
    bool stress = gc_stress != 0 && --gc_countdown == 0;
    if (stress) {
        gc_countdown = gc_stress;
    }
    if (stress || nwords > scheme_cap - scheme_len) {
        scheme_collect(roots, nroots);
        if (nwords > scheme_cap - scheme_len) {
            fprintf(stderr, "Out of memory\n");
            exit(1);
        }
    }
    value *ptr = &scheme_mem[scheme_len];
    scheme_len += nwords;
    return ptr;
}

// ----------------------------------------------------------------------------

static void print_pair(FILE *out, value x) {
    fprintf(out, "(");
    print_obj(out, car(x));
    x = cdr(x);
    while (is_pair(x)) {
        fprintf(out, " ");
        print_obj(out, car(x));
        x = cdr(x);
    }
    if (x != NIL) {
        fprintf(out, " . ");
        print_obj(out, x);
    }
    fprintf(out, ")");
}

static void print_obj(FILE *out, value x) {
    if (is_integer(x)) {
        fprintf(out, "%" PRId64, integer_value(x));
    } else if (is_immediate(x)) {
        switch (x) {
        case BOOL_FALSE:
            fprintf(out, "#f");
            break;
        case BOOL_TRUE:
            fprintf(out, "#t");
            break;
        case NIL:
            fprintf(out, "()");
            break;
        case UNDEF:
            fprintf(out, "#<undef>");
            break;
        default:
            fprintf(out, "#<err>");
        }
    } else if (is_pair(x)) {
        print_pair(out, x);
    } else if (is_pointer(x)) {
        value *ptr = (value *)x;
        if (ptr == NULL) {
            fprintf(out, "#<null>");
        } else if (ptr[0] == (value)(uintptr_t)&procedure_type) {
            fprintf(out, "#<procedure>");
        } else {
            fprintf(out, "#<unknown ptr>");
        }
    } else {
        fprintf(out, "#<err>");
    }
}

// ----------------------------------------------------------------------------

value scheme_enter(void);

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
        fprintf(stderr, "Invalid %s: %s\n", name, env);
        exit(1);
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
    scheme_len = 0;
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
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }
    value v = scheme_enter();
    print_obj(stdout, v);
    free(scheme_mem);
    free(spare_mem);
    return 0;
}
