// runtime.c

#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

// Their addresses are object headers, aligned so that they have a pointer tag
_Alignas(8) char procedure_type;
_Alignas(8) char frame_type;

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

void check_procedure(value x, uint64_t nargs) {
    if (!is_pointer(x)) {
        fprintf(stderr, "Calling error: "
                        "expected procedure, got non-pointer value\n");
        exit(1);
    }
    value *ptr = (value *)x;
    if (ptr == NULL) {
        fprintf(stderr, "Calling error: "
                        "expected procedure, got null pointer\n");
        exit(1);
    }
    if (ptr[0] != (value)(uintptr_t)&procedure_type) {
        fprintf(stderr, "Calling error: "
                        "expected procedure, got pointer of different type\n");
        exit(1);
    }
    uint64_t arity = ptr[1];
    if (arity != nargs) {
        fprintf(stderr,
                "Calling error: "
                "procedure of arity %" PRIu64 " called with %" PRIu64 " arguments\n",
                arity, nargs);
        exit(1);
    }
}

value eq(value x, value y) {
    return bool_to_value(x == y);
}

value int_add(value x, value y) {
    assert(is_integer(x) && is_integer(y));
    return x + y - 1;
}

value int_sub(value x, value y) {
    assert(is_integer(x) && is_integer(y));
    return x - y + 1;
}

value int_mul(value x, value y) {
    assert(is_integer(x) && is_integer(y));
    value product = (value)integer_value(x) * (value)integer_value(y);
    return int_to_value((int64_t)product);
}

value int_eq(value x, value y) {
    assert(is_integer(x) && is_integer(y));
    return bool_to_value(x == y);
}

value int_lt(value x, value y) {
    assert(is_integer(x) && is_integer(y));
    return bool_to_value((int64_t)x < (int64_t)y);
}

value int_leq(value x, value y) {
    assert(is_integer(x) && is_integer(y));
    return bool_to_value((int64_t)x <= (int64_t)y);
}

value int_gt(value x, value y) {
    assert(is_integer(x) && is_integer(y));
    return bool_to_value((int64_t)x > (int64_t)y);
}

value int_geq(value x, value y) {
    assert(is_integer(x) && is_integer(y));
    return bool_to_value((int64_t)x >= (int64_t)y);
}

// ----------------------------------------------------------------------------

value *scheme_mem;
size_t scheme_len, scheme_cap;

value *scheme_alloc(uint64_t nwords) {
    if (nwords > scheme_cap - scheme_len) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }
    value *ptr = &scheme_mem[scheme_len];
    scheme_len += nwords;
    return ptr;
}

value is_null(value x) {
    return bool_to_value(x == NIL);
}

value pair_p(value x) {
    return bool_to_value(is_pair(x));
}

value cons(value car, value cdr) {
    value *ptr = scheme_alloc(2);
    ptr[0] = car;
    ptr[1] = cdr;
    return (value)(uintptr_t)ptr | 3;
}

value car(value pair) {
    assert(is_pair(pair));
    value *ptr = (value *)(uintptr_t)(pair & ~(value)7);
    return ptr[0];
}

value cdr(value pair) {
    assert(is_pair(pair));
    value *ptr = (value *)(uintptr_t)(pair & ~(value)7);
    return ptr[1];
}

// ----------------------------------------------------------------------------

static void print_obj(value x);

static void print_pair(value x) {
    printf("(");
    print_obj(car(x));
    x = cdr(x);
    while (is_pair(x)) {
        printf(" ");
        print_obj(car(x));
        x = cdr(x);
    }
    if (x != NIL) {
        printf(" . ");
        print_obj(x);
    }
    printf(")");
}

static void print_obj(value x) {
    if (is_integer(x)) {
        printf("%" PRId64, integer_value(x));
    } else if (is_immediate(x)) {
        switch (x) {
        case BOOL_FALSE:
            printf("#f");
            break;
        case BOOL_TRUE:
            printf("#t");
            break;
        case NIL:
            printf("()");
            break;
        case UNDEF:
            printf("#<undef>");
            break;
        default:
            printf("#<err>");
        }
    } else if (is_pair(x)) {
        print_pair(x);
    } else if (is_pointer(x)) {
        value *ptr = (value *)x;
        if (ptr == NULL) {
            printf("#<null>");
        } else if (ptr[0] == (value)(uintptr_t)&procedure_type) {
            printf("#<procedure>");
        } else {
            printf("#<unknown ptr>");
        }
    } else {
        printf("#<err>");
    }
}

// ----------------------------------------------------------------------------

value scheme_main_stub(void);

int main(void) {
    scheme_len = 0;
    scheme_cap = 1 << 26;
    scheme_mem = malloc(scheme_cap * sizeof(value));
    if (scheme_mem == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }
    value v = scheme_main_stub();
    print_obj(v);
    free(scheme_mem);
    return 0;
}
