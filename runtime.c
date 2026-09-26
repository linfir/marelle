// runtime.c

#include <assert.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

char procedure_type;

// tagged value, tag is 3 lower bits
// 0 pointer
// 1 integer
// 2 immediate
typedef uint64_t value;

enum {
    BOOL_FALSE = 2,
    BOOL_TRUE = 10,
};

static bool is_pointer(value x) {
    return (x & 7) == 0;
}

static bool is_integer(value x) {
    return (x & 7) == 1;
}

static bool is_immediate(value x) {
    return (x & 7) == 2;
}

static int64_t integer_value(value x) {
    return (int64_t)x >> 3;
}

static value int_to_value(int64_t x) {
    return ((value)x << 3) | 1;
}

static value value_to_bool(bool b) {
    return b ? BOOL_TRUE : BOOL_FALSE;
}

void *check_procedure(value x, int64_t nargs) {
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
    int64_t arity = (int64_t)ptr[1];
    if (arity != nargs) {
        fprintf(stderr,
                "Calling error: "
                "procedure of arity %" PRId64 " called with %" PRId64 " arguments\n",
                arity, nargs);
        exit(1);
    }
    return (void *)ptr[2];
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
        default:
            printf("#<err>");
        }
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

value scheme_main_stub(void);

int main(void) {
    value v = scheme_main_stub();
    print_obj(v);
    return 0;
}

value eq(value x, value y) {
    return value_to_bool(x == y);
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
    return value_to_bool(x == y);
}

value int_lt(value x, value y) {
    assert(is_integer(x) && is_integer(y));
    return value_to_bool((int64_t)x < (int64_t)y);
}

value int_leq(value x, value y) {
    assert(is_integer(x) && is_integer(y));
    return value_to_bool((int64_t)x <= (int64_t)y);
}

value int_gt(value x, value y) {
    assert(is_integer(x) && is_integer(y));
    return value_to_bool((int64_t)x > (int64_t)y);
}

value int_geq(value x, value y) {
    assert(is_integer(x) && is_integer(y));
    return value_to_bool((int64_t)x >= (int64_t)y);
}
