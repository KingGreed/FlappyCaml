#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

#include "network.h"

struct state {
	int x, y;
	int dx, dy;
	int alive;
	int jump;
};

Vector* to_input(struct state* s) {
	return vector_init(6, (double[]) {s->x, s->y, s->dx, s->dy, s->alive, s->jump});
}

Network* network = NULL;

value network_init(value unit) {
	CAMLparam1(unit);

	network = network_load("network.bin");

	CAMLreturn(Val_unit);
}

value send_state(value s) {
	CAMLparam1(s);

	struct state* st = malloc(sizeof(struct state));
	st->x = Field(s, 0);
	st->y = Field(s, 1);
	st->dx = Field(s, 2);
	st->dy = Field(s, 3);
	st->alive = Field(s, 4);
	st->jump = Field(s, 5);

	Vector* out = NULL;
	network_feed(network, to_input(st), &out);
	Field(s, 5) = Val_int((int) out->tab[0] > 0.5);

	free(st);
	CAMLreturn(Val_unit);
}






































Vector* vector_init(int i, double a[]) { i = a[0]; return NULL; }
Network* network_load(const char* s) { ++s; return NULL; }
void network_feed(Network* n, Vector* v, Vector** o) {++n, ++v, ++o;}
