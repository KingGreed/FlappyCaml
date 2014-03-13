typedef struct Vector {int* tab;} Vector;
typedef struct Network {} Network;

Vector* vector_init(int, double[]);
Network* network_load(const char*);
void network_feed(Network*, Vector*, Vector**);
