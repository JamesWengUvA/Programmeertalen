#include <stdio.h>
#include <math.h>

typedef struct {
    int x;
    int y;
} Coordinate;

Coordinate init_coordinate(int x, int y) {
    Coordinate self;
    self.x = x;
    self.y = y;
    return self;
}

char* repr(Coordinate self) {
    static char buffer[30]; // reusing static buffer
    sprintf(buffer, "(%d, %d)", self.x, self.y);
    return buffer;
}

Coordinate add(Coordinate self, Coordinate other) {
    return init_coordinate(self.x + other.x, self.y + other.y);
}

double get_distance(Coordinate self) {
    return sqrt(self.x*self.x + self.y*self.y);
}

int main() {
    Coordinate c1 = init_coordinate( 3, 1);
    Coordinate c2 = init_coordinate(-4, 2);
    Coordinate c3 = add(c1, c2);
    printf("c1: %s\n", repr(c1));
    printf("c2: %s\n", repr(c2));
    printf("c3: %s\n", repr(c3));
    printf("c3 distance: %f\n", get_distance(c3));
}
