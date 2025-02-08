import math


class Coordinate:

    def __init__(self, x, y):
        """ Initalises a Coordinate object. For example:
        >>> c1 = Coordinate(1, 2)
        """
        self.x = x
        self.y = y

    def __repr__(self):
        """ Returns the string representation of a Coordinate.
        For example:
        >>> print(Coordinate(1, 2))
        (1, 2)
        """
        return f"({self.x}, {self.y})"

    def __add__(self, other):
        """ Returns a new Coordinate by adding 'self' and
        'other'. This method enables the '+' operator:
        >>> print(Coordinate(1, 2) + Coordinate(3, 3))
        (4, 5)
        """
        return Coordinate(self.x + other.x, self.y + other.y)

    def get_distance(self):
        """ Returns the distance to (0, 0). For example:
        >>> Coordinate(3, 4).get_distance()
        5.0
        """
        return math.sqrt(self.x**2 + self.y**2)


def main():
    c1 = Coordinate(3, 1)
    c2 = Coordinate(-4, 2)
    c3 = c1 + c2
    print("c1:", c1)
    print("c2:", c2)
    print("c3:", c3)
    print("c3 distance:", c3.get_distance())


if __name__ == "__main__":
    main()
