"""
Header comment
"""

import random
import copy

class Resources:
    """Describes the weight and volume of an object."""
    def __init__(self, weight, volume):
        """Initialise Resources using the weight and volume."""
        self.weight = weight
        self.volume = volume

    def __repr__(self):
        """Return a string displaying the weight and volume"""
        return f"Weight: {self.weight}; Volume: {self.volume}"

    def __add__(self, other):
        """Add and return two Resources instances"""
        return Resources(self.weight + other.weight,
                         self.volume + other.volume)

    def __sub__(self, other):
        """Subtract and return two Resources instances"""
        return Resources(self.weight - other.weight,
                         self.volume - other.volume)

    def fits_in(self, other):
        """Return true if self fits in other."""
        return self.weight <= other.weight and self.volume <= other.volume


class Item:
    """Item that will go into the knapsack"""
    def __init__(self, name, points, resources):
        """Initialise Item using the name of the item, how many points it is
        worth and the resources."""
        self.name = name
        self.points = points
        self.resources = resources

    def __repr__(self):
        """Return a string with the name, points, and resources of the item."""
        return f"Name: {self.name}; Points: {self.points}; {self.resources}"

    def get_name(self):
        """Return the name of the item."""
        return self.name

    def get_points(self):
        """Return the points of the item."""
        return self.points

    def get_resources(self):
        """Return the resources of the item."""
        return self.resources


class Items:
    """A list of items"""
    def __init__(self):
        """Initalise by making a list."""
        self.items = []

    def __repr__(self):
        """Return a string with the list of items."""
        return f"{self.items}"

    def __len__(self):
        """Return the length of the list of items."""
        return len(self.items)

    def __getitem__(self, index):
        """Return the item on the given index."""
        return self.items[index]

    def add_item(self, item):
        """Add an item to the list."""
        self.items.append(item)

    def remove_item(self, index):
        """Remove and return the item on the given index from the list."""
        if self.items:
            return self.items.pop(index)
        return False


class Knapsack:
    """Knapsack with items"""
    def __init__(self, capacity):
        """Initialise an empty knapsack with 0 points and the capacity of the
        resources."""
        self._points = 0
        self._items = Items()
        self._capacity = capacity
        self._resources = Resources(0, 0)

    def __repr__(self):
        """Return a string with the points, resources, and list of items."""
        return f"""Points: {self._points}; Resources: {
               self._resources}; Capacity: {self._capacity}; Items: {
               self._items}"""

    def __len__(self):
        """Return the length of the list of items in the knapsack."""
        return len(self._items)

    def __getitem__(self, index):
        """Return the item in the knapsack on the given index."""
        return self._items[index]

    def item_fits(self, item):
        """Return true if the item fits in the knapsack, otherwise return
        false."""
        item_resources = item.get_resources()
        if item_resources.fits_in(self._capacity - self._resources):
            return True
        return False

    def add_item(self, item):
        """Add item to knapsack and return true if the item fits in the
        knapsack, otherwise return false."""
        if not self.item_fits(item):
            return False
        self._items.add_item(item)
        self._points += item.get_points()
        self._resources += item.get_resources()
        return True

    def remove_last_item(self):
        """Remove and return the last item from the knapsack."""
        i = len(self) - 1
        self._points -= self._items[i].get_points()
        self._resources -= self._items[i].get_resources()
        return self._items.remove_item(i)

    def remove_random_item(self):
        """Remove and return a random item from the knapsack."""
        i = random.randint(0, len(self) - 1)
        self._points -= self._items[i].get_points()
        self._resources -= self._items[i].get_resources()
        return self._items.remove_item(i)

    def get_points(self):
        """Return the total number of points in the knapsack."""
        return self._points

    def get_capacity(self):
        """Return the capacity of the knapsack."""
        return self._capacity

    def reset(self):
        """Remove every item in the knapsack and reset the points."""
        self._points = 0
        self._items = Items()
        self._resources = Resources(0, 0)

    def save(self, filename):
        """Save the current state of the knapsack to filename."""
        with open(filename, "w") as f:
            f.write(f"points:{self.get_points()}\n")
            for item in self._items:
                f.write(f"{item.get_name()}\n")


class Solver:
    """Solver used to solve knapsack problem."""
    def solve(self, knapsack, items):
        """Solve the knapsack problem with the knapsack and the list of
        items by storing the knapsack with the most points."""
        pass

    def get_best_knapsack(self):
        """Return knapsack with the most points."""
        return self.best_knapsack

    def update_best_knapsack(self, knapsack):
        """Update best knapsack to new knapsack."""
        self.best_knapsack = Knapsack(knapsack.get_capacity())
        for item in knapsack:
            self.best_knapsack.add_item(item)


class Solver_Random(Solver):
    """Random solver that solves the knapsack problem by inserting random
    items in the knapsack."""
    def __init__(self, runs):
        """Initialise random solver. The number of runs determine how
        many times the solver will try to solve the knapsack problem."""
        self.runs = runs
        self.best_knapsack = Knapsack(Resources(0, 0))

    def solve(self, knapsack, items):
        """Solve the knapsack problem by repeatedly inserting random items into
        the knapsack and saving the best knapsack."""
        for run in range(self.runs):
            knapsack.reset()
            available_items = list(range(len(items)))
            random.shuffle(available_items)
            while available_items:
                i = available_items.pop()
                if not knapsack.add_item(items[i]):
                    break

            if self.best_knapsack.get_points() < knapsack.get_points():
                self.update_best_knapsack(knapsack)


def load_knapsack(knapsack_file):
    """Return a knapsack and a list of items from the knapsack_file"""
    with open(knapsack_file, "r") as f:
        lines = f.read().splitlines()
    knapsack_info = lines[1].split(", ")
    knapsack = Knapsack(Resources(int(knapsack_info[2]),
                                  int(knapsack_info[3])))
    items = Items()
    for i in range(2, len(lines)):
        item_info = lines[i].split(", ")
        items.add_item(Item(item_info[0],
                            int(item_info[1]),
                            Resources(int(item_info[2]), int(item_info[3]))))
    return knapsack, items


def solve(solver, knapsack_file, solution_file):
    """ Uses 'solver' to solve the knapsack problem in file
    'knapsack_file' and writes the best solution to 'solution_file'.
    """
    knapsack, items = load_knapsack(knapsack_file)
    solver.solve(knapsack, items)
    knapsack = solver.get_best_knapsack()
    print(f"""saving solution with {
          knapsack.get_points()} points to '{solution_file}'""")
    knapsack.save(solution_file)


def test():
    solver_random = Solver_Random(1000)
    knapsack_file = "knapsack_small"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv",
          knapsack_file + "_solution_random.csv")

    knapsack_file = "knapsack_medium"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv",
          knapsack_file + "_solution_random.csv")

    knapsack_file = "knapsack_large"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv",
          knapsack_file + "_solution_random.csv")


def main():
    solver_random = Solver_Random(1000)
    solver_optimal_recursive = Solver_Optimal_Recursive()
    solver_optimal_iterative_deepcopy = Solver_Optimal_Iterative_Deepcopy()
    solver_optimal_iterative = Solver_Optimal_Iterative()
    solver_random_improved = Solver_Random_Improved(5000)

    knapsack_file = "knapsack_small"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv",
          knapsack_file + "_solution_random.csv")
    solve(solver_optimal_recursive, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_recursive.csv")
    solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    solve(solver_optimal_iterative, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative.csv")
    solve(solver_random_improved, knapsack_file + ".csv",
          knapsack_file + "_solution_random_improved.csv")

    knapsack_file = "knapsack_medium"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv",
          knapsack_file + "_solution_random.csv")
    solve(solver_optimal_recursive, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_recursive.csv")
    solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    solve(solver_optimal_iterative, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative.csv")
    solve(solver_random_improved, knapsack_file + ".csv",
          knapsack_file + "_solution_random_improved.csv")

    knapsack_file = "knapsack_large"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv",
          knapsack_file + "_solution_random.csv")
    solve(solver_random_improved, knapsack_file + ".csv",
          knapsack_file + "_solution_random_improved.csv")

if __name__ == "__main__":  # keep this at the bottom of the file
    # main()
    test()
