"""
Name: James Weng
UvAnetID: 15685365
Study: BSc Informatica

This file has multiple Solver classes that solve the knapsack problem. The
knapsack and items have a points value, weight and volume.
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
        """Return a string displaying the weight and volume."""
        return f"Weight: {self.weight}; Volume: {self.volume}"

    def __add__(self, other):
        """Add and return two Resources instances."""
        return Resources(self.weight + other.weight,
                         self.volume + other.volume)

    def __sub__(self, other):
        """Subtract and return two Resources instances."""
        return Resources(self.weight - other.weight,
                         self.volume - other.volume)

    def __eq__(self, other):
        """Return true if the two resources have the same values."""
        if self.weight == other.weight and self.volume == other.volume:
            return True
        return False

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

    def __eq__(self, other):
        """Return true if the two items are the same item."""
        if (self.name == other.name and self.points == other.points and
                self.resources == other.resources):
            return True
        return False


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

    def remove_index(self, index):
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
        if item.resources.fits_in(self._capacity - self._resources):
            return True
        return False

    def add_item(self, item):
        """Add item to knapsack and return true if the item fits in the
        knapsack, otherwise return false."""
        if not self.item_fits(item):
            return False
        self._items.add_item(item)
        self._points += item.points
        self._resources += item.resources
        return True

    def remove_last_item(self):
        """Remove and return the last item from the knapsack."""
        i = len(self) - 1
        self._points -= self._items[i].points
        self._resources -= self._items[i].resources
        return self._items.remove_index(i)

    def remove_random_item(self):
        """Remove and return a random item from the knapsack."""
        i = random.randint(0, len(self) - 1)
        self._points -= self._items[i].points
        self._resources -= self._items[i].resources
        return self._items.remove_index(i)

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
                f.write(f"{item.name}\n")


class Solver:
    """Solver used to solve knapsack problem."""
    def __init__(self):
        """Initialise solver and create empty best knapsack with no
        resources."""
        self.best_knapsack = Knapsack(Resources(0, 0))

    def solve(self, knapsack, items):
        """Solve the knapsack problem with the knapsack and the list of
        items by storing the knapsack with the most points."""
        pass

    def check_best_knapsack(self, knapsack):
        """Update best knapsack to new knapsack if the new knapsack has more
        points."""
        if self.best_knapsack.get_points() < knapsack.get_points():
            self.best_knapsack = Knapsack(knapsack.get_capacity())
            for item in knapsack:
                self.best_knapsack.add_item(item)
            return True
        return False


class Solver_Random(Solver):
    """Random solver that solves the knapsack problem by inserting random
    items in the knapsack."""
    def __init__(self, runs):
        """Initialise random solver. The number of runs determine how
        many times the solver will try to solve the knapsack problem."""
        super().__init__()
        self.runs = runs

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
            self.check_best_knapsack(knapsack)


class Solver_Optimal_Recursive(Solver):
    """Solver that solves the knapsack problem by going through each
    combination of items recursively."""

    def __init__(self):
        """Initialise optimal recursive solver."""
        super().__init__()

    def solve(self, knapsack, items):
        """Solve knapsack problem by calling recursive function."""
        self._recursive_function(knapsack, items, 0)

    def _recursive_function(self, knapsack, items, index):
        """Call itself after trying to add the item on index. Then remove the
        item and call itself again. Save the new knapsack whenever it has more
        points than the previous best."""
        self.check_best_knapsack(knapsack)

        if index >= len(items):
            return

        if knapsack.add_item(items[index]):
            self._recursive_function(knapsack, items, index + 1)
            knapsack.remove_last_item()

        self._recursive_function(knapsack, items, index + 1)


class Solver_Optimal_Iterative_Deepcopy(Solver):
    """Solver that solves the knapsack problem by trying all combinations of
    items through a depth-first search. The DFS is done by deepcopying
    knapsacks."""
    def __init__(self):
        """Initialise solver with an empty stack."""
        super().__init__()
        self.stack = []

    def solve(self, knapsack, items):
        """Solve the knapsack problem by doing a depth-first search using a
        stack with knapsacks."""
        self.stack.append((0, copy.deepcopy(knapsack)))
        while self.stack:
            i, knapsack = self.stack.pop()
            if i >= len(items):
                continue
            self.stack.append((i + 1, knapsack))
            knapsack = copy.deepcopy(knapsack)
            if knapsack.add_item(items[i]):
                self.stack.append((i + 1, knapsack))
            self.check_best_knapsack(knapsack)


class Solver_Optimal_Iterative(Solver):
    """An alternative version of the Solver_Optimal_Iterative_Deepcopy class
    that doesn't use deepcopy. Instead of pushing knapsacks on a stack, the
    solver pushes lists of items which it then adds to the knapsack."""
    def __init__(self):
        """Initialise the solver with an empty stack and a list for lists."""
        super().__init__()
        self.stack = []
        self.list_of_lists = []

    def solve(self, knapsack, items):
        """Solve the knapsack problem by doing a depth-first search using a
        stack with lists of items."""
        self.stack.append((0, []))
        while self.stack:
            i, item_list = self.stack.pop()
            if i >= len(items):
                self.list_of_lists.append(copy.copy(item_list))
                continue
            self.stack.append((i + 1, copy.copy(item_list)))
            item_list.append(items[i])
            self.stack.append((i + 1, copy.copy(item_list)))

        for list in self.list_of_lists:
            knapsack.reset()
            for item in list:
                if not knapsack.add_item(item):
                    continue
            self.check_best_knapsack(knapsack)


class Solver_Random_Improved(Solver_Random):
    """An improved version of the Solver_Random class that uses a hill climbing
    algorithm after the knapsack has been filled with random items."""
    def __init__(self, runs):
        """Initialise solver with the number of runs. Hill climb iterations
        determine how many times the hill climb algorithm will be applied."""
        super().__init__(runs)
        # Hill climb iterations may be changed manually.
        self.hill_climb_iterations = 250

    def solve(self, knapsack, items):
        super().solve(knapsack, items)
        knapsack = copy.deepcopy(self.best_knapsack)
        available_items = [item for item in items if item
                           not in knapsack._items]

        for i in range(self.hill_climb_iterations):
            knapsack = copy.deepcopy(self.best_knapsack)
            removed_item = knapsack.remove_random_item()
            available_items.append(removed_item)
            random.shuffle(available_items)
            for item in available_items:
                if not knapsack.add_item(item):
                    break
            if self.check_best_knapsack(knapsack):
                available_items = [item for item in items if item
                                   not in knapsack._items]
            else:
                available_items.remove(removed_item)


def load_knapsack(knapsack_file):
    """Return a knapsack and a list of items from the knapsack_file."""
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
    knapsack = solver.best_knapsack
    print(f"""saving solution with {
          knapsack.get_points()} points to '{solution_file}'""")
    knapsack.save(solution_file)


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
    main()
