"""
Header comment
"""

class Resources:
    """Resources of the items and knapsack."""
    def __init__(self, weight, volume):
        self.weight = weight
        self.volume = volume

    def __repr__(self):
        return f"Weight: {self.weight} Height: {self.volume}"

    def __add__(self, other):
        return Resources(self.weight + other.weight,
                         self.volume + other.volume)

    def __sub__(self, other):
        return Resources(self.weight - other.weight,
                         self.volume - other.volume)

    def fits_in(self, other):
        """Return true if self fits in other."""
        return self.weight <= other.weight and self.volume <= other.volume


class Item:
    """Item"""
    def __init__(self, name, points, resources):
        self.name = name
        self.points = points
        self.resources = resources

    def __repr__(self):
        return f"Name: {self.name} Points: {self.points} {self.resources}"

    def get_name(self):
        return self.name

    def get_points(self):
        return self.points

    def get_resources(self):
        return self.resources


class Items:
    """List of items"""
    def __init__(self):
        self.items = []

    def __repr__(self):
        return f"{self.items}"

    def __len__(self):
        return len(self.items)

    def __getitem__(self, index):
        return self.items[index]

    def add_item(self, item):
        self.items.append(item)

    def remove_item(self, index):
        self.items.pop(index)


class Knapsack:
    """Knapsack with items"""
    def __init__(self, resources):
        self._points = 0
        self._resources = resources
        self._items = Items()

    def __repr__(self):
        return f"Points: {self._points} {self._resources} Items: {self._items}"

    def __len__(self):
        return len(self._items)

    def item_fits(self, item):
        item_resources = item.get_resources()
        return True if item_resources.fits_in(self._resources) else False


def test():
    knapsack = Knapsack(Resources(50, 50))
    item0 = Item("Lamp", 15, Resources(20, 30))
    item1 = Item("Table", 35, Resources(50, 80))
    print(knapsack.item_fits(item0))
    print(knapsack.item_fits(item1))


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


def solve(solver, knapsack_file, solution_file):
    """ Uses 'solver' to solve the knapsack problem in file
    'knapsack_file' and writes the best solution to 'solution_file'.
    """
    knapsack, items = load_knapsack(knapsack_file)
    solver.solve(knapsack, items)
    knapsack = solver.get_best_knapsack()
    print(f"saving solution with {
        knapsack.get_points()} points to '{solution_file}'")
    knapsack.save(solution_file)


if __name__ == "__main__":  # keep this at the bottom of the file
    # main()
    test()
