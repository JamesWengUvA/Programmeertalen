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
    main()
