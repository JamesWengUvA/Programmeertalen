def process_numbers(numbers, number_handler):
    print("process_numbers:")
    for i in numbers:
        number_handler(i)


def double_the_number(i):
    print(i * 2)


def repeat_three_times(i):
    print(str(i) * 3)


def main():
    numbers = [1, 2, 3, 4, 5]
    process_numbers(numbers, double_the_number)
    process_numbers(numbers, repeat_three_times)
    process_numbers(numbers, lambda i: print(i * i))


if __name__ == "__main__":
    main()
