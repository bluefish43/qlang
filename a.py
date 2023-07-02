from multiprocessing import Pool
import math
import time

def calculate_factorial(num):
    return math.factorial(num)

if __name__ == '__main__':
    started_at = time.time()
    with Pool() as pool:
        numbers = range(1000)
        results = pool.map(calculate_factorial, numbers)

    for num, result in zip(numbers, results):
        print(f"The factorial of {num} is: {result}")
    ended_at = time.time()
    print(f"{ended_at - started_at}")