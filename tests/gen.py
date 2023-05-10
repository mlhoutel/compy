import math

# Définir une classe pour un cercle
class Circle:
    def __init__(self, radius):
        self.radius = radius

    def area(self):
        return math.pi * self.radius ** 2

    def circumference(self):
        return 2 * math.pi * self.radius

# Définir une fonction pour imprimer le résultat de la surface et de la circonférence d'un cercle
def print_circle_info(circle):
    print("Circle radius:", circle.radius)
    print("Circle area:", circle.area())
    print("Circle circumference:", circle.circumference())

# Définir une fonction pour trouver la plus grande valeur dans une liste
def find_max_value(lst):
    max_value = lst[0]
    for item in lst:
        if item > max_value:
            max_value = item
    return max_value

# Définir une fonction pour calculer le factoriel d'un nombre entier
def factorial(n):
    if n == 0:
        return 1
    else:
        return n * factorial(n-1)

# Définir une fonction pour vérifier si un nombre est premier
def is_prime(n):
    if n < 2:
        return False
    for i in range(2, int(math.sqrt(n))+1):
        if n % i == 0:
            return False
    return True

# Définir une fonction pour trouver tous les nombres premiers dans une liste
def find_primes(lst):
    primes = []
    for item in lst:
        if is_prime(item):
            primes.append(item)
    return primes

# Définir une fonction pour calculer la moyenne d'une liste de nombres
def calculate_average(lst):
    if len(lst) == 0:
        return None
    else:
        return sum(lst) / len(lst)

# Définir une liste de nombres
numbers = [3, 7, 2, 8, 1, 9, 4, 6, 5]

# Créer un cercle avec un rayon de 5
my_circle = Circle(5)

# Imprimer l'information sur le cercle
print_circle_info(my_circle)

# Trouver la plus grande valeur dans la liste de nombres
max_number = find_max_value(numbers)
print("Max value in numbers list:", max_number)

# Calculer le factoriel de 5
factorial_5 = factorial(5)
print("Factorial of 5:", factorial_5)

# Trouver tous les nombres premiers dans la liste de nombres
prime_numbers = find_primes(numbers)
print("Prime numbers in numbers list:", prime_numbers)

# Calculer la moyenne des nombres dans la liste de nombres
average_number = calculate_average(numbers)
print("Average value in numbers list:", average_number)