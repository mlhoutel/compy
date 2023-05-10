

print("\n====================================================================================")
print("                             TEST 1 - DECLARATIONS                                  ")
print("====================================================================================")

print("function: ")
def test1():
    a = 1
    b = a + 2
    c = a + b
    print(c)
test1()

print("inlined: ")
[a := 1, b := a + 2, c := a + b, print(c)]

print("\n====================================================================================")
print("                             TEST 2 - FUNCTIONS                                    ")
print("====================================================================================")

print("function: ")
def test2():
    a = 1
    def sprint(v):
        print(v)
    sprint(a)
test2()

print("inlined: ")
[a := 1, sprint := (lambda v: print(v)), sprint(a)]

print("\n====================================================================================")
print("                             TEST 3 - LOOPS                                         ")
print("====================================================================================")

print("function: ")
def test3():
    for i in range(10):
        i *= 2
        print(i)
test3()

print("inlined: ")
[[(lambda i: [i := i * 2, print(i)])(i) for i in range(10)]]

print("\n====================================================================================")
print("                          TEST 4 - ESCAPING SCOPE FOR                                 ")
print("====================================================================================")

print("function: ")
def test4():
    a = 1
    b = 2
    for i in range(10):
        a = a + 1
        b = "test"
    print(a, b)
test4()

print("inlined: ")
[a := 1, b := 2, [(t := (lambda: [_a := a + 1, _b := "test", (_a, _b)][-1])(), a := t[0], b := t[1]) for i in range(10)], print(a, b)]


print("\n====================================================================================")
print("                             TEST 5 - WHILE LOOPS                                   ")
print("====================================================================================")

print("function: ")
def test5():
    a = 10
    while a > 2:
        a = a / 2
        print(a)
test5()

print("inlined: ")
[a := 10, (lambda w, a: w(w, a))(lambda w, a: [_wcond := a > 2, [_a := a / 2, print(_a), w(w, _a)] if _wcond else None], a) ]

print("\n====================================================================================")
print("                                TEST 6 - CLASSES                                    ")
print("====================================================================================")


print("function: ")
def test6():
    class A():
        a = 1
    i = A()
    print(i.a)
test6()

print("inlined: ")
[A := type('A', (object,), {'a': 1}), i := A(), print(i.a)]


print("\n====================================================================================")
print("                                TEST 7 - DECORATOR                                  ")
print("====================================================================================")

print("function: ")

def test7():
    def decorator(f):
        print("decorated")
        return f()
    
    @decorator
    def foo():
        return "bar"

    print(foo)
test7()

print("inlined: ")
[decorator := (lambda f: [print("decorated"), f()][-1]), foo := decorator(lambda: "bar"), print(foo)]

print("\n====================================================================================")
print("                          TEST 8 - CLASS DECORATOR                                  ")
print("====================================================================================")

print("function: ")
def test8():
    class A():
        m = "bar"
        @property
        def foo(self):
            return self.m
    i = A()
    print(i.foo)
test8()

print("inlined: ")
[A := type('A', (object,), {'m': "bar", 'foo': property(lambda self: self.m)}), i := A(), print(i.foo)]


print("\n====================================================================================")
print("                     TEST 9 - ESCAPING SCOPE WHILE                                  ")
print("====================================================================================")

print("function: ")
def test9():
    a = 1
    while a < 5:
        a += 1
        print(a)
    print(a)
test9()

print("inlined: ")
[a := 1, _temp := (lambda _core, a: _core(_core, a))((lambda _core, a: [_a := a, _cond := _a < 5, [_a := _a + 1, print(_a), _core(_core, _a)][-1] if _cond else (_a,)][-1]), a), a := _temp[0], print(a)]

print("\n====================================================================================")
print("                             TEST 10 - RETURN                                       ")
print("====================================================================================")

print("function: ")
def test10():
    def foo(x):
        x += 1
        if x < 0:
            return -1
        else:
            return 1
    
    print(foo(10))
    
test10()

print("inlined: ")
[foo := (lambda x: [x := x + 1, -1 if x < 0 else 1][-1]), print(foo(10))]


print("\n====================================================================================")
print("                     TEST 11 - RETURN ESCAPING SCOPE                                ")
print("====================================================================================")

print("function: ")
def test11():
    def foo(x):
        for i in range(10):
            x = x - 2
            if i + x < 0:
                return -1
            elif i - x > 0:
                return "test"
            print(x)
            
    print(foo(10))
    
test11()

print("inlined: ")
[foo := (lambda x: next((lambda y: -1 if i + (y := y - 2) < 0 else "test" if i - y > 0 else [print(y), None][-1])(x) for i in range(10)), None), print(foo(10))]
[foo := (lambda x: [_x := x, [_x := _x - 2, ]])]

print("\n====================================================================================")
print("                     TEST 12 - RETURN ESCAPING SCOPE                                ")
print("====================================================================================")

print("function: ")
def test13():
    def bar(lst):
        result = []
        for x in lst:
            if x < 0:
                continue
            result.append(x)
        return result
    print(bar([-1, 1, 2]))
test13()

print("inlined: ")
[bar := (lambda lst: [x for x in lst if x >= 0]), print(bar([-1, 1, 2]))]


print("\n====================================================================================")
print("                     TEST 13 - RETURN ESCAPING SCOPE                                ")
print("====================================================================================")

print("function: ")
def test14():
    def baz(n):
        for i in range(n):
            i = i - 1
            yield i**2
    
    for i in baz(10):
        print(i)
test14()

print("inlined: ")
[baz := (lambda n: [([_i := i, _i := _i - 1, _i**2][-1] for i in range(n))][-1]), [[print(i)] for i in baz(10)]]