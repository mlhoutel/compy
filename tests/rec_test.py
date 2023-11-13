"""
print("start")

for i in range(10):
    while True:
        return
    print(i)

print("ended")
"""

[
    __INL__STATE := (None, 1),
    print('start'),
    [
        [__INL__i := i, 
            [__INL__STATE := (None, 3),
             __INL__TEMP := (lambda __INL__CORE, __INL__STATE: __INL__CORE(__INL__CORE, __INL__STATE))((lambda __INL__CORE, __INL__STATE: 
                 [__INL__COND := True and __INL__STATE[1] > 3, 
                    [__INL__STATE := (None, 0), 
                    __INL__CORE(__INL__CORE, __INL__STATE)
                    ][-1] if __INL__COND 
                  else (__INL__STATE, None)][-1]),
                __INL__STATE),
             __INL__STATE := __INL__TEMP[0]],
            print(__INL__STATE),
            [print(__INL__i)] if __INL__STATE[1] > 1 else None] for i in range(10)],
    [print('ended')] if __INL__STATE[1] > 0 else None]