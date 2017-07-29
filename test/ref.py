def diffLocalAndOuterRef():
    n = 0
    def inner():
        v = n
        n = "42"

def getRightOuterRef():
    n = 0
    def inner0():
        v = 42
        v = n
        def inner1():
            n = "42"
            def inner2():
                nonlocal v
                v = n

def diffSameOuterRefToTwoDiffVar():
    n = 0
    def inner0():
        v = 42
        def inner1():
            nonlocal v
            v = n
        def inner2():
            n = 0.42
            def inner3():
                nonlocal v
                v = n
