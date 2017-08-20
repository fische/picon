x = 12345
class MyClass:
    """A simple example class"""
    i = x

    def f(self):
        return self.i

t = MyClass()
y = t.f()
