print(list(range(1, 5)))


def get_maior(n, lista):
    if len(lista) > 0:
        if n > lista[0]:
            n = get_maior(n, lista[1:])
        else:
            n = get_maior(lista[0], lista[1:])
    return n


lista = [9, 33, 65, 31]

print(get_maior(lista[0], lista[1:]))
a = (0, 9)
b = (0, 9)
print(a == b)
