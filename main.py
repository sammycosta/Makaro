# Teste de uso de github online!

file = open('puzzle.txt', "r")

N = int(file.readline())

matriz_regioes = []
matriz_certezas = []
for i in range(N):
    matriz_regioes.append(file.readline().split())
file.readline() # \n
for i in range(N):
    matriz_certezas.append([int(x) for x in file.readline().split()])

