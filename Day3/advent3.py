import numpy as np
import sys

def read_data(file):
  with open(file) as f:
    raw = map(lambda x: x.split(),f.readlines())
    input = list(map(lambda sides: list(map(lambda side: int(side),sides)),raw))
    return input

def check_tri(input, count = 0):
  for sides in input:
    count += int(sides[0] + sides[1] >  sides[2] 
                 and sides[1] + sides[2] > sides[0] 
                 and sides[2] + sides[0] > sides[1])
  return count

def check_tri2(input, count = 0):
  for sides in input:
    count += int(max(sides) < sum(sides) - max(sides))
  return count

def check_tri3(input):
  return sum(map(lambda sides: int(max(sides) < sum(sides)-max(sides)),input))

def check_tri4(input):
  #lol
  return sum([sum(map(lambda sides: int(max(sides) < 
  	   sum(filter(lambda side: side != max(sides),sides))),input)), 
         sum(map(lambda sides: int(max(sides) < 
  	   sum(filter(lambda side: side == max(sides),sides))),input))])

def transpose_list(input,dim):
  column_vec = np.array(input).T.reshape(1,np.array(input).size)[0]
  column_vec = list(zip(*[iter(column_vec)]*3))
  return column_vec

def algo_test(alg1,alg2,alg3,alg4,input):
  if (alg1(input) == alg2(input) == alg3(input) == alg4(input) and 
	  alg1(transpose_list(input,3)) == alg2(transpose_list(input,3)) ==
	  alg3(transpose_list(input,3)) == alg4(transpose_list(input,3))):
    print("all algorithms match. yay!")
  else:
    print("algos don't match you made a boo-boo")

def solution(file):
  input = read_data(file)
  q1 = check_tri3(input)
  q2 = check_tri3(transpose_list(input,3))
  print("Q1 = {}\nQ2 = {}".format(q1,q2))
  algo_test(check_tri, check_tri2, check_tri3, check_tri4, input)

if name == "__main__":
  solution(sys.args[1])
