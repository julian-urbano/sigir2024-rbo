exec(open('rbo/Python/rbo.py').read())

def assert_eq(obj, expected, digits = 6):
  assert round(obj, digits) == round(expected, digits)

def test_row(row):
  x = from_string(row['x'])
  y = from_string(row['y'])
  p = row['p']
  
  r = rbo(x, y, p, ties = 'w')
  assert_eq(r['ext'], row['w.ext'])
  assert_eq(r['min'], row['w.min'])
  assert_eq(r['max'], row['w.max'])
  assert_eq(r['res'], row['w.res'])
  
  r = rbo(x, y, p, ties = 'a')
  assert_eq(r['ext'], row['a.ext'])
  assert_eq(r['min'], row['a.min'])
  assert_eq(r['max'], row['a.max'])
  assert_eq(r['res'], row['a.res'])
  
  r = rbo(x, y, p, ties = 'b')
  assert_eq(r['ext'], row['b.ext'])
  assert_eq(r['min'], row['b.min'])
  assert_eq(r['max'], row['b.max'])
  assert_eq(r['res'], row['b.res'])
  
import pandas as pd
test = pd.read_csv('rbo/test.csv')

test.apply(lambda row: test_row(row), axis=1)
