####################################################################################################
# Copyright 2024 Julián Urbano <urbano.julian@gmail.com>                               MIT LICENSE #
#                                                                                                  #
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software    #
# and associated documentation files (the "Software"), to deal in the Software without             #
# restriction, including without limitation the rights to use, copy, modify, merge, publish,       #
# distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the    #
# Software is furnished to do so, subject to the following conditions:                             #
#                                                                                                  #
# The above copyright notice and this permission notice shall be included in all copies or         #
# substantial portions of the Software.                                                            #
#                                                                                                  #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING    #
# BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND       #
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,     #
# DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,   #
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.          #
####################################################################################################
# Always check for the latest version at https://github.com/julian-urbano/sigir2024-rbo            #
#                                                                                                  #
# If you use this software, please cite the following paper:                                       #
#                                                                                                  #
# M. Corsi and J. Urbano, "The Treatment of Ties in Rank-Biased Overlap", International ACM SIGIR  #
# Conference on Research and Development in Information Retrieval, 2024.                           #
####################################################################################################

import re
import numpy as np

# Utility functions to create rankings #############################################################

def from_string(s):
  """
  Create a ranking from a string representation.
  
  Item names must use alphanumeric characters only. Items must be separated by a blank space, and
  tie groups must be enclosed in parentheses. For example: "red (blue green) yellow pink".
  """
  if not isinstance(s, str) or not re.match(r"^(\w+|\(\w+( \w+)+\))( (\w+|\(\w+( \w+)+\)))*$", s):
    raise ValueError("'s' must be a string representation of a ranking using alphanumeric characters")
  
  # separate letters, but retain ( and ) for groups
  g = re.split(' +', s)

  r = []  # to build up the output vector
  in_group = False  # to keep track of whether we're in a group or not

  for gg in g:
    if gg.startswith('(') or gg.endswith(')'):
      if in_group:
        # end of group
        gg = gg[:-1]
        r[-1].add(gg)
        in_group = False
      else:
        # beginning of group
        gg = gg[1:]
        r.append({gg})
        in_group = True
    else:
      if in_group:
        # tied, inner
        r[-1].add(gg)
      else:
        # untied
        r.append(gg)
  
  return r

def to_string(r):  
  """
  Create a string representation from a ranking.
  """
  return ' '.join([item if isinstance(item, str) else '('+' '.join(item)+')' for item in r]).strip()

def extract_ranking(items, scores):
  """
  Create a ranking of `items`, sorting by `scores` in descending order. Items with the same score
  will be grouped in a tie.
  This is useful for instance to extract the ranking of documents by their retrieval value.
  """
  # Sort by score first
  items = np.array(items)
  scores = np.array(scores)
  o = np.argsort(-scores)
  items = items[o]
  scores = scores[o]
  
  r = [items[0]] 
  prev = scores[0]  # Keep track of the previous score
  i = 1

  while i < len(scores):
    if scores[i] == prev:
      if isinstance(r[-1], str):
        r[-1] = {r[-1]}
      r[-1].add(items[i])
    else:
      r.append(items[i]) # new group
    prev = scores[i]
    i += 1

  return r

# RBO ##############################################################################################

def flatten(x, y = None):
  """
  Flatten rankings (with possible ties), returning:
    - `id`: flattened ranking.
    - `t`: top ranks.
    - `b`: bottom ranks.
  If both `x` and `y` are given, it returns a tuple identifying the Longer and Shorter rankings.
  """
  if y is not None:
    x = flatten(x)
    y = flatten(y)

    if len(x['id']) >= len(y['id']):
      return (y, x)
    else:
      return (x, y)
  else:
    ids = []
    t = []
    b = []

    r = -1  # last rank
    index = 0
    for g in x:
      if isinstance(g, str):
        ids.append(g)
        t.append(r+1)
        b.append(r+1)
        r += 1
      else:
        for gg in g:
          ids.append(gg)
          t.append(r+1)
          b.append(r+len(g))
        r += len(g)
        
  return {'id': np.array(ids), 't': np.array(t), 'b': np.array(b)}

def cc_w(S, L):
  """
  Compute w-variant item contributions of the flattened rankings S and L. Also returns the set
  `omega` with all items, used to index the rows of the contribution matrices.
  """
  omega = np.concatenate((S['id'], L['id'][~np.isin(L['id'], S['id'])])) # union of domains, keeping order
  return (cc_w_(S, omega, len(L['id'])), cc_w_(L, omega, len(L['id'])), omega)
def cc_w_(f, omega, max_d):   
  m = np.zeros((len(omega), max_d))
  for i in range(len(f['id'])):
    row = np.where(omega == f['id'][i])
    cols = np.arange(f['t'][i], max_d)
    m[row, cols] = 1
  return m

def cc_a(S, L):
  """
  Compute a/b-variant item contributions of the flattened rankings S and L. Also returns the set
  `omega` with all items, used to index the rows of the contribution matrices.
  """
  omega = np.concatenate((S['id'], L['id'][~np.isin(L['id'], S['id'])])) # union of domains, keeping order
  return (cc_a_(S, omega, len(L['id'])), cc_a_(L, omega, len(L['id'])), omega)
def cc_a_(f, omega, max_d):
  m = np.zeros((len(omega), max_d))
  for i in range(len(f['id'])):
    row = np.where(omega == f['id'][i])
    cols = np.arange(f['b'][i], max_d)
    m[row, cols] = 1
    cols = np.arange(f['t'][i], f['b'][i])
    m[row, cols] = np.arange(1, f['b'][i]-f['t'][i]+1) / (f['b'][i]-f['t'][i]+1)
  return m

def rbo(x, y, p, ties = 'a', score = ('ext', 'min', 'max', 'res')):
  """
  Compute RBO between `x` and `y`, with persistence parameter `p`, and handling ties when present.
  
  The rankings should be represented as a list of items, where ties are identified by a set, e.g.:
    ["red", {"blue", "green"}, "yellow", "pink"]
  Utility functions `from_string`, `to_string` and `extract_ranking` can ease the process of
  creating valid rankings from string representations or raw ranking data.
  
  Parameters
  ----------
  ties: 'a' (default), 'b' or 'w'
    Please see section 1.1 of the SIGIR 2024 paper to decide which one should be used. In summary:
      - When ties represent equality (i.e. sports rankings), use 'w'.
      - When ties represent uncertainty (i.e. don't know which items go first):
        - Use 'a' to compute the average RBO across permutations of the ties.
        - Use 'b' if the RBO score should be corrected by the information lost due to ties.
        
  score: tuple with any of 'ext', 'min', 'max' and 'res' (all by default)
    - 'ext': extrapolate the agreement in the seen part to the unseen part.
    - 'min': lower bound by assuming no additional overlap in the unseen part.
    - 'max': upper bound by assuming maximum overlap in the unseen part.
    - 'res': size of the residual, i.e. 'max'-'min'.
  """
  if not (0 < p < 1):
    raise ValueError("'p' must be between 0 and 1.")
  if not ties in ('w', 'a', 'b'):
    raise ValueError("'ties' must be one of 'w', 'a' or 'b'.")
  if not np.all(np.isin(score, ('ext', 'min', 'max', 'res'))):
    raise ValueError("'score' must be one or more of 'ext', 'min', 'max' or 'res'.")
  
  # Flat representations
  S, L = flatten(x, y)
  s = len(S['id'])
  l = len(L['id'])
  
  # If there are no ties, use w-variant for efficiency
  if(np.all(S['t']==S['b']) and np.all(L['t']==L['b'])):
    ties = 'w'
    
  # Calculate individual item contributions
  if ties == 'w':
    cS, cL, omega = cc_w(S, L)
  else:
    cS, cL, omega = cc_a(S, L)
  
  # First section: 1 to s
  d = np.arange(0, s)
  X1 = np.sum(cS[:, d] * cL[:, d], axis=0)
  if 'w' in ties:
    A1 = 2 * X1 / (np.sum(cS[:, d], axis=0) + np.sum(cL[:, d], axis=0))
  elif 'a' in ties:
    A1 = X1 / (d+1)
  else:
    A1 = X1 / np.sqrt(np.sum(cS[:, d] ** 2, axis=0)) / np.sqrt(np.sum(cL[:, d] ** 2, axis=0))

  # Second section: s+1 to l
  d = np.arange(s, l)
  X2_seen = np.sum(cS[:, d] * cL[:, d], axis=0)
  
  if 'min' in score or 'res' in score:
    X2_min = X2_seen
    if 'w' in ties:
      A2_min = 2 * X2_min / (d+1 + np.sum(cL[:, d], axis=0))
    elif 'a' in ties:
      A2_min = X2_min / (d+1)
    else:
      A2_min = X2_min / np.sqrt(d+1) / np.sqrt(np.sum(cL[:, d] ** 2, axis=0))

  if 'max' in score or 'res' in score:
    LdS = L['id'][~np.isin(L['id'], S['id'])] # uniques in L, keeping order
    cL_unseen_max = cL[np.isin(omega, LdS[d-s]),:][:,d]
    cL_unseen_max[np.tril_indices_from(cL_unseen_max, k=-1)] = np.nan
    X2_unseen_max = np.nansum(cL_unseen_max, axis=0)
    X2_max = X2_seen + X2_unseen_max
    if 'w' in ties:
      A2_max = 2 * X2_max / (d+1 + np.sum(cL[:, d], axis=0))
    elif 'a' in ties:
      A2_max = X2_max / (d+1)
    else:
      A2_max = X2_max / np.sqrt(d+1) / np.sqrt(np.sum(cL[:, d] ** 2, axis=0))
     
  if 'ext' in score:
    LdS = L['id'][~np.in1d(L['id'], S['id'])] # uniques in L, keeping order
    cL_unseen = cL[np.isin(omega, LdS),:][:,d]
    cL_unseen[cL_unseen == 0] = np.nan
    cL_unseen = np.nanmean(cL_unseen, axis=0)
    X2_unseen_ext = (d-s+1) * A1[s-1] * cL_unseen

    X2_ext = X2_seen + X2_unseen_ext
    if 'w' in ties:
      A2_ext = 2 * X2_ext / (d+1 + np.sum(cL[:, d], axis=0))
    elif 'a' in ties:
      A2_ext = X2_ext / (d+1)
    else:
      A2_ext = X2_ext / np.sqrt(d+1) / np.sqrt(np.sum(cL[:, d] ** 2, axis=0))

  # Third section: l+1 to inf
  X_seen = np.concatenate([X1, X2_seen])
  Xl = X_seen[l - 1]  # X_l
  if 'min' in score or 'res' in score:
    d = np.arange(1, l + 1)
    sec3_min = Xl * (np.log(1 / (1 - p)) - np.sum(p ** np.array(d) / np.array(d)))
  if 'max' in score or 'res' in score:
    f = l + s - Xl
    d = np.arange(l + 1, f + 1)
    sec3_max = np.sum((2 * np.array(d) - l - s + Xl) / np.array(d) * p ** np.array(d)) + p ** (f + 1) / (1 - p)
  if 'ext' in score:
    sec3_ext = (Xl + (l - s) * A1[s - 1]) / l * p ** (l + 1) / (1 - p)

  # All sections
  d = np.arange(1, l + 1)
  if 'min' in score or 'res' in score:
    rbo_min = (1 - p) / p * (np.sum(np.concatenate([A1, A2_min]) * p ** np.array(d)) + sec3_min)
  if 'max' in score or 'res' in score:
    rbo_max = (1 - p) / p * (np.sum(np.concatenate([A1, A2_max]) * p ** np.array(d)) + sec3_max)
  if 'ext' in score:
    rbo_ext = (1 - p) / p * (np.sum(np.concatenate([A1, A2_ext]) * p ** np.array(d)) + sec3_ext)
  
  # Output
  r = {}
  if 'ext' in score:
    r['ext'] = rbo_ext
  if 'min' in score:
    r['min'] = rbo_min
  if 'max' in score:
    r['max'] = rbo_max
  if 'res' in score:
    r['res'] = rbo_max - rbo_min
  
  return r
