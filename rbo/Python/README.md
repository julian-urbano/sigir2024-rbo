# Rank-Biased Overlap implementation in Python

File `rbo.py` contains a standalone implementation of the Rank-Biased Overlap rank similarity measure. Specifically, it can calculate all relevant scores ($RBO_{EXT}$, $RBO_{MIN}$, $RBO_{MAX}$ and $RBO_{RES}$) in all three tie-aware variants ($RBO^w$, $RBO^a$ and $RBO^b$), as defined in the following paper:

- M. Corsi and J. Urbano, "[The Treatment of Ties in Rank-Biased Overlap](http://julian-urbano.info/files/publications/068-treatment-ties-rank-biased-overlap.pdf)", *International ACM SIGIR Conference on Research and Development in Information Retrieval*, 2024.

If you use this software, please cite this paper.

## How to create rankings

Items are represented with strings, using only alphanumeric characters. A ranking is a list of items. Items that are tied are simply grouped under the same set. For example:

```python
x = ['red', {'blue', 'green'}, 'yellow', 'pink']
y = [{'blue', 'red'}, 'white', {'yellow', 'black', 'purple'}, 'green']
```

As this can become tedious, we provide three helper functions to create rankings:

- `from_string` allows you to create a ranking from a plain text representation. For example, ranking `x` above can be created as follows:

  ```python
  >>> from_string('red (blue green) yellow pink')
  # ['red', {'blue', 'green'}, 'yellow', 'pink']
  ```
  
- `to_string` is the inverse operation; it creates a plain text representation from a ranking:

  ```python
  >>> to_string(x)
  # 'red (blue green) yellow pink'
  ```
  
- `extract_ranking` creates a ranking given a list of `items` and their `scores`, sorting in descending order:

  ```python
  >>> extract_ranking(['green', 'yellow', 'blue', 'pink', 'red'],
                      [4, 2, 4, 1, 6])
  # ['red', {'blue', 'green'}, 'yellow', 'pink']
  ```

  This is useful for example to extract the rankings as represented in a retrieval run.

## How to compute RBO

Given two rankings `x` and `y`, RBO can be easily computed with a `p` of your choice:

```python
>>> rbo(x, y, p = .95)
# {'ext': 0.6922853, 'min': 0.3310519, 'max': 0.8930692, 'res': 0.5620173}
```

All four scores will be computed by default. If only some of them are necessary, you can specify which through argument `score`:

```python
>>> rbo(x, y, p = .95, score = ('ext', 'res'))
# {'ext': 0.6922853, 'res': 0.5620173}
```

The $RBO^a$ variant is computed by default, but this can be changed through argument `ties`:

```python
>>> rbo(x, y, p = .95, ties = 'w')
# {'ext': 0.7068257, 'min': 0.3429684, 'max': 0.9049857, 'res': 0.5620173}

>>> rbo(x, y, p = .95, ties = 'b')
# {'ext': 0.7207131, 'min': 0.3509163, 'max': 0.9129336, 'res': 0.5620173}
```

## What RBO variant should you use?

- When a tie represents equality, so that tied items *really* occur at the same rank, you should compute $RBO^w$.
- When a tie represents uncertainty, so that it is not known which item appears first:

  - Ties should not be broken deterministically, such as by doc ID, because it inflates $RBO$ scores.
  - Ties should not be broken at random because it introduces noise. $RBO^a$ should be used instead, as it precisely computes the expected $RBO$ when breaking ties at random.
  - If the measured overlap should be corrected by the amount of information lost due to ties, $RBO^b$ should be used. This ensures $RBO^b(X,X)=1$, and implies $RBO^a\leq RBO^b$.

For more details, please refer to Section 1.1 of the paper.
